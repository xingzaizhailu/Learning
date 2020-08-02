A Docker image consists of *read-only* layers each of which represents a Dockerfile instruction. The layers are stacked and each one is a delta of the changes from the previous layer.

When you run an image and generate a container, you add a new *writable layer* (the “container layer”) on top of the underlying layers. All changes made to the running container, such as writing new files, modifying existing files, and deleting files, are written to this thin writable container layer.

## General guidances and recommendations

### Understand build context

When you issue a `docker build` command, the current working directory is called **build context**. By default, the `Dockerfile` is assumed to be located under cwd, but you can specify it with `-f`. Regardless of where the `Dockerfile` actually lives, all recursive contents of the files and directories in the current directory are sent to the Docker daemons as the build context.

### Pipe Dockerfile through `stdin`

Piping a `Dockerfile` through `stdin` can be useful to perform one-off builds without writing a Dockerfile to disk, or in situations where the `Dockerfile` is generated, and should not persist afterwards.

The following commands are equivalent:

```bash
echo -e 'FROM busybox\nRUN echo "hello world"' | docker build -
```

```shell
docker build -<<EOF
FROM busybox
RUN echo "hello world"
EOF
```

#### Build an image using a dockerfile from stdin, without sending build context

The hyphen (`-`) takes the position of the `PATH`, and instructs Docker to read the build context (which only contains a `Dockerfile`) from `stdin` instead of a directory:

```bash
docker build [OPTIONS] -
```

Omitting the build context can be useful in situations where your `Dockerfile` does not require files to be copied into the image, and improves the build-speed, as no files are sent to the daemon.

If you want to improve the build-speed by excluding *some* files from the build- context, refer to [exclude with .dockerignore](https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#exclude-with-dockerignore).

#### Build from a local build context, using a Dockerfile from stdin

The syntax uses the `-f` (or `--file`) option to specify the `Dockerfile` to use, using a hyphen (`-`) as filename to instruct Docker to read the `Dockerfile` from `stdin`:

```bash
docker build [OPTIONS] -f- PATH
```

This syntax can be useful in situations where you want to build an image from a repository that does not contain a `Dockerfile`, or if you want to build with a custom `Dockerfile`, without maintaining your own fork of the repository.

The example below builds an image using a `Dockerfile` from `stdin`, and adds the `hello.c` file from the [“hello-world” Git repository on GitHub](https://github.com/docker-library/hello-world).

```bash
docker build -t myimage:latest -f- https://github.com/docker-library/hello-world.git <<EOF
FROM busybox
COPY hello.c .
EOF
```

When building an image using a remote Git repository as build context, Docker performs a `git clone` of the repository on the local machine, and sends those files as build context to the daemon.

### Exclude with .dockerignore

To exclude files not relevant to the build (without restructuring your source repository) use a `.dockerignore` file. This file supports exclusion patterns similar to `.gitignore` files. For information on creating one, see the [.dockerignore file](https://docs.docker.com/engine/reference/builder/#dockerignore-file).

### Use multi-stage builds

[Multi-stage builds](https://docs.docker.com/develop/develop-images/multistage-build/) allow you to drastically reduce the size of your final image, without struggling to reduce the number of intermediate layers and files.

Because an image is built during the final stage of the build process, you can minimize image layers by [leveraging build cache](https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#leverage-build-cache).

### Don't install unnecessary packages

### Decouple applications

Each container should have only one concern. Decoupling applications into multiple containers makes it easier to scale horizontally and reuse containers.

Limiting each container to one process is a good rule of thumb, but it is not a hard and fast rule. Use your best judgment to keep containers as clean and modular as possible. If containers depend on each other, you can use [Docker container networks](https://docs.docker.com/network/) to ensure that these containers can communicate.

### Minimise the number of layers

Only the instructions `RUN`, `COPY`, `ADD` create layers. Other instructions create temporary intermediate images, and do not increase the size of the build.

Where possible, use [multi-stage builds](https://docs.docker.com/develop/develop-images/multistage-build/), and only copy the artifacts you need into the final image. This allows you to include tools and debug information in your intermediate build stages without increasing the size of the final image.

### Sort multi-line arguments

Whenever possible, ease later changes by sorting multi-line arguments alphanumerically. This helps to avoid duplication of packages and make the list much easier to update. This also makes PRs a lot easier to read and review.

```dockerfile
RUN apt-get update && apt-get install -y \
  bzr \
  cvs \
  git \
  mercurial \
  subversion
```

### Leverage build cache

When building an image, Docker steps through the instructions in your `Dockerfile`, executing each in the order specified. As each instruction is examined, Docker looks for an existing image in its cache that it can reuse, rather than creating a new (duplicate) image.

If you do not want to use the cache at all, you can use the `--no-cache=true` option on the `docker build` command. However, if you do let Docker use its cache, it is important to understand when it can, and cannot, find a matching image.

- Starting with a parent image that is already in the cache, the next instruction is compared against all child images derived from that base image to see if one of them was built using the exact same instruction. If not, the cache is invalidated.
- In most cases, simply comparing the instruction in the `Dockerfile` with one of the child images is sufficient. However, certain instructions require more examination and explanation.
- For the `ADD` and `COPY` instructions, the contents of the file(s) in the image are examined and a checksum is calculated for each file. The last-modified and last-accessed times of the file(s) are not considered in these checksums. During the cache lookup, the checksum is compared against the checksum in the existing images. If anything has changed in the file(s), such as the contents and metadata, then the cache is invalidated.
- Aside from the `ADD` and `COPY` commands, cache checking does not look at the files in the container to determine a cache match. For example, when processing a `RUN apt-get -y update` command the files updated in the container are not examined to determine if a cache hit exists. In that case just the command string itself is used to find a match.

Once the cache is invalidated, all subsequent `Dockerfile` commands generate new images and the cache is not used.

### Dockerfile instructions

#### `FROM`

Specifies the base (parent) image - provides the initial layer(s).  Whenever possible, use current official images as the basis for your images. The [Alpine image](https://hub.docker.com/_/alpine/) is recommended as it is tightly controlled and small in size (currently under 5 MB), while still being a full Linux distribution.

#### `LABEL`

Provides metadata. Good place to include maintainer info. You can add labels to your image to help organize images by project, record licensing information, to aid in automation, or for other reasons.

```dockerfile
# Set one or more individual labels
LABEL maintainer="wutexuanleo@outlook.com"
LABEL version="0.0.1"

# Or
LABEL maintainer="wutexuanleo@outlook.com" version="0.0.1"

# Or
LABEL maintainer="wutexuanleo@outlook.com" \
			version="0.0.1"
```

#### `ENV`

Sets a persistent environment variable.

To make new software easier to run, you can use `ENV` to update the `PATH` environment variable for the software your container installs. For example, `ENV PATH /usr/local/nginx/bin:$PATH` ensures that `CMD ["nginx"]` just works.

```dockerfile
ENV PG_MAJOR 9.3
ENV PG_VERSION 9.3.4
RUN curl -SL http://example.com/postgres-$PG_VERSION.tar.xz | tar -xJC /usr/src/postgress && …
ENV PATH /usr/local/postgres-$PG_MAJOR/bin:$PATH
```

#### `RUN`

Runs a command and creates an image layer. Used to install packages into containers.

Split long or complex `RUN` statements on multiple lines separated with backslashes to make your `Dockerfile` more readable, understandable, and maintainable.

##### APT-GET

Probably the most common use-case for `RUN`.

Avoid `RUN apt-get upgrade` and `dist-upgrade`, as many of the “essential” packages from the parent images cannot upgrade inside an [unprivileged container](https://docs.docker.com/engine/reference/run/#security-configuration). If a package contained in the parent image is out-of-date, contact its maintainers. If you know there is a particular package, `foo`, that needs to be updated, use `apt-get install -y foo` to update automatically.

Always combine `RUN apt-get update` with `apt-get install` in the same `RUN` statement. Using `apt-get update` alone in a `RUN` statement causes caching issues and subsequent `apt-get install` instructions fail.

In addition, when you clean up the apt cache by removing `/var/lib/apt/lists` it reduces the image size, since the apt cache is not stored in a layer. Since the `RUN` statement starts with `apt-get update`, the package cache is always refreshed prior to `apt-get install`.

> Official Debian and Ubuntu images [automatically run `apt-get clean`](https://github.com/moby/moby/blob/03e2923e42446dbb830c654d0eec323a0b4ef02a/contrib/mkimage/debootstrap#L82-L105), so explicit invocation is not required.

##### Using Pipes

Docker executes these commands using the `/bin/sh -c` interpreter, which only evaluates the exit code of the last operation in the pipe to determine success. For example,

```dockerfile
RUN wget -O - https://some.site | wc -l > /number
```

this build step succeeds and produces a new image so long as the `wc -l` command succeeds, even if the `wget` command fails.

If you want the command to fail due to an error at any stage in the pipe, prepend `set -o pipefail &&` to ensure that an unexpected error prevents the build from inadvertently succeeding. For example:

```
RUN set -o pipefail && wget -O - https://some.site | wc -l > /number
```

> Not all shells support the `-o pipefail` option.
>
> In cases such as the `dash` shell on Debian-based images, consider using the *exec* form of `RUN` to explicitly choose a shell that does support the `pipefail` option. For example:
>
> ```
> RUN ["/bin/bash", "-c", "set -o pipefail && wget -O - https://some.site | wc -l > /number"]
> ```

#### `COPY`

Copies files and directories to the container. (Recommended whenever possible). Only supports the basic copying of local files into the container.

If you have multiple `Dockerfile` steps that use different files from your context, `COPY` them individually, rather than all at once. This ensures that each step’s build cache is only invalidated (forcing the step to be re-run) if the specifically required files change.

For example:

```dockerfile
COPY requirements.txt /tmp/
RUN pip install --requirement /tmp/requirements.txt
COPY . /tmp/
```

Results in fewer cache invalidations for the `RUN` step, than if you put the `COPY . /tmp/` before it.

#### `ADD`

Copies files and directories to the container. It has two more use cases than `COPY`

- Can be used to move files from a remote URL to a container.
- Can unpack local TAR files. `ADD rootfs.tar.xz /`

Because image size matters, using `ADD` to fetch packages from remote URLs is strongly discouraged; you should use `curl` or `wget` instead. That way you can delete the files you no longer need after they’ve been extracted and you don’t have to add another layer in your image. For example, you should avoid doing things like:

```dockerfile
ADD http://example.com/big.tar.xz /usr/src/things/
RUN tar -xJf /usr/src/things/big.tar.xz -C /usr/src/things
RUN make -C /usr/src/things all
```

And instead, do something like:

```dockerfile
RUN mkdir -p /usr/src/things \
    && curl -SL http://example.com/big.tar.xz \
    | tar -xJC /usr/src/things \
    && make -C /usr/src/things all
```

#### `CMD`

Provides a command and arguments for an executing container. Parameters can be overridden. 

- _There can be only one CMD_. Other wise all but the final one are ignored.
- CMD can include an executable. `CMD` should almost always be used in the form of `CMD ["executable", "param1", "param2"…]`.
- If CMD is present without an executable, then an ENTRYPOINT instruction must exist. In that case, both CMD and ENTRYPOINT instructions should be in JSON format.
  -  `CMD` should rarely be used in the manner of `CMD ["param", "param"]` in conjunction with [`ENTRYPOINT`](https://docs.docker.com/engine/reference/builder/#entrypoint), unless you and your expected users are already quite familiar with how `ENTRYPOINT` works.
- Command line arguments to `docker run` override arguments provided to CMD in the Dockerfile.

#### `WORKDIR`

Sets the working directory for the instructions that follow.

For clarity and reliability, you should always use absolute paths for your `WORKDIR`. Also, you should use `WORKDIR` instead of proliferating instructions like `RUN cd … && do-something`, which are hard to read, troubleshoot, and maintain.

#### `ARG`

Defines a variable to pass to Docker at build-time.

- Not available in running containers.
- You can use ARG to set an ENV at build time

#### `ENTRYPOINT`

Provides command and arguments for an executing container. Arguments persist.

- Similar to `CMD`, but it's parameters are not overwritten if a container is run with command line parameters. Instead, it will append to the `ENTRYPOINT` instructions's arguments.
- e.g. `docker run my_imange bash` adds `bash` to the end of the `ENTRYPOINT`'s existing arguments'.

#### `EXPOSE`

Exposes a port.

- EXPOSE does not actually publish the port. Rather, it acts as a documentation between the person who builds the image and the person who runs the container.
- Use `docker run` with the `-p` flag to publish and map one or more ports at run time. The uppercase `-P` flag will publish all exposed ports.

#### `VOLUME`

Creates a directory mount point to access and store persistent data.

#### `USER`

If a service can run without privileges, use `USER` to change to a non-root user. Start by creating the user and group in the `Dockerfile` with something like

```dockerfile
RUN groupadd -r postgres && useradd --no-log-init -r -g postgres postgres .
```

To reduce layers and complexity, avoid switching `USER` back and forth frequently.

Only the instructions FROM, RUN, COPY, and ADD create layers in the final image. Other instructions configure things, add metadata, or tell Docker to do something at run time, such as expose a port or run a command.

#### `ONBUILD`

An `ONBUILD` command executes after the current `Dockerfile` build completes. `ONBUILD` executes in any child image derived `FROM` the current image. Think of the `ONBUILD` command as an instruction the parent `Dockerfile` gives to the child `Dockerfile`.

A Docker build executes `ONBUILD` commands before any command in a child `Dockerfile`.