## Resources

- [Get Sarted](https://docs.docker.com/get-started)
- [Best practices](https://docs.docker.com/develop/dev-best-practices/)
- [Slimming Down Your Docker Images](https://towardsdatascience.com/slimming-down-your-docker-images-275f0ca9337e)
- [Volumes: Data in Docker](https://towardsdatascience.com/pump-up-the-volumes-data-in-docker-a21950a8cd8)

## Docker Image

- An image includes everything needed to run an application:
  - The code or binary
  - Metadata about the image
  - Runtimes
  - Dependencies
  - Any other filesystem objects required.
    - One of the most important aspects of container **isolation** is that each container interacts with its own private filesystem
- Image is
  - no a compelete OS
  - no kernel, kernel modules (e.g. drivers)
- Image can be
  - small as one file like a golang static binary
  - or big as a Ubuntu distro with apt, Apache, PHP and more

### Image layers

- Images are made up of file system changes and metadata.
- Each layer is uniquely identified and only stored once on a host.
- This saves storage space on host and transfer time on push/pull.
- A container is just a single read/write layer on top of image.

##### Some commands

- Show layers of the image: `docker image history nginx:latest`
  - Images are read only.
  - **Copy on write:**  Copy the file need to be modified out to the top layer and save the modified file there.
- Show all details (metadata) of the image and how it is made: `docker image inspect nginx`

## Containers

### Dockerfile

The Dockerfile tells Docker how to build the image that will be used to make containers. Each Docker image contains a file named *Dockerfile* with no extension. The Dockerfile is assumed to be in the current working directory when `docker build` is called to create an image. A different location can be specified with the file flag (`-f`).

Recall that a container is built from a series of layers. Each layer is read only ([copy-on-write](https://docs.docker.com/storage/storagedriver/#the-copy-on-write-cow-strategy)), except the final container layer that sits on top of the others. The Dockerfile tells Docker which layers to add and in which order to add them.

Each layer is really just a file with the changes since the previous layer. In Unix, pretty much everything is a [file](https://en.wikipedia.org/wiki/Everything_is_a_file).

### Dockerfile instructions

**NOTE:** The order of commands matters. Put those changes less on top. Because all commands after the modified one will have to rerun.

Comments start with `#`.

- `FROM` — specifies the base (parent) image - provides the initial layer(s).
- `LABEL` —provides metadata. Good place to include maintainer info.
  
  - LABEL maintainer="wutexuanleo@outlook.com"
  
- `ENV` — sets a persistent environment variable.
- `RUN` —runs a command and creates an image layer. Used to install packages into containers.

  - `requirements.txt`

  - ```dockerfile
    # forward your log and error logs to docker log collector
    RUN ln -sf /dev/stdout /path/to/your/log.log \
    		&& ln -sf /dev/stderr /path/to/your/error.log
    ```
- `COPY` — copies files and directories to the container. (Recommended whenever possible)
- `ADD` — copies files and directories to the container. It has two more use cases than `COPY`
  - Can be used to move files from a remote URL to a container.
  - Can unpack local TAR files.
- `CMD` — provides a command and arguments for an executing container. Parameters can be overridden. 
  - _There can be only one CMD_. Other wise all but the final one are ignored.
  - CMD can include an executable. If CMD is present without an executable, then an ENTRYPOINT instruction must exist. In that case, both CMD and ENTRYPOINT instructions should be in JSON format.
  - Command line arguments to `docker run` override arguments provided to CMD in the Dockerfile.
- `WORKDIR` — sets the working directory for the instructions that follow.
- `ARG` — defines a variable to pass to Docker at build-time.
  - Not available in running containers.
  - You can use ARG to set an ENV at build time
- `ENTRYPOINT` — provides command and arguments for an executing container. Arguments persist.
  - Similar to `CMD`, but it's parameters are not overwritten if a container is run with command line parameters. Instead, it will append to the `ENTRYPOINT` instructions's arguments.
  - e.g. `docker run my_imange bash` adds `bash` to the end of the `ENTRYPOINT`'s existing arguments'.
- `EXPOSE` — exposes a port.
  - EXPOSE does not actually publish the port. Rather, it acts as a documentation between the person who builds the image and the person who runs the container.
  - Use `docker run` with the `-p` flag to publish and map one or more ports at run time. The uppercase `-P` flag will publish all exposed ports.
- `VOLUME` — creates a directory mount point to access and store persistent data.

Only the instructions FROM, RUN, COPY, and ADD create layers in the final image. Other instructions configure things, add metadata, or tell Docker to do something at run time, such as expose a port or run a command.

The [Docker docs](https://docs.docker.com/v17.09/engine/reference/builder/#understand-how-cmd-and-entrypoint-interact) have a few suggestions for choosing between CMD and ENTRYPOINT for your initial container command:

- Favor ENTRYPOINT when you need to run the same command every time.
- Favor ENTRYPOINT when a container will be used as an executable program.
- Favor CMD when you need to provide extra default arguments that could be overwritten from the command line.

### Define a container with a `Dockerfile`

Create an empty directory, then create a file called `Dockerfile`, copy-and-paste
the following content into that file, and save it.

```dockerfile
# Use an official Python runtime as a parent image
FROM python:2.7-slim

# Set the working directory to /app
# All subsequent actions are taken from this dir in your image filesystem.
WORKDIR /app

# Copy the current directory contents into the container at WORKDIR/ present location, ie. `/app`
ADD . .

# Install any needed packages specified in requirements.txt
RUN pip install --trusted-host pypi.python.org -r requirements.txt

# Make port 80 available to the world outside this container
EXPOSE 80

# Define environment variable
ENV NAME World

# Run app.py when the container launches
CMD ["python", "app.py"]
```
If you are behind a proxy server, add the following lines to your Dockerfile,
using the ENV command to specify the host and port for your proxy servers:  

``` dockerfile
# Set proxy server, replace host:port with values for your servers
ENV http_proxy host:port
ENV https_proxy host:port
```
This Dockerfile refers to a couple of files we haven’t created yet,
namely app.py and requirements.txt.   

### The app itself
Create two more files, requirements.txt and app.py, and put them in the same folder with the Dockerfile. This completes our app. When the above Dockerfile is built into an image,
app.py and requirements.txt will be present because of that Dockerfile’s ADD command,
and the output from app.py will be accessible over HTTP thanks to the EXPOSE command.  

#### requirements.txt
```
Flask
Redis
```
#### app.py
``` python
from flask import Flask
from redis import Redis, RedisError
import os
import socket

# Connect to Redis
redis = Redis(host="redis", db=0, socket_connect_timeout=2, socket_timeout=2)

app = Flask(__name__)

@app.route("/")
def hello():
  try: 
    visits = redis.incr("counter")
    except RedisError:
      visits = "<i>cannot connect to Redis, counter disabled</i>"

      html = "<h3>Hello {name}!</h3>" \
      "<b>Hostname:</b> {hostname}<br/>" \
      "<b>Visits:</b> {visits}"
      return html.format(name=os.getenv("NAME", "world"), hostname=socket.gethostname(), visits=visits)

    if __name__ == "__main__":
      app.run(host='0.0.0.0', port=80)
```

### Build the app
We are ready to build the app. Make sure you are still at the top level of your new directory.
Here’s what ls should show:

``` shell
$ ls
Dockerfile    app.py      requirements.txt
```
Now run the build command. This creates a Docker image, which we’re going to tag using -t so it has
a friendly name.  

```sh
$ docker build -t friendlyhello .
```

Where is your built image? It’s in your machine’s local Docker image registry:  

``` shell
$ docker images

REPOSITORY            TAG                 IMAGE ID
friendlyhello         latest              326387cea398
```
Tip: You can use the commands `docker images` or the newer `docker image ls` list images.  

### Run the app
Run the app, mapping your machine’s port 4000 to the container’s published port 80 using -p:  

```sh
$ docker run -p 4000:80 friendlyhello
# or, run in the background
$ docker run -d -p 4000:80 friendlyhello
```
You should see a message that Python is serving your app at http://0.0.0.0:80. But that message is
coming from inside the container, which doesn’t know you mapped port 80 of that container to 4000,
making the correct URL http://localhost:4000.  

To find the IP address, use the command docker-machine ip.
You can also use the curl command in a shell to view the same content.

``` shell
$ curl http://localhost:4000
<h3>Hello World!</h3><b>Hostname:</b> 8fc990912a14<br/><b>Visits:</b> <i>cannot connect to Redis, counter disabled</i>
```
You can also see the abbreviated container ID with `docker container ls`:

``` shell
$ docker container ls
CONTAINER ID        IMAGE               COMMAND             CREATED
1fa4ab2cf395        friendlyhello       "python app.py"     28 seconds ago
```
You’ll see that CONTAINER ID matches what’s on http://localhost:4000.  
Now use docker container stop to end the process, using the CONTAINER ID, like so:

``` shell
$ docker container stop 1fa4ab2cf395
```

### Share your image
#### Log in with your Docker ID
Sign up for one at [cloud.docker.com]().

```shell
$ docker login [Service]
# To see auth info
$ cat ~/.docker/config.json
```
Remember to `docker logout` on machines you don't trust.

#### Tag the image

The notation for associating a local image with a repository on a registry is
`username/repository:tag`. Now, put it all together to tag the image. Run docker tag image
with your username, repository, and tag names so that the image will upload to your desired destination.   

```sh
$ docker tag image username/repository:tag
# For example: 
$ docker tag friendlyhello john/get-started:part2
```
#### Publish the image
```sh
$ docker push username/repository:tag
```
#### Pull and run the image from the remote repository
From now on, you can use docker run and run your app on any machine with this command:

```sh
$ docker run -p 4000:80 username/repository:tag
```
If the image isn’t available locally on the machine, Docker will pull it from the repository.

```sh
$ docker run -p 4000:80 john/get-started:part2
```
Note: If you don’t specify the :tag portion of these commands, the tag of :latest will be assumed,
both when you build and when you run images. Docker will use the last version of the image that
ran without a tag specified (not necessarily the most recent image).  

### Commands of this part
```sh
docker build -t friendlyname .  # Create image using this directory's Dockerfile
docker run -p 4000:80 friendlyname  # Run "friendlyname" mapping port 4000 to 80
docker run -d -p 4000:80 friendlyname         # Same thing, but in detached mode
docker container ls                                # List all running containers
docker container ls -a             # List all containers, even those not running
docker container stop <hash>           # Gracefully stop the specified container
docker container kill <hash>         # Force shutdown of the specified container
docker container rm <hash>        # Remove specified container from this machine
docker container rm $(docker container ls -a -q)         # Remove all containers
docker image ls -a                             # List all images on this machine
docker image rm <image id>            # Remove specified image from this machine
docker image rm $(docker image ls -a -q)   # Remove all images from this machine
docker login             # Log in this CLI session using your Docker credentials
docker tag <image> username/repository:tag  # Tag <image> for upload to registry
docker push username/repository:tag            # Upload tagged image to registry
docker run username/repository:tag                   # Run image from a registry
```

### Inside Containers

Look from outside:

- `docker container top` - process list on one container
- `docker container inspect` - details of one container config
- `docker container stats` - live performance stats for all containers

Get into the containers:

(No SSH needed, docker cli is great substitute for adding SSH to containers)

- `docker container run -it` - start new container interactively
  - -i: interactive
  - -t: pseudo-TTY
  - e.g. `docker container run -it --name mynginx nginx bash`
  - restart: `docker container start -ai mynginx`
- `docker container exec -it <container_name> <CMD>` - run additional command in existing container
  - `docker container exec -it mysql bash`
- different Linux distros in containers

*Note:* Alpine is so small to have bash, you can use `sh` instead.