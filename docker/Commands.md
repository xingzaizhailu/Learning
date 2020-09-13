## [Docker Commands](https://towardsdatascience.com/15-docker-commands-you-should-know-970ea5203421)

### Containers

`docker container my_command`:

- `Create` - Create a container from an image

- `start` - Start an _existing_ container.

- `run` - Create a new container and start it.

  - ```shell
    $ docker container run -i -t -p 1000:8000 --rm my_image
    ```

  - `-i` is short for `--interactive`. Keep STDIN open even if unattached.

  - `-t`is short for`--tty`. Allocates a pseudo [terminal](http://en.wikipedia.org/wiki/Pseudo_terminal) that connects your terminal with the container’s STDIN and STDOUT.

  - You need to specify both `-i` and `-t` to then interact with the container through your terminal shell. (You can combine them as `-it`).

  - `--rm` Automatically delete the container when it stops running.

- `ls` - List _running_ containers

  - `-s` short for `--size`: list the size for each container

- `inspect` - See lots of info about a container.

- `logs` - Print logs.

- `stop` - Gracefully stop running container.

  - 10s before container shutdown to finish any processes by default.

- `kill` - Stop main process in container abruptly.

  - `docker container kill $(docker ps -q)`: kill all running containers

- `rm` - Delete a stopped container.

  - `docker container rm $(docker ps -a -q)`: delete all containers that are not running

### Images

`docker image my_command`:

- `build `— Build an image.
  - `-t`: short for `--tag`
- `push` — Push an image to a remote registry.
- `ls` — List images.
- `history` — See intermediate image info. Display an image’s intermediate images with sizes and how they were created.
- `inspect` — See lots of info about an image, including the layers.
- `rm` — Delete an image.
  - `docker image rm $(docker images -a -q)` — Delete all images.

### Misc

- `docker version` — List info about your Docker Client and Server versions.

- `docker login `— Log in to a Docker registry.

- `docker system prune` — Delete all unused containers, unused networks, and dangling images

  - `docker system prune -a --volumes`
    - `-a` is short for `--all`. Delete unused images, [not just dangling ones](https://stackoverflow.com/a/45143234/4590385).
    - `--volumes` Remove unused volumes. We’ll talk more about volumes in the next article.

### [Use Prune to keep docker system clean](https://youtu.be/_4QzP7uwtvI)

Use `prune` to clean up images, volumes, build cache and containers.

- `docker image prune` - clean up "dangling" images
  - `docker image prune -a` - remove all images not using
  - Use `docker system df` to see space usage
- `docker system prune` - clean up everything