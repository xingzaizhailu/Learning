## Container Lifetime

Containers are usually immutable and ephemeral.

- "Immutable Infrastructure": only re-deploy containers, never change
- Give us seprate of concepts

## Persistent Data

Two ways:

- **Volumes:** make special location outside of container UFS
- **Bind Mounts:** link container path to host path (existing directory)

### Volume

Like [mysql Dockerfile](https://github.com/docker-library/mysql/blob/776073e676697ed7a57911c94d1fc34c5d1ced37/8.0/Dockerfile#L77), it uses `VOLUME` command to specify where to store data.

```sh
$ docker run -d --name mysql -e MYSQL_ALLOW_EMPTY_PASSWORD=True mysql
# The volume can also be seen in container info (Mounts and Volume entries)
$ docker container inspect mysql
$ docker volume ls
$ docker volume inspect <volume_id>
```

Those data remains if the container is stopped/deleted.

#### Named Volume

Friendly way to assign volumes to containers (makes both volume name and it's location in host more friendly).

```sh
# the same as use `VOLUME` in Dockerfile
$ docker run -d --name mysql -e MYSQL_ALLOW_EMPTY_PASSWORD=True -v /var/lib/mysql mysql
# Named volume
$ docker run -d --name mysql -e MYSQL_ALLOW_EMPTY_PASSWORD=True -v mysql-db:/var/lib/mysql mysql
```

#### Create volume

The case you want to create a volume ahead time: specify driver.

```sh
$ docker volume create
```

##### Shell differences for path expansion

For PowerShell: use `${pwd}`

For` cmd.exe` command prompt: use `%cd%`

Linux/macOS bash, sh, zsh, and Windows Docker Toolbox Quickstart Terminal: use `$(pwd)` 

**Note:** if you have spaces in your path, you'll usually need to quote the whole path in the docker command.

### Bind Mounting

- Map a host file or directory to a container file or directory
  - Basically just two locations pointing to the same file(s)
- Again, skips UFS, and host files overwrite any in container
- Can't use in Dockerfile, mubt be at `container run`

```shell
# Full path on the left rather than name.
# mac/linux
$ ... run -v /Users/leo/stuff:/path/container
# windows
$ ... run -v //c/Users/bret/stuff:/path/container
```

