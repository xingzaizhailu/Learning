## Services



### About Services

In a distributed application, different pieces of the app are called “services.” Services are really just “containers in production.” A service only runs one image, but it codifies the way that image runs—what ports it should use, how many replicas of the container should run so the service has the capacity it needs, and so on. Scaling a service changes the number of container instances running that piece of software, assigning more computing resources to the service in the process.

### Using `docker service`

``` shell
$ docker service create alpine # create a service with a instance of alpine image
$ docker service ls
$ docker service ps name

$ docker service update <ID> —replicas 3    # scale up
$ docker service ls
$ docker service ps <service_name>
$ docker container ls

$ docker container rm -f <name>.1.<ID>      # new instance will be automatically launched after a instance fail
$ docker service ls # multiple times, you will see new instance automatically being created
$ docker service ps <service_name>					# will be able to see the history

$ docker service rm service_name
$ docker container ls
```

### Using `docker-compose.yml` file
A `docker-compose.yml` file is a YAML file that defines how Docker containers should behave in
production.

#### `docker-compose.yml`
Save this file as `docker-compose.yml` wherever you want. Be sure you have pushed the image you created in Part 2 to a registry, and update this .yml by replacing username/repo:tag with your image details.

```yaml
version: "3"
services:
  web:
    # replace username/repo:tag with your name and image details
    image: username/repo:tag
    deploy:
      replicas: 5
      resources:
        limits:
          cpus: "0.1"
          memory: 50M
      restart_policy:
        condition: on-failure
    ports:
      - "80:80"
    networks:
      - webnet
networks:
  webnet:
```

This `docker-compose.yml` file tells Docker to do the following:
- Pull the image we uploaded in step 2 from the registry.
- Run 5 instances of that image as a service called web, limiting each one to use, at most, 10% of the CPU (across all cores), and 50MB of RAM.
- Immediately restart containers if one fails.
- Map port 80 on the host to web’s port 80.
- Instruct web’s containers to share port 80 via a load-balanced network called webnet. (Internally, the containers themselves will publish to web’s port 80 at an ephemeral port.)
- Define the webnet network with the default settings (which is a load-balanced overlay network).

### Run your new load-balanced app
Before we can use the `docker stack deploy` command we’ll first run:

``` sh
$ docker swarm init
```

Note: We’ll get into the meaning of that command in part 4. If you don’t run docker swarm init you’ll get an error that “this node is not a swarm manager.” Now let’s run it. You have to give your app a name. Here, it is set to getstartedlab:  

```sh
$ docker stack deploy -c docker-compose.yml getstartedlab
```

Our single service stack is running 5 container instances of our deployed image on one host. Let’s investigate.  

Get the service ID for the one service in our application:

```sh
$ docker service ls
```

A single container running in a service is called a **task**. Tasks are given unique IDs that numerically increment, up to the number of replicas you defined in docker-compose.yml. List the tasks for your service:

```sh
$ docker service ps getstartedlab_web
```

Tasks also show up if you just list all the containers on your system, though that will not be filtered by service:  

```sh
$ docker container ls -q
```

### Scale the app

You can scale the app by changing the replicas value in docker-compose.yml, saving the change, and
re-running the docker stack deploy command:

``` shell
$ docker stack deploy -c docker-compose.yml getstartedlab
```

Docker will do an in-place update, no need to tear the stack down first or kill any containers.

Now, re-run docker `container ls -q` to see the deployed instances reconfigured. If you scaled up the replicas, more tasks, and hence, more containers, are started.

#### Take down the app and  the swarm

Take the app down with docker stack rm:

``` shell
$ docker stack rm getstartedlab
```

Take down the swarm.

``` shell
$ docker swarm leave --force
```

#### Wrapping up
To recap, while typing docker run is simple enough, the true implementation of a container in production is running it as a service. Services codify a container’s behavior in a Compose file, and this file can be used to scale, limit, and redeploy our app. Changes to the service can be applied in place, as it runs, using the same command that launched the service: `docker stack deploy`.  

##### Commands use this part
``` shell
$ docker stack ls                                            # List stacks or apps
$ docker stack deploy -c <composefile> <appname>  # Run the specified Compose file
$ docker service ls                 # List running services associated with an app
$ docker service ps <service>                  # List tasks associated with an app
$ docker inspect <task or container>                   # Inspect task or container
$ docker container ls -q                                      # List container IDs
$ docker stack rm <appname>                             # Tear down an application
$ docker swarm leave --force      # Take down a single node swarm from the manager
```
