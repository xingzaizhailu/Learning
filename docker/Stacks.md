## Stacks

A stack is a group of interrelated services that share dependencies, and can be orchestrated and scaled together. A single stack is capable of defining and coordinating the functionality of an entire application, including services, volumes, networks (though very complex applications may want to use multiple stacks).

Here, you will take what you’ve learned, make multiple services relate to each other, and run them on multiple machines.

### Add a new service and redeploy

It’s easy to add services to our `docker-compose.yml` file. First, let’s add a free visualizer service that lets us look at how our swarm is scheduling containers.  

1. Open up `docker-compose.yml` in an editor and replace its contents with the following. Be sure to replace `username/repo:tag` with your image details.  (Note: new `deploy:` key in compose file. Compose ignores `deploy:` and swarm ignores `build:`)

``` yaml
version: "3"
services:
web:
  # replace username/repo:tag with your name and image details
  image: username/repo:tag
  deploy:
    replicas: 5
    restart_policy:
      condition: on-failure
    resources:
      limits:
        cpus: "0.1"
        memory: 50M
  ports:
    - "80:80"
  networks:
    - webnet
visualizer:
  image: dockersamples/visualizer:stable
  ports:
    - "8080:8080"
  volumes:
    - "/var/run/docker.sock:/var/run/docker.sock"
  deploy:
    placement:
      constraints: [node.role == manager]
  networks:
    - webnet
networks:
  webnet:
```

The only thing new here is the peer service to web, named visualizer. You’ll see two new things here:

- a volumes key, giving the visualizer access to the host’s socket file for Docker, and
- a placement key, ensuring that this service only ever runs on a swarm manager – never a worker. That’s because this container, built from an open source project created by Docker, displays Docker services running on a swarm in a diagram.

2. Make sure your shell is configured to talk to manager node

3. Re-run the docker stack deploy command on the manager, and whatever services need updating will be updated:

``` sh
$ docker stack deploy -c docker-compose.yml getstartedlab
Updating service getstartedlab_web (id: angi1bf5e4to03qu9f93trnxm)
Creating service getstartedlab_visualizer (id: l9mnwkeq2jiononb5ihz9u7a4)
```

4. Take a look at the visualizer. You saw in the Compose file that visualizer runs on port 8080. Get the IP address of one of your nodes by running `docker-machine ls`. Go to either IP address at port 8080 and you will see the visualizer running.

The single copy of visualizer is running on the manager as you expect, and the 5 instances of web are spread out across the swarm. You can corroborate this visualization by running `docker stack ps <stack>`:

``` sh
$ docker stack ps getstartedlab
$ docker stack services getstartedlab
```

The visualizer is a standalone service that can run in any app that includes it in the stack. It doesn’t depend on anything else. Now let’s create a service that does have a dependency: the Redis service that will provide a visitor counter.  

### Persist the data

1. Save this new docker-compose.yml file, which finally adds a Redis service. Be sure to replace `username/repo:tag` with your image details.

```
version: "3"
services:
  web:
    # replace username/repo:tag with your name and image details
    image: username/repo:tag
    depends_on:
    	- redis
    deploy:
      replicas: 5
      update_config:
      	parallelism: 2
      	delay: 10s
      restart_policy:
        condition: on-failure
        max_attempts: 3
        window: 20s
        resources:
          limits:
            cpus: "0.1"
            memory: 50M
          ports:
            - "80:80"
          networks:
            - webnet
  visualizer:
    image:
      dockersamples/visualizer:stable
    ports:
      - "8080:8080"
    volumes:
      - "/var/run/docker.sock:/var/run/docker.sock"
    deploy:
      placement:
        constraints: [node.role == manager]
    networks:
      - webnet
  redis:
    image: redis
    ports:
      - "6379:6379"
    volumes:
      - /home/docker/data:/data
    deploy:
      placement:
        constraints: [node.role == manager]
    command: redis-server --appendonly yes
    networks:
      - webnet
networks:
  webnet:
```

- redis always runs on the manager, so it’s always using the same filesystem.
- redis accesses an arbitrary directory in the host’s file system as /data inside the container, which is where Redis stores data. Together, this is creating a “source of truth” in your host’s physical filesystem for the Redis data. Without this, Redis would store its data in /data inside the container’s filesystem, which would get wiped out if that container were ever redeployed.

2. Create a ./data directory on the manager:

   ```docker-machine ssh myvm1 "mkdir ./data"```

3. Make sure your shell is configured to talk to manager node

4. Run docker stack deploy one more time.

   ```$ docker stack deploy -c docker-compose.yml getstartedlab```

5. Run docker service ls to verify that the three services are running as expected.

```sh
$ docker service ls
ID            NAME                      MODE        REPLICAS  IMAGE                            PORTS
x7uij6xb4foj  getstartedlab_redis       replicated  1/1       redis:latest                     *:6379->6379/tcp
n5rvhm52ykq7  getstartedlab_visualizer  replicated  1/1       dockersamples/visualizer:stable  *:8080->8080/tcp
mifd433bti1d  getstartedlab_web         replicated  5/5       orangesnap/getstarted:latest     *:80->80/tcp
```

Check the web page at one of your nodes (e.g. http://192.168.99.101) and you’ll see the results of the visitor counter, which is now live and storing information on Redis. Also, check the visualizer at port 8080 on either node’s IP address, and you’ll see the redis service running along with the web and visualizer services.
