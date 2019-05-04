[tarted](https://docs.docker.com/get-started)
- Stack (on the top)
  - Services
  - Container
## Containers
### Define a container with a `Dockerfile`
Create an empty directory, then create a file called `Dockerfile`, copy-and-paste
the following content into that file, and save it.

```
    # Use an official Python runtime as a parent image
    FROM python:2.7-slim

    # Set the working directory to /app
    WORKDIR /app

    # Copy the current directory contents into the container at /app
    ADD . /app

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

``` docker
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

    docker build -t friendlyhello .

Where is your built image? It’s in your machine’s local Docker image registry:  

``` shell
    $ docker images

    REPOSITORY            TAG                 IMAGE ID
    friendlyhello         latest              326387cea398
```
Tip: You can use the commands docker images or the newer docker image ls list images.  

### Run the app
Run the app, mapping your machine’s port 4000 to the container’s published port 80 using -p:  

```
    docker run -p 4000:80 friendlyhello
    // or, run in the background
    docker run -d -p 4000:80 friendlyhello
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
You can also see the abbreviated container ID with docker container ls:

``` shell
    $ docker container ls
    CONTAINER ID        IMAGE               COMMAND             CREATED
    1fa4ab2cf395        friendlyhello       "python app.py"     28 seconds ago
```
You’ll see that CONTAINER ID matches what’s on http://localhost:4000.  
Now use docker container stop to end the process, using the CONTAINER ID, like so:

``` shell
    docker container stop 1fa4ab2cf395
```

### Share your image
#### Log in with your Docker ID
Sign up for one at [cloud.docker.com]().

    $ docker login
#### Tag the image
The notation for associating a local image with a repository on a registry is
`username/repository:tag`. Now, put it all together to tag the image. Run docker tag image
with your username, repository, and tag names so that the image will upload to your desired destination.   

```
    docker tag image username/repository:tag
    // For example: 
    docker tag friendlyhello john/get-started:part2
```
#### Publish the image
```
    docker push username/repository:tag
```
#### Pull and run the image from the remote repository
From now on, you can use docker run and run your app on any machine with this command:

    docker run -p 4000:80 username/repository:tag
If the image isn’t available locally on the machine, Docker will pull it from the repository.

    $ docker run -p 4000:80 john/get-started:part2
Note: If you don’t specify the :tag portion of these commands, the tag of :latest will be assumed,
both when you build and when you run images. Docker will use the last version of the image that
ran without a tag specified (not necessarily the most recent image).  

### Commands of this part
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

## Services
### About Services
In a distributed application, different pieces of the app are called “services.”  
Services are really just “containers in production.” A service only runs one image, but it codifies
the way that image runs—what ports it should use, how many replicas of the container should run so
the service has the capacity it needs, and so on. Scaling a service changes the number of container
instances running that piece of software, assigning more computing resources to the service in the
process.

Luckily it’s very easy to define, run, and scale services with the Docker platform – just write a
`docker-compose.yml` file.

### Your first `docker-compose.yml` file
A `docker-compose.yml` file is a YAML file that defines how Docker containers should behave in
production.

#### `docker-compose.yml`
Save this file as docker-compose.yml wherever you want. Be sure you have pushed the image you
created in Part 2 to a registry, and update this .yml by replacing username/repo:tag with your image details.

```
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
This docker-compose.yml file tells Docker to do the following:
- Pull the image we uploaded in step 2 from the registry.
- Run 5 instances of that image as a service called web, limiting each one to use, at most, 10% of the CPU (across all cores), and 50MB of RAM.
- Immediately restart containers if one fails.
- Map port 80 on the host to web’s port 80.
- Instruct web’s containers to share port 80 via a load-balanced network called webnet. (Internally, the containers themselves will publish to web’s port 80 at an ephemeral port.)
- Define the webnet network with the default settings (which is a load-balanced overlay network).

### Run your new load-balanced app
Before we can use the docker stack deploy command we’ll first run:

    docker swarm init
Note: We’ll get into the meaning of that command in part 4. If you don’t run docker swarm init
you’ll get an error that “this node is not a swarm manager.”  
Now let’s run it. You have to give your app a name. Here, it is set to getstartedlab:  

    docker stack deploy -c docker-compose.yml getstartedlab
Our single service stack is running 5 container instances of our deployed image on one host. Let’s
investigate.  

Get the service ID for the one service in our application:

    docker service ls
A single container running in a service is called a task. Tasks are given unique IDs that
numerically increment, up to the number of replicas you defined in docker-compose.yml. List the
tasks for your service:

    docker service ps getstartedlab_web
Tasks also show up if you just list all the containers on your system, though that will not be
filtered by service:  

    docker container ls -q
### Scale the app
You can scale the app by changing the replicas value in docker-compose.yml, saving the change, and
re-running the docker stack deploy command:

    docker stack deploy -c docker-compose.yml getstartedlab
Docker will do an in-place update, no need to tear the stack down first or kill any containers.

Now, re-run docker container ls -q to see the deployed instances reconfigured. If you scaled up the
replicas, more tasks, and hence, more containers, are started.
#### Take down the app and  the swarm
Take the app down with docker stack rm:

    docker stack rm getstartedlab
Take down the swarm.

    docker swarm leave --force
#### Wrapping up
To recap, while typing docker run is simple enough, the true implementation of a container in
production is running it as a service. Services codify a container’s behavior in a Compose file, and this file can be used to scale, limit, and redeploy our app. Changes to the service can be applied in place, as it runs, using the same command that launched the service: docker stack deploy.  
##### Commands use this part
    docker stack ls                                            # List stacks or apps
    docker stack deploy -c <composefile> <appname>  # Run the specified Compose file
    docker service ls                 # List running services associated with an app
    docker service ps <service>                  # List tasks associated with an app
    docker inspect <task or container>                   # Inspect task or container
    docker container ls -q                                      # List container IDs
    docker stack rm <appname>                             # Tear down an application
    docker swarm leave --force      # Take down a single node swarm from the manager
## Swarms
### Introduction
In part 3, you took an app you wrote in part 2, and defined how it should run in production by
turning it into a service, scaling it up 5x in the process.  

Here in part 4, you deploy this application onto a cluster, running it on multiple machines.
Multi-container, multi-machine applications are made possible by joining multiple machines into
a “Dockerized” cluster called a swarm.
### Understanding Swarm clusters
A swarm is a group of machines that are running Docker and joined into a cluster. After that has
happened, you continue to run the Docker commands you’re used to, but now they are executed on a
cluster by a swarm manager. The machines in a swarm can be physical or virtual. After joining a
swarm, they are referred to as nodes.  

Swarm managers can use several strategies to run containers, such as “emptiest node” – which fills
the least utilized machines with containers. Or “global”, which ensures that each machine gets
exactly one instance of the specified container. You instruct the swarm manager to use these
strategies in the Compose file, just like the one you have already been using.  

Swarm managers are the only machines in a swarm that can execute your commands, or authorize other
machines to join the swarm as workers. Workers are just there to provide capacity and do not have
the authority to tell any other machine what it can and cannot do.  
### Set up your swarm
A swarm is made up of multiple nodes, which can be either physical or virtual machines. The basic concept is simple enough: run docker swarm init to enable swarm mode and make your current machine a swarm manager, then run docker swarm join on other machines to have them join the swarm as workers.  
#### Create a cluster
First, you’ll need a hypervisor that can create virtual machines (VMs), so install Oracle VirtualBox for your machine’s OS.  

Now, create a couple of VMs using docker-machine, using the VirtualBox driver:

    docker-machine create --driver virtualbox myvm1
    docker-machine create --driver virtualbox myvm2
##### LIST THE VMS AND GET THEIR IP ADDRESSES
You now have two VMs created, named myvm1 and myvm2. 
Use this command to list the machines and get their IP addresses.  

    docker-machine ls
Here is example output from this command.  

    $ docker-machine ls
    NAME    ACTIVE   DRIVER       STATE     URL                       SWARM   DOCKER        ERRORS
    myvm1   -        virtualbox   Running   tcp://192.168.99.100:2376         v17.06.2-ce   
    myvm2   -        virtualbox   Running   tcp://192.168.99.101:2376         v17.06.2-ce   
##### INITIALIZE THE SWARM AND ADD NODES
The first machine will act as the manager, which executes management commands and authenticates
workers to join the swarm, and the second will be a worker.

You can send commands to your VMs using `docker-machine ssh`. Instruct myvm1 to become a swarm
manager with `docker swarm init` and you’ll see output like this:

```
    $ docker-machine ssh myvm1 "docker swarm init --advertise-addr <myvm1 ip>"
    Swarm initialized: current node <node ID> is now a manager.

    To add a worker to this swarm, run the following command:

      docker swarm join \
      --token <token> \
      <myvm ip>:<port>

    To add a manager to this swarm, run 'docker swarm join-token manager' and follow the instructions.
```
As you can see, the response to docker swarm init contains a pre-configured docker swarm join
command for you to run on any nodes you want to add. Copy this command, and send it to myvm2 via
docker-machine ssh to have myvm2 join your new swarm as a worker:  

    $ docker-machine ssh myvm2 "docker swarm join \
    --token <token> \
    <ip>:2377"

    This node joined a swarm as a worker.
Congratulations, you have created your first swarm!  

Run docker node ls on the manager to view the nodes in this swarm:

    $ docker-machine ssh myvm1 "docker node ls"
    ID                            HOSTNAME        STATUS        AVAILABILITY    MANAGER STATUS
    brtu9urxwfd5j0zrmkubhpkbd     myvm2           Ready         Active
    rihwohkh3ph38fhillhhb84sk *   myvm1           Ready         Active          Leader
### Deploy your app on the swarm cluster
#### Configure a `docker-machine` shell to the swarm manager
So far, you’ve been wrapping Docker commmands in `docker-machine ssh` to talk to the VMs. Another
option is to run `docker-machine env <machine>` to get and run a command that configures your
current shell to talk to the Docker daemon on the VM. This method works better for the next step
because it allows you to use your local `docker-compose.yml` file to deploy the app “remotely”
without having to copy it anywhere.  

Type `docker-machine env myvm1`, then copy-paste and run the command provided as the last line of
the output to configure your shell to talk to myvm1, the swarm manager.  

The commands to configure your shell differ depending on whether you are Mac, Linux, or Windows,
so examples of each are shown on the tabs below.

##### DOCKER MACHINE SHELL ENVIRONMENT ON MAC OR LINUX
Run docker-machine env myvm1 to get the command to configure your shell to talk to myvm1.

    $ docker-machine env myvm1
    export DOCKER_TLS_VERIFY="1"
    export DOCKER_HOST="tcp://192.168.99.100:2376"
    export DOCKER_CERT_PATH="/Users/sam/.docker/machine/machines/myvm1"
    export DOCKER_MACHINE_NAME="myvm1"
    #Run this command to configure your shell:
    #eval $(docker-machine env myvm1)
Run the given command to configure your shell to talk to myvm1.

    eval $(docker-machine env myvm1)
Run docker-machine ls to verify that myvm1 is now the active machine, as indicated by the asterisk
next to it.

    $ docker-machine ls
    NAME    ACTIVE   DRIVER       STATE     URL                        SWARM  DOCKER       ERRORS
    myvm1   *        virtualbox   Running   tcp://192.168.99.100:2376         v17.06.2-ce  
    myvm2   -        virtualbox   Running   tcp://192.168.99.101:2376         v17.06.2-ce  
##### Deploy the app on the swarm manager
Now that you have my myvm1, you can use its powers as a swarm manager to deploy your app by using
the same docker stack deploy command you used in part 3 to myvm1, and your local copy of
docker-compose.yml.

You are connected to myvm1 by means of the docker-machine shell configuration, and you still have
access to the files on your local host. Make sure you are in the same directory as before, which
includes the docker-compose.yml

    docker stack deploy -c docker-compose.yml getstartedlab
Only this time you’ll see that the services (and associated containers) have been distributed
between both myvm1 and myvm2.

    $ docker stack ps getstartedlab

    ID            NAME                  IMAGE                   NODE   DESIRED STATE
    jq2g3qp8nzwx  getstartedlab_web.1   john/get-started:part2  myvm1  Running
    88wgshobzoxl  getstartedlab_web.2   john/get-started:part2  myvm2  Running
    vbb1qbkb0o2z  getstartedlab_web.3   john/get-started:part2  myvm2  Running
    ghii74p9budx  getstartedlab_web.4   john/get-started:part2  myvm1  Running
    0prmarhavs87  getstartedlab_web.5   john/get-started:part2  myvm2  Running
##### Connecting to VMs with docker-machine env and docker-machine ssh
- To set your shell to talk to a different machine like myvm2, simply re-run docker-machine env in
the same or a different shell, then run the given command to point to myvm2. This is always
specific to the current shell. If you change to an unconfigured shell or open a new one, you
need to re-run the commands. Use docker-machine ls to list machines, see what state they are in,
get IP addresses, and find out which one, if any, you are connected to. To learn more, see the
Docker Machine getting started topics.
- Alternatively, you can wrap Docker commands in the form of docker-machine ssh <machine>
"<command>", which logs directly into the VM but doesn’t give you immediate access to files on
your local host.
- On Mac and Linux, you can use docker-machine scp <file> <machine>:~ to copy files across
machines, but Windows users need a Linux terminal emulator like Git Bash in order for this to
work.
This tutorial demos both docker-machine ssh and docker-machine env, since these are available on
all platforms via the docker-machine CLI.
#### Accessing your cluster
You can access your app from the IP address of either myvm1 or myvm2.  
Run docker-machine ls to get your VMs’ IP addresses.
Having connectivity trouble?

Keep in mind that in order to use the ingress network in the swarm, you need to have the following
ports open between the swarm nodes before you enable swarm mode:

- Port 7946 TCP/UDP for container network discovery.
- Port 4789 UDP for the container ingress network.
### Iterating and scaling your app
### Cleanup and reboot
#### Stacks and swarms
You can tear down the stack with docker stack rm. For example:

    docker stack rm getstartedlab
docker-machine ssh myvm2 "docker swarm leave" on the worker and docker-machine ssh myvm1 "docker
swarm leave --force" on the manager
#### Unsetting docker-machine shell variable settings
    eval $(docker-machine env -u)
#### Restarting Docker machines
If you shut down your local host, Docker machines will stop running. You can check the status of
machines by running docker-machine ls.

    $ docker-machine ls
    NAME    ACTIVE   DRIVER       STATE     URL   SWARM   DOCKER    ERRORS
    myvm1   -        virtualbox   Stopped                 Unknown
    myvm2   -        virtualbox   Stopped                 Unknown
To restart a machine that’s stopped, run:

    docker-machine start <machine-name>
For example:

```
    $ docker-machine start myvm1
    Starting "myvm1"...
    (myvm1) Check network to re-create if needed...
    (myvm1) Waiting for an IP...
    Machine "myvm1" was started.
    Waiting for SSH to be available...
    Detecting the provisioner...
    Started machines may have new IP addresses. You may need to re-run the `docker-machine env` command.

    $ docker-machine start myvm2
    Starting "myvm2"...
    (myvm2) Check network to re-create if needed...
    (myvm2) Waiting for an IP...
    Machine "myvm2" was started.
    Waiting for SSH to be available...
    Detecting the provisioner...
    Started machines may have new IP addresses. You may need to re-run the `docker-machine env` command.
```

#### Commands this part
    docker-machine create --driver virtualbox myvm1 # Create a VM (Mac, Win7, Linux)
    docker-machine create -d hyperv --hyperv-virtual-switch "myswitch" myvm1 # Win10
    docker-machine env myvm1                # View basic information about your node
    docker-machine ssh myvm1 "docker node ls"         # List the nodes in your swarm
    docker-machine ssh myvm1 "docker node inspect <node ID>"        # Inspect a node
    docker-machine ssh myvm1 "docker swarm join-token -q worker"   # View join token
    docker-machine ssh myvm1   # Open an SSH session with the VM; type "exit" to end
    docker node ls                # View nodes in swarm (while logged on to manager)
    docker-machine ssh myvm2 "docker swarm leave"  # Make the worker leave the swarm
    docker-machine ssh myvm1 "docker swarm leave -f" # Make master leave, kill swarm
    docker-machine ls # list VMs, asterisk shows which VM this shell is talking to
    docker-machine start myvm1            # Start a VM that is currently not running
    docker-machine env myvm1      # show environment variables and command for myvm1
    eval $(docker-machine env myvm1)         # Mac command to connect shell to myvm1
    & "C:\Program Files\Docker\Docker\Resources\bin\docker-machine.exe" env myvm1 | Invoke-Expression
    # Windows command to connect shell to myvm1
    docker stack deploy -c <file> <app>  # Deploy an app; command shell must be set to talk to manager
    (myvm1), uses local Compose file
    docker-machine scp docker-compose.yml myvm1:~ # Copy file to node's home dir (only required if you
        use ssh to connect to manager and deploy the app)
    docker-machine ssh myvm1 "docker stack deploy -c <file> <app>"   # Deploy an app using ssh (you
        must have first copied the Compose file to myvm1)
    eval $(docker-machine env -u)     # Disconnect shell from VMs, use native docker
    docker-machine stop $(docker-machine ls -q)               # Stop all running VMs
    docker-machine rm $(docker-machine ls -q) # Delete all VMs and their disk images
## Stacks
A stack is a group of interrelated services that share dependencies, and can be orchestrated and
scaled together. A single stack is capable of defining and coordinating the functionality of an
entire application (though very complex applications may want to use multiple stacks).  
Here, you will take what you’ve learned, make multiple services relate to each other, and run them
on multiple machines.

### Add a new service and redeploy
It’s easy to add services to our docker-compose.yml file. First, let’s add a free visualizer service that lets us look at how our swarm is scheduling containers.  

1. Open up docker-compose.yml in an editor and replace its contents with the following. Be sure to
replace username/repo:tag with your image details.  

```
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
The only thing new here is the peer service to web, named visualizer. You’ll see two new things
here: a volumes key, giving the visualizer access to the host’s socket file for Docker, and a
placement key, ensuring that this service only ever runs on a swarm manager – never a worker. That’s
because this container, built from an open source project created by Docker, displays Docker
services running on a swarm in a diagram.

2. Make sure your shell is configured to talk to myvm1
3. Re-run the docker stack deploy command on the manager,
and whatever services need updating will be updated:

    $ docker stack deploy -c docker-compose.yml getstartedlab
    Updating service getstartedlab_web (id: angi1bf5e4to03qu9f93trnxm)
    Creating service getstartedlab_visualizer (id: l9mnwkeq2jiononb5ihz9u7a4)
4. Take a look at the visualizer.
You saw in the Compose file that visualizer runs on port 8080. Get the IP address of one of your
nodes by running docker-machine ls. Go to either IP address at port 8080 and you will see the
visualizer running.

The single copy of visualizer is running on the manager as you expect, and the 5 instances of web
are spread out across the swarm. You can corroborate this visualization by running docker stack ps
<stack>:

    docker stack ps getstartedlab
The visualizer is a standalone service that can run in any app that includes it in the stack. It
doesn’t depend on anything else. Now let’s create a service that does have a dependency: the Redis
service that will provide a visitor counter.  

### Persist the data
1. Save this new docker-compose.yml file, which finally adds a Redis service. Be sure to replace
username/repo:tag with your image details.

```
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
- redis accesses an arbitrary directory in the host’s file system as /data inside the container, which is where Redis stores data.
Together, this is creating a “source of truth” in your host’s physical filesystem for the Redis
data. Without this, Redis would store its data in /data inside the container’s filesystem, which
would get wiped out if that container were ever redeployed.
2. Create a ./data directory on the manager:

    docker-machine ssh myvm1 "mkdir ./data"
3. Make sure your shell is configured to talk to myvm1
4. Run docker stack deploy one more time.

    $ docker stack deploy -c docker-compose.yml getstartedlab
5. Run docker service ls to verify that the three services are running as expected.
```
    $ docker service ls
    ID            NAME                      MODE        REPLICAS  IMAGE                            PORTS
    x7uij6xb4foj  getstartedlab_redis       replicated  1/1       redis:latest                     *:6379->6379/tcp
    n5rvhm52ykq7  getstartedlab_visualizer  replicated  1/1       dockersamples/visualizer:stable  *:8080->8080/tcp
    mifd433bti1d  getstartedlab_web         replicated  5/5       orangesnap/getstarted:latest     *:80->80/tcp
```
Check the web page at one of your nodes (e.g. http://192.168.99.101) and you’ll see the results of
the visitor counter, which is now live and storing information on Redis.  
Also, check the visualizer at port 8080 on either node’s IP address, and you’ll see the redis
service running along with the web and visualizer services.

## Deploy your app
### Choose an option
To set up and deploy:

- Connect Docker Cloud with your preferred provider, granting Docker Cloud permission to automatically
- provision and “Dockerize” VMs for you.
- Use Docker Cloud to create your computing resources and create your swarm.
Deploy your app.

#### Connect Docker Cloud
### Introduction
# SKIPPED HERE
# TODOOOO
## Deploy your app
#
