## Swarms

### Containers Everywhere = New Problems

How to:

- automate container lifecycle?
- easily scale out/in/up/down?
- ensure our containers are re-created if they fail?
- replace containers without downtime (blue/green deploy)?
- control/track where containers get started?
- create cross-node virtual networks?
- ensure only trusted servers run our containers?
- store secrets, keys, passwords and get them to the right container (and only that container)?

### Introduction

Previously, you took an app you wrote, and defined how it should run in production by turning it into a service, scaling it up 5x in the process.  

Here, you deploy this application onto a cluster, running it on multiple machines. Multi-container, multi-machine applications are made possible by joining multiple machines into a “Dockerized” cluster called a **swarm**.

### Understanding Swarm clusters

A swarm is a group of machines that are running Docker and joined into a cluster. After that has happened, you continue to run the Docker commands you’re used to, but now they are executed on a cluster by a swarm manager. The machines in a swarm can be physical or virtual. After joining a swarm, they are referred to as **nodes**.  

Swarm managers can use several *strategies* to run containers, such as *“emptiest node”* – which fills the least utilized machines with containers. Or *“global”*, which ensures that each machine gets exactly one instance of the specified container. You instruct the swarm manager to use these strategies in the Compose file, just like the one you have already been using.  

Swarm managers are the only machines in a swarm that can execute your commands, or authorize other machines to join the swarm as workers. Workers are just there to provide capacity and do not have the authority to tell any other machine what it can and cannot do.  

### Set up your swarm

A **swarm** is made up of multiple nodes, which can be either physical or virtual machines. The basic concept is simple enough: run `docker swarm init` to enable swarm mode and make your current machine a swarm manager, then run `docker swarm join` on other machines to have them join the swarm as workers.  

Once enabled, there will be more commands:

- `docker swarm`
- `docker node`
- `docker service`
- `docker stack`
- `docker secret`

#### Create a cluster

First, you’ll need a hypervisor that can create virtual machines (VMs), so install Oracle VirtualBox for your machine’s OS.  

Now, create a couple of VMs using `docker-machine`, using the VirtualBox driver:

```shell
$ docker-machine create --driver virtualbox node1
$ docker-machine create --driver virtualbox node2
```

##### LIST THE VMS AND GET THEIR IP ADDRESSES

You now have two VMs created, named "node1" and "node2". 
Use this command to list the machines and get their IP addresses.  

```shell
$ docker-machine ls
NAME    ACTIVE   DRIVER       STATE     URL                       SWARM   DOCKER        ERRORS
node1   -        virtualbox   Running   tcp://192.168.99.100:2376         v17.06.2-ce   
node2   -        virtualbox   Running   tcp://192.168.99.101:2376         v17.06.2-ce   
```

##### INITIALIZE THE SWARM AND ADD NODES

The first machine will act as the manager, which executes management commands and authenticates workers to join the swarm, and the second will be a worker.

You can send commands to your VMs using `docker-machine ssh`. Instruct "node1" to become a swarm manager with `docker swarm init` and you’ll see output like this:

```shell
$ docker-machine ssh node1 "docker swarm init --advertise-addr <node1 ip>"
Swarm initialized: current node <node ID> is now a manager.

To add a worker to this swarm, run the following command:

docker swarm join \
--token <token> \
<node ip>:<port>

To add a manager to this swarm, run 'docker swarm join-token manager' and follow the instructions.
```

As you can see, the response to `docker swarm init` contains a pre-configured `docker swarm join` command for you to run on any nodes you want to add. Copy this command, and send it to node2 via `docker-machine ssh` to have node2 join your new swarm as a worker:  

```shell
$ docker-machine ssh node2 "docker swarm join \
--token <token> \
<node1_ip>:2377"

This node joined a swarm as a worker.
```

Congratulations, you have created your first swarm!  

Run `docker node ls` on the manager to view the nodes in this swarm:

```shell
$ docker-machine ssh node1 "docker node ls"
ID                            HOSTNAME        STATUS        AVAILABILITY    MANAGER STATUS
brtu9urxwfd5j0zrmkubhpkbd     node2           Ready         Active
rihwohkh3ph38fhillhhb84sk *   node1           Ready         Active          Leader
```

If you want to make node2 also a manager, you could use

``` shell
$ docker node update --role manager node2
```

A manager node has components (Raft):

- API: Accepts commands from client and creates service object
- Orchestrator: Reconciliation loop for service objects and creates tasks
- Allocator: Allocates IP addresses to tasks
- Scheduler: Assigns nodes to tasks
- Dispatcher: checks in on workers

A worker node has:

- Worker: Connects to dispatcher to check on assigned tasks
- Executor: Executes the tasks assigned to worker node

### Deploy your app on the swarm cluster

#### Configure a `docker-machine` shell to the swarm manager

So far, you’ve been wrapping Docker commmands in `docker-machine ssh` to talk to the VMs. Another option is to run `docker-machine env <machine>` to get and run a command that configures your current shell to talk to the Docker daemon on the VM. This method works better for the next step because it allows you to use your local `docker-compose.yml` file to deploy the app “remotely” without having to copy it anywhere.  

Type `docker-machine env node1`, then copy-paste and run the command provided as the last line of the output to configure your shell to talk to node1, the swarm manager.  

The commands to configure your shell differ depending on whether you are Mac, Linux, or Windows, so examples of each are shown on the tabs below.

##### DOCKER MACHINE SHELL ENVIRONMENT ON MAC OR LINUX

Run `docker-machine env node1` to get the command to configure your shell to talk to node1.

```shell
$ docker-machine env node1
export DOCKER_TLS_VERIFY="1"
export DOCKER_HOST="tcp://192.168.99.100:2376"
export DOCKER_CERT_PATH="/Users/sam/.docker/machine/machines/node1"
export DOCKER_MACHINE_NAME="node1"
#Run this command to configure your shell:
#eval $(docker-machine env node1)
```

Run the given command to configure your shell to talk to node1.

```shell
$ eval $(docker-machine env node1)
```

Run `docker-machine ls` to verify that node1 is now the active machine, as indicated by the asterisk next to it.

```shell
$ docker-machine ls
NAME    ACTIVE   DRIVER       STATE     URL                        SWARM  DOCKER       ERRORS
node1   *        virtualbox   Running   tcp://192.168.99.100:2376         v17.06.2-ce  
node2   -        virtualbox   Running   tcp://192.168.99.101:2376         v17.06.2-ce  
```

##### Deploy the app on the swarm manager

Now that you have my node1, you can use its powers as a swarm manager to deploy your app by using the same docker stack deploy command you used in part 3 to node1, and your local copy of `docker-compose.yml`.

You are connected to node1 by means of the docker-machine shell configuration, and you still have access to the files on your local host. Make sure you are in the same directory as before, which includes the `docker-compose.yml`

```shell
$ docker stack deploy -c docker-compose.yml getstartedlab
```

Only this time you’ll see that the services (and associated containers) have been distributed between both node1 and node2.

```shell
$ docker stack ps getstartedlab

ID            NAME                  IMAGE                   NODE   DESIRED STATE
jq2g3qp8nzwx  getstartedlab_web.1   john/get-started:part2  node1  Running
88wgshobzoxl  getstartedlab_web.2   john/get-started:part2  node2  Running
vbb1qbkb0o2z  getstartedlab_web.3   john/get-started:part2  node2  Running
ghii74p9budx  getstartedlab_web.4   john/get-started:part2  node1  Running
0prmarhavs87  getstartedlab_web.5   john/get-started:part2  node2  Running
```

##### Connecting to VMs with docker-machine env and docker-machine ssh
- To set your shell to talk to a different machine like node2, simply re-run docker-machine env in the same or a different shell, then run the given command to point to node2. This is always specific to the current shell. If you change to an unconfigured shell or open a new one, you need to re-run the commands. Use `docker-machine ls` to list machines, see what state they are in, get IP addresses, and find out which one, if any, you are connected to.
- Alternatively, you can wrap Docker commands in the form of docker-machine ssh <machine> "<command>", which logs directly into the VM but doesn’t give you immediate access to files on your local host.
- On Mac and Linux, you can use `docker-machine scp <file> <machine>:~` to copy files across machines, but Windows users need a Linux terminal emulator like Git Bash in order for this to work. This tutorial demos both docker-machine ssh and docker-machine env, since these are available on all platforms via the docker-machine CLI.

#### Accessing your cluster
You can access your app from the IP address of either node1 or node2. Run `docker-machine ls` to get your VMs’ IP addresses. Having connectivity trouble?

Keep in mind that in order to use the ingress network in the swarm, you need to have the following ports open between the swarm nodes before you enable swarm mode:

- Port 7946 TCP/UDP for container network discovery.
- Port 4789 UDP for the container ingress network.

### Iterating and scaling your app

### Cleanup and reboot

#### Stacks and swarms
You can tear down the stack with docker stack rm. For example:

```shell
$ docker stack rm getstartedlab
```

`docker-machine ssh node2 "docker swarm leave"` on the worker and `docker-machine ssh node1 docker swarm leave --force` on the manager

#### Unsetting docker-machine shell variable settings

```shell
$ eval $(docker-machine env -u)
```

#### Restarting Docker machines
If you shut down your local host, Docker machines will stop running. You can check the status of machines by running docker-machine ls.

```shell
$ docker-machine ls
NAME    ACTIVE   DRIVER       STATE     URL   SWARM   DOCKER    ERRORS
node1   -        virtualbox   Stopped                 Unknown
node2   -        virtualbox   Stopped                 Unknown
```

To restart a machine that’s stopped, run:

```shell
$ docker-machine start <machine-name>
```

For example:

```shell
$ docker-machine start node1
Starting "node1"...
(node1) Check network to re-create if needed...
(node1) Waiting for an IP...
Machine "node1" was started.
Waiting for SSH to be available...
Detecting the provisioner...
Started machines may have new IP addresses. You may need to re-run the `docker-machine env` command.

$ docker-machine start node2
...
```

#### Commands this part

```shell
$ docker-machine create --driver virtualbox node1 # Create a VM (Mac, Win7, Linux)
$ docker-machine create -d hyperv --hyperv-virtual-switch "myswitch" node1 # Win10
$ docker-machine env node1                # View basic information about your node
$ docker-machine ssh node1 "docker node ls"         # List the nodes in your swarm
$ docker-machine ssh node1 "docker node inspect <node ID>"        # Inspect a node
$ docker-machine ssh node1 "docker swarm join-token -q worker"   # View join token
$ docker-machine ssh node1   # Open an SSH session with the VM; type "exit" to end
$ docker node ls                # View nodes in swarm (while logged on to manager)
$ docker-machine ssh node2 "docker swarm leave"  # Make the worker leave the swarm
$ docker-machine ssh node1 "docker swarm leave -f" # Make master leave, kill swarm
$ docker-machine ls # list VMs, asterisk shows which VM this shell is talking to
$ docker-machine start node1            # Start a VM that is currently not running
$ docker-machine env node1      # show environment variables and command for node1
$ eval $(docker-machine env node1)         # Mac command to connect shell to node1
$ "C:\Program Files\Docker\Docker\Resources\bin\docker-machine.exe" env node1 | Invoke-Expression # Windows command to connect shell to node1
$ docker stack deploy -c <file> <app>  # Deploy an app; command shell must be set to talk to manager (node1), uses local Compose file
$ docker-machine scp docker-compose.yml node1:~ # Copy file to node's home dir (only required if you use ssh to connect to manager and deploy the app)
$ docker-machine ssh node1 "docker stack deploy -c <file> <app>"   # Deploy an app using ssh (you must have first copied the Compose file to node1)
$ eval $(docker-machine env -u)     # Disconnect shell from VMs, use native docker
$ docker-machine stop $(docker-machine ls -q)               # Stop all running VMs
$ docker-machine rm $(docker-machine ls -q) # Delete all VMs and their disk images
```



