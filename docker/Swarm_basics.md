## Swarm Basic Features

Create a swarm with 3 nodes first.

### Overlay Multi-Host Networking

Swarm-wide bridge network driver where containers can communicate across nodes.

- Just choose `--driver overlay` when creating network
- For container-to-container traffic inside a single Swarm
- Optional IPSec (AES) encryption on network creation
- Each service can be connected to multiple networks
  - e.g. front-end and back-end

```sh
$ docker network create --driver ovlerlay mydrupal
$ docker network ls

$ docker service create --name psql --network mydrupal -e POSTGRES_PASSWORD=mypass postgres
$ docker service ls
$ docker service ps psql
$ docker container logs psql.1.<ID>

$ docker service create --name drupal --network mydrupal -p 80:80 drupal
$ watch docker service ls	# Rerun the command by ubuntu again and again
```

And no matter which node `mydrupal` is deployed to, you can visit it by any of the nodes:80.

### Routing Mesh

What makes `mydrupal` available from any of the nodes.

- Routes ingress (incoming) packages for a Service to proper Task
- Spans all nodes in Swarm
- Uses IPVS from Linux Kernel
- Load balances Swarm Services across their Tasks
- Two ways this works:
  - Container-to-container in a Overlay network (uses VIP)
  - External traffic incoming to published ports (all nodes listen)

``` sh
$ docker service create --name search --replicas 3 -p 9200:9200 elasticsearch:2
$ docker service ps search
$ curl localhost:9200		# multiple times
```

#### Routing Mesh Cout.

- This is stateless load balancing
- The LB is at OSI Layer 3 (TCP), not Layer 4 (DNS)
- Both limitation can be overcome with:
  - Nginx or HAProxy LB proxy, or
  - Docker Enterprise Edition, which comes with built-in L4 web proxy

### Secrets Storage for Swarm 

- Only stored on disk on Manager nodes
- Secrets are first stored in Swarm, then assigned to a Service(s)
- Only containers in assigned Service(s) can see them
- They look like files in container but are actually in-memory file system
  - /run/secretes/<secret_name> or /run/secrets/<secret_alias>
- Local docker-compose can use file-based secrets, but not secure

#### Using secrets in Swarm Services

```sh
# file based
$ cat psql_user.txt
mypsqluser
$ docker secret create psql_user psql_user.txt
# or CLI
$ echo "myDBpassWORD" | docker secret create psql_pass -
$ docker secret ls
```

```sh
$ docker sevice create --name psql --secret psql_user --secret psql_pass \
	-e POSTGRES_PASSWORD_FILE=/run/secrets/psql_pass \
	-e POSTGRES_USER_FILE=/run/secrets/psql_user postgres
$ docker service update --secret-rm
```

#### Using secrets in Swarm Stacks

Create a `docker-compose.yml` file:

``` yaml
version: "3.1"

services:
	psql:
		image: postgres
		secrets:
			- psql_user
			- psql_password
		environment:
			POSTGRES_PASSWORD_FILE: /run/secrets/psql_password
			POSTGRES_USER_FILE: /run/secrets/psql_user

secrets:
  psql_user:
	  file: ./psql_user.txt
  psql_password:
  	external: true
```

``` sh
docker stack deploy -c docker-compose.yml mydb
```

#### Using secrets with local docker compose

``` sh
# under folder where saves previous `docker-compose.yml`
$ docker-compose up -d
# cat secrets, but only works for file based secrets
$ docker-compose exec psql cat /run/secrets/psql_user
dbuser
```

