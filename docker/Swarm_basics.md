## Swarm Basic Features

Create a swarm with 3 nodes first.

### Overlay Multi-Host Networking

Swarm-wide bridge network driver where containers can communicate across nodes.

- Just choose `--driver overlay` when creating network
- For container-to-container traffic inside a single Swarm
- Optional IPSec (AES) encryption on network creation
- Each service can be connected to multiple networks
  - e.g. front end and back end

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

- Routes ingress (incoming) packages for a Service to proper Task
- Spans all nodes in Swarm
- Uses IPVS from Linux Kernel
- Load balances Swarm Services across their Tasks
- Two ways this works:
  - Container-to-container in a Overlay network (uses VIP)
  - External traffic incoming to published ports (all nodes listen)

``` sh
$ docker service create --name search --replicas 3 -p 9200:9200 elasticsearch:2
```

- This is stateless load balancing
- The LB is at OSI Layer 3 (TCP), not Layer 4 (DNS)
- Both limitation can be overcome with:
  - Nginx or HAProxy LB proxy, or
  - Docker Enterprise Edition, which comes with built-in L4 web proxy