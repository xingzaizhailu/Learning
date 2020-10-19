## Healthchecks in Dockerfiles

- Docker engine with exec's the command in the container (e.g. `curl localhost`)
- It expects `exit 0` (OK) or `exit 1` (Error)
- Three container states:
  - starting
  - healthy
  - unhealthy
- Not a external monitoring replacement
- It shows up in `docker container ls`
- Check last 5 healthchecks with `docker container inspect`
- Docker run does nothing with healthchecks
- Services will replace tasks if they fail healthcheck
- Service updates wait for them before continuing

### Cont

It's supported in Dockerfile, Compose YAML, docker run and Swarm Services

#### Healthcheck in `docker run`

``` sh
$ docker run \
	--health-cmd="curl -f localhost:9200/_cluster/health || false" \
	--health-interval=5s \
	--health-retries=3 \
	--health-timeout=2s \
	--health-start-period=15s \
	elasticsearch:2
```

#### Healthcheck in Dockerfile

- Options for healthcheck command

  - `--interval=DURATION` (default: 30s)
  - `--timeout=DURATION` (default: 30s)
  - `--start-period=DURATION` (default: 0s, allow longer waiting time before the first check. Note: It's still checked just doesn't count.)
  - `--retries=N` (default: 3)

- Basic command using default options

  - `HEALTHCHECK curl -f http://localhost/ || false`

- Custom options with the command

  - ```dockerfile
    # This is suitable for websites running in Nginx
    HEALTHCHECK --timeout=2s --interval=3s --retries=3 \
    	CMD curl -f http://localhost/ || exit
    ```

##### examples

``` dockerfile
FROM postgres

HEALTHCHECK --interval=5s --timeout=3s \
	CMD pg_isready -U postgres || exit 1
```

#### Healthcheck in Compose/ Stack files

``` yaml
version: "2.1"	# minimum for healthchecks
services:
	web:
		image: nginx
		healthcheck:
			test: ["CMD", "curl", "-f", "http://localhost"]
			interval: 1m30s
			timeout: 10s
			retries: 3
			start_period: 1m # version 3.4 minimum
```

#### Healthcheck in Swarm

``` sh
# Comparing
$ docker service create --name p1 postgres
# and
$ docker service create --name p2 --health-cmd="pg_isready -U postgres || exit 1" postgres
# which turns to `running` after 30s interval
```

