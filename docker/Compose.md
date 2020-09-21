## Compose

Why:

- Configure relationships between containers
- Save our docker container run settings in easy-to-read file
- Create one-liner developer environment startups

What:

- YAML-formatted file that describes our solution options for
  - containers
  - networks
  - volumes
- A CLI toll `docker-compose` used for local dev/test automation with those YAML files

### `docker-compose.yml`

`docker-compose.yml` is default filename, but any can be used with `docker-compose -f`.

```yaml
version: '3.1'

services:					# Containers, same as docker run
	Servicename:
		image: 								# Optional if you use build
		command:							# Optional, replace the default CMD specified by the image
		environment:					# Optional, same as -e in docker run
		volumes:							# Optional, save as -v in docker run
		ports:
		depends_on:

	Servicename2:

volumes:					# Optional, same as docker volume create
networks: 				# Optional, same as docker network create
```

``` YAML
version: '3'

services:
	wordpress:
		image: wordpress
		ports:
			- 8080:80
		environment:
			WORDPRESS_DB_PASSWORD: example
		volumes:
			- ./wordpress-data:/var/www/html

	mysql:
		image: mariadb
		environment:
			MYSQL_ROOT_PASSWORD: example
		volumes:
			- ./mysql-data:/var/lib/mysql
```

### Compose CLI

Two common commands:

- `docker-compose up`  # set up volumes/networks and start all containers
- `docker-compose down`  # stop all containers and remove cont/vol/net

### Using Compose to Build

It will build the image with `docker-compose up` if not found in cache. Also rebuild with `docker-compose build`. It's great for complex builds that have lots of vars  or build args.

``` yaml
version: "3.8"

services:
	servicename:
		build: .
```

Or more customised:

```yaml
version: "3.8"

services:
	servicename:
		build:
			context: .
			dockerfile: Dockerfile-alternate
			args:
				arg1: val1
		image: image_name:tag
		ports:
		volumes:
```

