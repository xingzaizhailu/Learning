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

### Full app lifecycle with Compose

Single set of compose files for:

- Local `docker-compose up` for development environment
- Remote `docker-compose up` for CI environment
- Remote `docker stack deploy` for production environment

These files are:

- default `docker-compose.yml` file
- `docker-compose.override.yml`: `docker-compose` will automatically bring this up if the file is named exactly like this, i.e. this file override what's in `docker-compose.yml`
- `docker-compose.test.yml`
- `docker-compose.prod.yml`

default `docker-compose.yml` file:

``` yaml
version: '3.1'

services:
	drupal:
		image: bretfisher/custom-drupal:latest
	postgres:
		image: postgres:9.6

```

`docker-compose.override.yml`

```yaml
version: '3.1'

services:
	drupal:
		# because image name already specified in `docker-compose.yml` file
		build: .
		ports:
			- "8080:80"
		volumes:
			- drupal-modules:/var/www/html/modules
      - drupal-profiles:/var/www/html/profiles
      - drupal-sites:/var/www/html/sites
      - ./themes:/var/www/html/themes

  postgres:
    environment:
      - POSTGRES_PASSWORD_FILE=/run/secrets/psql_password
    secrets:
      - psql_password
    volumes:
      - drupal-data:/var/lib/postgresql/data

volumes:
  drupal-modules:
  drupal-profiles:
  drupal-sites:
  drupal-themes:
  drupal-data:

secrets:
  psql_password:
    file: psql-fake-password.txt
```

`docker-compose.test.yml`:

``` yaml
version: '3.1'

services:
	drupal:
		image: bretfisher/custom-drupal
		build: .
		ports:
			- "80:80"

	postgres:
		environment:
			- POSTGRES_PASSWORD_FILE=/run/secrets/psql_password
		secrets:
			- psql_password
		volumes:
			- ./sample-data:/var/lib/postgresql/data

secrets:
	psql_password:
		file: psql-fake-password.txt
```

`docker-compose.prod.yml`:

``` yaml
version: '3.1'

services:
	drupal:
		ports:
			- "80:80"
		volumes:
			- drupal-modules:/var/www/html/modules
      - drupal-profiles:/var/www/html/profiles
      - drupal-sites:/var/www/html/sites
      - drupal-themes:/var/www/html/themes

  postgres:
    environment:
      - POSTGRES_PASSWORD_FILE=/run/secrets/psql_password
    secrets:
      - psql_password
    volumes:
      - drupal-data:/var/lib/postgresql/data

volumes:
  drupal-modules:
  drupal-profiles:
  drupal-sites:
  drupal-themes:
  drupal-data:

secrets:
  psql_password:
    external: true
```

``` sh
# development
$ docker-compose up -d # run docker-compose.yml and overlay docker-compose.override.yml on top
# could try `docker inspect <drupal_service>`, you'll see all settings from override file

# CI
$ docker-compose -f docker-compose -f docker-compose.yml -f docker-compose.test.yml up -d
# try `docker inspect <drupal_service>` again
$ docker-compose -f docker-compose.yml -f docker-compose.prod.yml config > output.yml

# Deploy
$ docker stack deploy output.yml
```

