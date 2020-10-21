## Registries

### Docker Hub: Digging Deeper

It's really Docker Registry plus lightweight image building

Link docker hub to github/ bitbucket: `Create -> Create Automated Build -> choose..`

### Docker Registry

A private image registry for your network.

Some resources:

- [Secure your Registry with TLS](https://docs.docker.com/registry/configuration/)
- Storage cleanup via [Garbage Collection](https://docs.docker.com/registry/garbage-collection/)
- Enable Hub caching via ["--registry-mirror"](https://docs.docker.com/registry/recipes/mirror/)

### Run a Private Docker Registry

#### Registry and Prosper TLS

- "Secure by Defaul": Docker won't talk to registry without HTTPS
  - Except, localhost (127.0.0.0/8)
- For remote self-signed TLS, enable "insecure-registry" in engine

``` sh
$ docker container run -d -p 5000:5000 --name registry -v $(pwd)/registry-data:/var/lib/registry registry
$ docker container ls

$ docker pull hello-world
$ docker tag hello-world 127.0.0.1:5000/hello-world
$ docker push 127.0.0.1:5000/hello-world

$ docker iamge remove hello-world
$ docker iamge remove 127.0.0.1:5000/hello-world
$ docker run 127.0.0.1:5000/hello-world
```

### Docker Registry with Swarm

Because of Routing Mesh, all nodes can see 127.0.0.1:5000. Remember to decide how to store images (volume driver) in production.

Experiment on: [Play with Docker](https://labs.play-with-docker.com)

``` sh
$ docker service create --name registry --publish 5000:5000 registry
$ docker service ps registry
# browser: 127.0.0.1:500/v2/_catalog

$ docker pull hello-world
$ docker tag hello-world 127.0.0.1:5000/hello-world
$ docker push 127.0.0.1:5000/hello-world
# browser: 127.0.0.1:500/v2/_catalog
```