### Get started

#### Limit Your Simultaneous Innovation

Many initial container projects are too big in one scope. Solutions you maybe don't need day one:

- Fully automatic CI/CD
- Dynamic performance scaling
- Containerizing all or nothing
- Starting with persistent data

#### Legacy Apps Work In Containers Too

- Microservice conversion isn't required
- 12 Factor is a horizon we're always chasing
  - Don't let these ideals delay containerization

#### What to Focus On First: Dockerfiles

- More important than fancy orchestration
- It's your new build and environment documentation
- Study Dockerfile/ ENTRYPOINT of Hub Officials
- FROM Official distros that are most familiar

##### Dockerfile Maturity Model

- Make it start
- Make it log all things to stdout/stderr
- Make it documented in file
- Make it work for others
- Make it lean
- Make it scale

##### Dockerfile Anti-pattern

###### Trapping Data

- Problems: Storing unique data in container.

- Solution: Define VOLUME for each location.

###### Using Latest

- Problem: Image builds pull FROM latest
- Solution: Use specific FROM tags
- Problem: Image builds install latest packages
- Solution: Specify version for critical apt/yum/apk packages

###### Leaving Default Config

- Problem: Not changing app defaults, or blindly copying VM conf
  - e.g. php.ini, mysql.conf.d, java memory
- Solution: Update default configs via ENV, RUN, and ENTRYPOINT

###### Environment Specific

- Problem: Copy in environment config at image build
- Solution: Single Dockerfile with default ENV's and overwrite per-environment with ENTRYPOINT script

#### Containers-on-VM or Container-on-Bare-Metal

Do either, or both (Stick with what you are good at). Lots of pros/cons to either.

- Do some basic performance testing. You will learn lots
- 2017 Docker Inc. and HPE whitepaper on MySQL benchmark
  - [bretfisher.com/dockercon17eu]()

#### OS Linux Distribution / Kernel Matters

- Docker is very kernel and storage driver dependent
- Innovations/ fixes are still happening here
- "Minimum" version that works != "best" version
- No pre-existing opinion? Ubuntu 16.04 LTS
  - Popular, well-tested with Docker
  - 4.x kernel and wide storage driver support
- On InfraKit and LinuxKit
- Get correct Docker for your distro from [store.docker.com]()

#### Container Base Distribution: Which One?

Which FROM image should you use?

- Don't make a decision based on image size (remember it's Single Instance Storage)

- At first: match your existing deployment process

- Consider changing to Alpine later, maybe much later

#### Good Defaults: Swarm Architectures

- Simple sizing guidelines based off:
  - Docker internal testing
  - Docker reference or architectures
  - Real world deployments
  - Swarm3k lessons learned
- Baby Swarm: 1-Node instead of 1 Docker instance
  - Gives more features than docker run
- HA Swarm: 3-Node
  - Minimum for HA
  - All managers, **don't** set even number of nodes (especially managers)
  - One node can fail
  - Use when very small budget
  - Pet projects or Test/CI
- Biz Swarm: 5-Node
  - Better high-availability
  - All managers
  - Two nodes can fail
  - My minimum for uptime that affects $$$
- Flexy Swarm: 10+ Nodes
  - 5 dedicated Managers
  - Workers in DMZ
  - Anything beyond 5 nodes, stick with 5 managers and rest Workers
  - Control container placement with labels + constraints
- Swole Swarm: 100+ Nodes
  - 5 dedicated managers
  - Resize Managers as you grow
  - Multiple Workers subnets on Private/DMZ
  - Control containers placement with labels + constraints

#### Don't Turn Cattle into Pets

- Assume nodes will be replaced (Don't make nodes specific)
- Assume containers will be recreated
- Docker for (AWS/Azure) does this
- LinuxKit and InfraKit expect it

#### Reasons for Multiple Swarms

##### Bad Reasons

- Different hardware configurations (or OS)
- Different subnets or security groups
- Different availability zones
- Security boundaries for compliance

##### Good Reasons:

- Learning run stuff on Test Swarm
- Geographical boundaries
- Management boundaries using Docker API (or Docker EE RBAC, or other auth plugin)

#### About Windows Server Swarm

- Hard to be "Windows Only Swarm", mix with Linux nodes
- Much to those tools are Linux only
- Windows = Less choice, but easier path
- Recommendation:
  - Managers on Linux
  - Reserve Windows for Windows-exclusive workloads

#### Outsource Well-Defined Plumbing

- Beware the "not implemented here" syndrome
- If challenge to implement and maintain
- \+ Saas/ commercial market is mature
- = Opportunities for outsourcing

##### For your consideration

- Image registry
- Logs
- Monitoring and alerting
- Tools/ Projects: https://github.com/cncf/landscape

#### Pure Open Source Self-Hosted Tech Stack
|               		| Open Source Tools            | Cheap and EAsy |
| ----------------- | ---------------------------- | ----------------- |
| Swarm GUI					| Portainer										 |  |
| Central Monitoring| Prometheus + Grafana  			 | Librato, Sysdig |
| Central Logging   | ELK													 | Docker for AWS/ Azure |
| Layer 7 Proxy     | Flow-Proxy, Traefik          |  |
| Registry          | Docker Distribution + Portus | Docker Hub, Quay |
| CI/CD             | Jenkins                      | Codeship, TravisCI |
| Storage           | REX-Ray                      | Docker for AWS/Azure |
| Networking        | Docker Swarms                |  |
| Orchestration     | Docker Swarms                | Docker for AWS/Azure |
| Runtime | Docker |  |
| HW / OS | InfraKit, Terraform | Docker for AWS/Azure |

Also Function As A Service: OpenFaas

### Summary

- Trim the optional requirements at first
- First, focus on Dockerfile/ docker-compose.yml
- Watch out for Dockerfile anti-patterns
- Stick with familiar OS and FROM images
- Grow Swarm as you grow
- Find ways to outsource plumbing
- Realize parts of your tech stack may change, stay flexible