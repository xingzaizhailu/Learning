## Services Updates

- Provides rolling replacement of tasks/ containers in a service.

- Limits downtime ("prevents" downtime is testing's job)
- Has many, many CLI options to control the update
- Create options will usually change, adding `-add` or `-rm` to them
- Includes rollback and healthcheck options
- Also has scale & rollback subcommand for quicker access
  - `docker service scale web=4` and `docker service rollback web`
- A stack deploy, when pre-existing, will issue service updates

### Examples

- Update the image used to a newer/older version:
  - `docker service update --image myapp:1.2.1 <servicename>`.
  - one at a time by default
- Change a port
  - docker service update --publish-rm 8080 --publish-add 9090:80

- Adding an environment variable and remove a port
  - `docker service update --env-add NODE_ENV=production --publish-rm 8080`.

- Change number of replicas of two services:
  - `docker service scale web=8 api=6`
- Rebalancing containers among nodes
  - docker service update --force web

- Just edit the YAML file, then
  - `docker stack deploy -c file.yml <stackname>`.s

