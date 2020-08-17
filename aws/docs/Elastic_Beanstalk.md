### Developer problems on AWS

- Managing infrastructure
- Deploying Code
- Configuring all the databases, load balancers, etc
- Scaling concerns

## Elastic Beanstalk

Elastic Beanstalk is a developer centric view of deploying an application on AWS. It uses many component's: EC2, ASG, ELB, RDS, etc... But it's all in one view that's easy to make sense of. The Beanstalk itself is free, you pay for resources created behind it.

### What is Beanstalk

- Managed service
  - Instance configuration/ OS is handled by Beanstalk
  - Deployment strategy is configurable but performed by Elastic Beanstalk
- Just the application code is the responsibility of the developer

#### Three architecture models in Beanstalk

- Single instance deployment: good for dev
- Load Balancer + Auto-scaling Group: great for production or pre-production web apps
- ASG only: great for non-web apps in production (workers, etc...)

#### Three Components

1. Application
2. Application Version: each deployment gets assigned a version
3. Environment name (dev, test, prod): free naming

You have full control over lifecycle of environments:

- You deploy application versions to environments and can promote application versions to the next environment.
- You can also rollback feature to previous application version.

### Beanstalk environment

### Elastic Beanstalk Deployment Modes

1. Single Instance: Great for dev
2. High Availability with Load Balancer: Great for prod

#### [Elastic Beanstalk Deployment Options for updates](https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/using-features.deploy-existing-version.html)

- **All at once** (deploy all in one go):
  - Fastest, but instances has downtime.
  - Great for quick iterations in development environment.
  - No additional cost
- **Rolling:** update a few instances at a time (bucket), and then move onto the next bucket once the first bucket is healthy
  - Application is running below capacity (partially replacement at a time)
  - Application is running both versions simultaneously
  - No additional cost
  - Long deployment
- **Rolling with additional batches:** like rolling, but spins up new instances to move the batch (so that the old application is still available)
  - Application is running at capacity (sometimes over capacity)
  - Can set the bucket size
  - Application is running both versions simultaneously
  - Small additional cost
  - Additional batch is removed at the end of the deployment
  - Longer deployment
  - Good for prod
  - ![](./images/Beanstalk_rolling_with_additional_batches.png)
- **Immutable:** spins up new instances in a new ASG, deploys version to these instances, and then swaps all the instances when everything is healthy
  - Zero downtime
  - New code is deployed to new instances on a temporary ASG
    - At first, make sure one instance works. Then launch all new instances.
    - When launch work is done. All instances in the new ASG will be moved to the permanent ASG.
    - At last, terminate all old instances.
  - High cost, double capacity
  - Longest deployment
  - Quick rollback in case of failures (just terminate new ASG)
  - Great for prod
- **Blue/ Green **(much manual work)
  - Not a "direct feature" of Elastic Beanstalk
  - Zero downtime and release facility
  - Create a new "stage" environment and deploy v2 there
  - The new environment (green) can be validated independently and roll back if issues
  - Route 53 can be setup using weighted policies to redirect a little bit of traffic to the stage environment
  - Using Beanstalk, "swap URLs" when done with the environment test

### Elastic Beanstalk CLI

We can install an additional CLI called the "EB cli" which makes working with Beanstalk from the CLI easier.

Basic commands: eb create/ eb status/ eb health/ eb events/ eb logs/ eb open/ eb deploy/ eb config/ eb terminate...

### Elastic Beanstalk Deployment Process

- Describe dependencies
  - requirements.txt for Python, package.json for Node.js
- Package code as zip, and describe dependencies
- Console: upload zip file (creates new app version), and then deploy
- CLI: create new app version using CLI (uploads zip), and then deploy

Elastic Beanstalk will deploy the zip on each EC2 instance, resolve dependencies and start the application.

### Beanstalk Lifecycle Policy

Elastic Beanstalk can store up to 1000 application versions. If you don't remove old versions, you won't be able to deploy anymore. To phase out old application versions, use a lifecycle policy:

- Based on time (old versions are removed)
- Based on space (when you have too many versions)

Versions that are currently used won't be deleted. Option available not to delete the source bundle in S3 to prevent data loss.

### Elastic Beanstalk Extensions

A zip file containing our code must be deployed to Elastic Beanstalk. All the parameters set in the UI can be configured with code using files.

Requirements:

- Must in the `.ebextensions/` directory in the root of source code
- YAML/ JSON format
- `.config` extensions (e.g. `logging.config`)
- Able to modify some default settings using: `option_setttings`
- Ability to add resources such as RDS, ElastiCache, DynamoDB, etc...

Resources managed by `.ebextensions` get deleted if the environment goes away.

### Elastic Beanstalk & CloudFormation

Under the hood, Elastic Beanstalk relies on CloudFormation. CloudFormation is used to provision other AWS services.

Use case: you can define CloudFormation resources in you `.ebextensions` to provision ElastiCache, an S3 bucket, anything you want.

### Elastic Beanstalk Cloning

Clone an environment with exact same configuration. Useful for deploying a "test" version of your application. After cloning an environment, you can change settings.

### Elastic Beanstalk Migration: Load Balancer

After creating an Elastic Beanstalk environment, you cannot change the ELB type (only the configuration).

To migrate:

1. Create a new environment with the same configuration except LB (can't clone)
2. Deploy your application onto the new environment
3. Perform a CNAME swap or Route 53 update

### RDS with Elastic Beanstalk

RDS can be provisioned with Beanstalk, which is great for dev/ test. But this is not great for prod as the database lifecycle is tied to the Beanstalk environment lifecycle.

The best for prod is to separately create an RDS database and provide our EB application with the connection string.

#### Elastic Beanstalk Migration: Decouple RDS

1. Create a snapshot of RDS DB (as a safeguard)
2. Go to the RDS console and protect the RDS database from deletion
3. Create a new Elastic Beanstalk environment, without RDS, point your application to existing RDS
4. Perform a CNAME swap (blue/green) or Route 53 update, confirm working
5. Terminate the old environment (RDS won't be deleted)
6. Delete CloudFormation stack (in DELETE_FAILED state) manually

### Elastic Beanstalk with Docker

#### Elastic Beanstalk - Single Docker

Run your application as a single docker container. Either provide:

- Dockerfile: Elastic Beanstalk will build and run the Docker container
- Dockerrun.aws.json (v1): Describe were *already built* Docker image is
  - Image
  - Ports
  - Volumns
  - Logging
  - Etc...

Beanstalk in Single Docker Container does not use ECS.

#### Elastic Beanstalk - Multi Docker Containers

Multi Docker helps run multiple containers per EC2 instance in EB. This will create for you:

- ECS Cluster
- EC2 instances, configured to use the ECS Cluster
- Load Balancer (in high availability mode)
- Task definitions and execution

Requires a `config Dockerrun.aws.json (v2)` at the root of source code. It is used to generate the ECS task definition. And your Docker images must be pre-build and stored in ECR for example.

### Elastic Beanstalk advanced concepts

#### Elastic Beanstalk and HTTPS

##### Elastic Beanstalk with HTTPS

- Idea: Load the SSL certificate onto the Load Balancer
- Can be done from the Console (EB console, load balancer configuration)
- Can be done from the code: `.ebextensions/securelistener-alb.config`
- SSL Certificate can be provisioned using ACM (AWS Certificate Manage) or CLI
- Must configure a security group rule to allow incoming port 443 (HTTPS port)

##### Elastic Beanstalk redirect HTTP to HTTPS

- [Configure your instance to redirect HTTP to HTTPS](https://github.com/awsdocs/elastic-beanstalk-samples/tree/master/configuration-files/aws-provided/security-configuration/https-redirect)

- Or configure the Application Load Balancer (ALB only) with a rule
- Make sure health checks are not redirected (so they keep giving 200 OK)

#### Web Server vs Worker Environment

If your application performs tasks that are long to complete (e.g. processing a video, generating a zip file, etc.), offload these tasks to a dedicated worker environment.

Decoupling your application into two tiers is common. You can define periodic tasks in a file `cron.yaml`.

The architecture is like:

You have a web tier (ELB + EC2) sends messages to a SQS Queue in the worker tier (SQS + EC2). And instances will read from SQS and finish the tasks.

#### Elastic Beanstalk - Custom Platform (Advanced)

Custom Platforms are very advanced, they allow to define from scratch:

- The OS
- Additional Software
- Scripts that Beanstalk runs on these platforms

Use case: app language is incompatible with Beanstalk & doesn't use Docker.

To create your own platform:

- Define an AMI using `Platform.yaml` file.
- Build that platform using the Packer software (open source tool to create AMIs)

##### Custom Platform vs Custom Image (AMI)

Custom Image is to tweak an *existing* Beanstalk Platform (Python, Node.js, Java...).

Custom Platform is to create an *entirely new* Beanstalk Platform.

