## ECS

### What is Docker

A software development platform to deploy apps. Apps are packaged in containers that can be run on any OS. Apps run the same, regardless of where they're run:

- Any machine
- No compatibility issues
- Predictable behaviour
- Less work
- Easier to maintain and deploy
- Works with any language, any OS, any technology 

#### Docker Containers Management

To manage containers, we need a container management platform:

- ECS: Amazon'w own platform
- Fargate: Amazon's own serverless platform
- EKS: Amazon's managed Kubernetes (open source)

### ECS Clusters

ECS Clusters are logical grouping of EC2 instances. EC2 instances run the ECS agent (Docker container) and the ECS agents registers the instance to the ECS cluster. The EC2 instances run a special AMI, made specifically for ECS.

### ECS Task Definitions

Task definitions are metadata in JSON form to tell ECS how to run a Docker Container. It contains crucial information around:

- Image Name
- Port Binding for Container and Host
- Memory and CPU required
- Environment variables
- Networking information

### ECS Service

ECS Services help define how many task should run and how they should be run. They ensure that the number of tasks desired is running across our fleet of EC2 instances.

### ECS Service with Load Balancer

ECS Service can be linked to ELB/ NLB/ ALB if needed.

We can set Host Port to be random (leave it empty or set port=0) for task definition so multiple identical tasks can run on the same EC2 instance without port conflicts. And using an Application Load Balancer with dynamic port forwarding 

The EC2 instance security group must allow traffic from the ALB on all ports.

## ECR

Private Docker image repository. Access is controlled through IAM (permission errors => policy).

AWS CLI v1 login command (may be asked at the exam):

- `$(aws ecr get-login --no-include-email --region ap-southeast-2)`

- `$()` means the output of what's in `()` should be executed.

AWS CLI v2 login command (pipe):

- `aws ecr get-login-password --region ap-southeast-2 | docker login --username AWS --password-stdin 1234567890.dkr.ecr.ap-southeast-2.amazonaws.com`

Docker Push & Pull:

- `docker push 1234567890.dkr.ecr.ap-southeast-2.amazonaws.com/demo:laest`
- `docker ppull 1234567890.dkr.ecr.ap-southeast-2.amazonaws.com/demo:laest`

### Fargate

When launching an ECS Cluster, we have to create our EC2 instances. If we need to scale, we need to add EC2 instances. With Fargate, it's all Serverless. We don't provision EC2 instances, we just create task definitions, and AWS will run our  containers for us. To scale, just increase the task number.

### ECS IAM Roles Deep Dive

- EC2 Instance Profile (attached at the instance level):
  - Used by the ECS agent
  - Makes API calls to ECS service
  - Send container logs to CloudWatch Logs
  - Pull Docker image from ECR

- ECS Task Role:
  - Allow each task to have a specific role
  - Use different roles for the different ECS Services you run
  - Task Role is defined in the task definition

### ECS Tasks Placement

When a task of type EC2 is launched, ECS must determine where to place it with the constraints of CPU, memory, and available port. Similarly, when a service scales in, ECS needs to determine which task to terminate.

To assist with this, you can define a task placement strategy and task placement constraints. 

Note: this is only for ECS with EC2, not for Fargate.

#### ECS Task Placement Process

When Amazon ECS places tasks, it uses the following process to select container instances:

1. Identity the instances that satisfy the CPU, memory and port requirements in the task definition
2. Identity the instances that satisfy the task placement constraints
3. Identity the instances that satisfy the task placement strategies
4. Select the instances for task placement

#### ECS Task Placement Strategies

- Binpack

  - Place tasks based on the least available amount of CPU or memory

  - This minimises the number of instances in use (cost savings)

  - ```json
    "placementStrategy": [
    	{
    		"field": "memory",
    		"type": "binpack"
    	}
    ]
    ```

- Random

  - ```json
    "placementStrategy": [
    	{
    		"type": "random"
    	}
    ]
    ```

- Spread

  - Place the task evenly based on the specified value

  - e.g. "instanceId", "attribute:ecs.availability-zone"

  - ```json
    "placementStrategy": [
    	{
    		"field": "attribute:ecs.availability-zone",
    		"type": "spread"
    	}
    ]
    ```

You can mis them together

```json
"placementStrategy": [
	{
		"field": "attribute:ecs.availability-zone",
		"type": "spread"
	},
	{
		"field": "instanceId",
		"type": "spread"
	}
]
```

```json
"placementStrategy": [
	{
		"field": "attribute:ecs.availability-zone",
		"type": "spread"
	},
	{
		"field": "memory",
		"type": "binpack"
	}
]
```

#### ECS Placement Constraints

- **distinctInstance:** place each task on a different container instance

  - ```json
    "placementConstraints": [
    	{
    		"type": "distinctInstance"
    	}
    ]
    ```

- **memberOf:** places task on instances that satisfy an expression

  - Uses the Cluster Query Language (advanced)

    - ```json
      "placementConstraints": [
      	{
      		"expression": "attribute:ecs.instance-type =~t2.*",
      		"type": "memberOf"
      	}
      ]
      ```

### ECS - Service Auto Scaling

CPU and RAM is tracked in CloudWatch at the ECS service level.

- **Target Tracking:** target a specific average CloudWatch metric
- **Step Scaling:** scale based on CloudWatch alarms
- **Scheduled Scaling:** based on predictable changes

ECS Service Scaling (task level) =/=  EC2 Auto Scaling (instance level). Fargate Auto Scaling is much easier to setup (because serverless).

#### ECS - Cluster Capacity Provider

A Capacity Provider is used in association with a cluster to determine the infrastructure that a task runs on.

- For ECS and Fargate users, the FARGATE and FARGATE_SPOT capacity providers are added automatically
- For Amazon ECS on EC2, you need to associate the capacity provider with an auto-scaling group

When you run a task or a service, you define a capacity provider strategy to prioritise in which provider to run. This allows the capacity provider to automatically provision infrastructure for you. 

