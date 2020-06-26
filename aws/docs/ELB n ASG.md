## Load Balancer

Load balancers are servers that forward internet traffic to multiple servers downstream.

### Why use a load balancer

- Spread load across multiple downstream instances
- Expose a single point of access (**DNS**) to your application
- Seamlessly handle failure of downstream instances
- Do regular health checks to your instances
  - done on a port and a route (eg. `/health`)
- Provide SSL termination (HTTPS) for your websites
- Enforce stickiness with cookies
- High availability across zones
- Separate public traffic from private traffic

### AWS Load Balancer

- Classic Load Balancer (v1, 2009)
  - HTTP, HTTPS, TCP
- Application Load Balancer (v2, 2016)
  - HTTP, HTTPS, WebSocket
- Network Load Balancer (v2, 2017)
  - TCP, TLS (secure TCP) & UDP

Overall, newer versions (v2 ) recommended.

#### LB Security Groups

LB Security Group: Allows HTTP (80) and HTTPS (443) from all source.

Application Security Group: Only allow HTTP from LB.

#### LB Good to know

- LB can scale but not instantaneously - contact AWS for a "warm-up"
- Troubleshooting
  - 4xx - Client induced errors
  - 5xx - application induced errors
  - LB Errors 503 - at capacity or no registered target
- Monitoring
  - ELB access logs will log all access requests
  - CloudWatch Metrics will give you aggregate statistics

### Classic LB (v1)

- Supports TCP (Layer 4), HTTP & HTTPS (Layer 7)
- Health checks are TCP or HTTP based
- Fixed hostname: XXX.region.elb.amazonaws.com

### Application LB (V2)

Application LB is layer 7 (HTTP). Expose a static **DNS** (URL).

- Load balancing to multiple HTTP applications across machines (target groups)

- Load balancing to multiple applications on the same machine (eg. containers)
- Routing tables to diff target groups
  - Routing based on **path** in URL (eg.com/users & eg.com/posts)
  - Routing based on **hostname** in URL (one.eg.com & other.eg.com)
  - Routing based on **Query String**, Headers (eg.com/users?id=123&order=false)

ALB is a great fit for micro services & container-based apps and it has a port mapping feature to redirect to a dynamic port in ECS

#### ALB Target Groups

- EC2 instances (can be managed by ASG) - HTTP
- EC2 tasks (managed by ECS itself) - HTTP
- Lambda functions - HTTP request is translated into a JSON event
- IP Address - must be private IPs

#### ALB Good to know

- ALB can route to multiple target groups
- Health checks are done at target group level
- Fixed hostname (XXX.region.elb.amazonaws.com)
- The application servers don't see the IP of the client directly
  - The true IP of the client is inserted in the header X-Forwarded-For
  - We can get Port (X-Forwarded-Port) and proto (X-Forwarded-Proto)

### Network Load Balancer (v2)

Network load balancers (Layer 4) allow to:

- Forward TCP & UDP traffic to your instances
- Handle millions of request per seconds
- Less latency ~100ms (vs 400ms for ALB)
  - NLB are used for extreme **performance**, TCP or UDP traffic

NLB has one **static IP** per AZ, and supports assigning Elastic IP (helpful for whitelisting specific IP).

### LB Stickiness

The same client is always redirected to the same instance behind a load balancer. This works for Classic LBs & Application LBs. The "cookie" used for stickiness has an expiration date you control.

Use case: make sure the user doesn't lose his session data.

Enabling stickiness through target group and this may bring imbalance to the load over the backend EC2 instances.

### Cross-Zone Load Balancing

Making each load balancer instance distributes evenly across all registered instances in all AZ.

For Classic LB:

- Disabled by default
- No charges for inter AZ data if enabled

For Application LB:

- Always on (can't be disabled)
- No charges for inter AZ data

Network LB:

- Disabled by default
- Extra payment for inter AZ data

### Elastic LB - SSL Certificates

An SSL (Secure Sockets Layer) Certificate allows traffic between your clients and your load balancer to be encrypted in transit (in-flight encryption).

TLS (Transport Layer Security) is a newer version.

Public SSL certificates are issued by Certificate Authorities (CA) and they must be renewed after expiration.

The load balancer uses an X.509 certificate (SSL/TLS server certificate)

You can manage certificates using ACM (AWS Certificate Manager)

You can create upload your own certificates alternatively

HTTPS listener:

- You must specify a default certificate
- you can add an optional list of certs to support multiple domains
- Clients can use SNI (Server Name Indication) to specify the hostname they reach
- Ability to specify a security policy to support older versions of SSL/ TLS (legacy clients)

#### SSL - Server Name Indication (SNI)

SNI solves the problem of loading multiple SSL certificates onto one web server (to serve multiple websites)

1. It's a "newer" protocol and requires the client to indicate the hostname of the target server in the initial SSL handshake.

2. The server will then find the correct certificate or return the default one.

**Note:** Only works for ALB & NLB (newer generation), CloudFront

### ELB - Connection Draining

- Feature naming
  - CLB: Connection draining
  - Target Group: Deregistration Delay (for ALB & NLB)
- Time to complete "in-flight requests" while the instance is de-registering or unhealthy
- Stops sending new requests to the instance which is de-registering.
- Between 1 to 3600 seconds, default is 300 seconds. Can be disabled (set value to 0)
- Set to a low value if your requests are short

## Auto Scaling Group

#### Goals

- Scale out (add EC2 instances) to match an increased load
- Scale in (remove EC2 instances) to match a decreased load
- Ensure we have a minimum and a maximum number of machines running
- Automatically Register new instances to a load balancer

#### Attributes

- A launch configuration:
  - AMI + Instance Type
  - EC2 User Data
  - EBS Volumes
  - Security Groups
  - SSH Key Pair
- Min/ Max Size, Initial Capacity
- Network + Subnet Info
- LB info
- Scaling Policies

### Auto Scaling Alarms

An Alarm monitors a metric (such as Average CPU)

Metrics are computed for overall ASG instances

### Auto Scaling New Rules

- Target Average CPU Usage
- Number of requests on the ELB per instance
- Average Network In
- Average Network Out

### Auto Scaling Custom Metric

1. Send custom metric from app on EC2 to CloudWatch (PutMetric API)
2. Create CloudWatch Alarm to react to low/ high values
3. Use the CloudWatch Alarm as the scaling policy for ASG

### Auto Scaling Groups - Scaling Policies

- Target Tracking Scaling
  - Easiest to set-up
  - e.g. ASG CPU to stay at around 40%
- Simple / Step Scaling
  - When a CloudWatch alarm is triggered (e.g. CPU > 70%), add 2 units
  - When a CloudWatch alarm is triggered (e.g. CPU < 30%), remove 1
- Scheduled Actions
  - Anticipate a scaling based on known usage patterns
  - e.g. increase the min capacity to 10 at 5 pm on Fridays

### Auto Scaling Groups - Scaling Cool-downs

The cool-down period helps ensure that your Auto Scaling Group doesn't launch or terminate additional instances before the previous scaling activity takes effect.

In addition to default cool-down for ASG, we can create cool-downs that apply to a specific simple scaling policy. A scaling-specific cool-down period overrides the default cool-down period.

One common use for scaling-specific cool-downs is with a scale-in policy -- a policy that terminates instances based on a specific criteria or metric. Because this policy terminates instances, Amazon EC2 Auto Scaling needs less time to determine whether to terminate additional instances. If the default cool-down period of 300 seconds is too long - you can reduce costs by applying a scaling-specific cool-down period of 180 seconds to the scale-in policy. If your application is scaling up and down multiple times each hour, modify the ASG cool-down timers and the CloudWatch Alarm Period that triggers the scale in.

