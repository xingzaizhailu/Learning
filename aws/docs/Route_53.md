## Route 53

Route53 is a Managed DNS (Domain Name System).

**DNS** is a collection of rules and records which helps clients understand how to reach a server through its domain name. In AWS, the most common records are:

- A: hostname to IPv4 (Client get IP from DNS, then send HTTP requests to the returned IP.)
- AAAA: hostname to IPv6
- CNAME: hostname to hostname
- Alias: hostname to AWS resource

### Overview

You pay 0.5$ per month per hosted zone. Route 53 can use:

- public domain names you own (or buy)
  - e.g. app1.mypublicdomain.com
- Private domain names that can be resolved by  your instances in your VPCs.
  - e.g. app1.company.internal

Route 53 has advanced features such as:

- Load balancing (through DNS - also called client load balancing)
- Health checks (although limited...)
- Routing policy: simple, failover, geolocation, latency, weighted, multi value

### DNS Records TTL (Time to Live)

TTL is mandatory for each DNS record

High TTL (e.g. 24hr):

- less traffic on DNS
- Possibly outdated records

Low TTL (e.g. 60s):

- More traffic on DNS
- Records are outdated for less time
- Easy to change records

### CNAME vs Alias

AWS Resources (Load Balancer, CloutFront...) expose an AWS hostname: lb1-1234.us-east-2.elb.amazonaws.com and you want myapp.mydomain.com

**CNAME:**

- Points a hostname to any other hostname. (app.mydomain.com => balabla.anything.com)
- Only for **Non** ROOT DOMAIN ! (e.g. something.mydomain.com)

**Alias:**

- Points a hostname to an AWS Resource (app.mydomain.com => blabla.amazonaws.com)
- Works for ROOT DOMAIN and NON ROOT DOMAIN (aka mydomain.com)
- Free of charge
- Native health check

### Health Checks

Have X health checks failed => unhealthy (default 3)

After X health checks passed => health (default 3)

Default Health Check Interval: 30s (can set to 10s - higher cost)

About 15 health checkers will check the endpoint health. (=> 1 request every 2 seconds on average)

Can have HTTP, TCP and HTTPS health checks (no SSL verification)

Possibility of integrating the health check with CloudWatch

Health checks can be linked to Route53 DNS queries.

### Routing Policy

#### Simple

Use when you need to redirect to a single resource. If multiple values are returned, a random one is chosen by the client. You can't attach health checks to simple routing policy.

#### Weighted

Control the % of the requests that go to specific endpoint.

- Helpful to test 1% of traffic on new app version for example. 
- Helpful to split traffic between two regions
- Can be associated with Health Checks

#### Latency - One of the most useful

Redirect to the server that has the least latency close to us. Super helpful when latency of users is a priority.   Latency is evaluated in terms of user to designated AWS Region.

#### Failover Routing Policy

Mandatory to do health check on primary instance. If the primary instance failed, route to secondary instance (disaster recovery).

#### Geolocation

Different from Latency based, this is routing based on user location. We specify that traffic from the UK should go to this specific IP.

A "default" policy should be created in case there's no match on location.

#### Multi Value

Use when routing traffic to multiple resources. Want to associate a Route 53 health checks with records (unhealthy resources will not be returned). 

Up to 8 healthy records are returned for each Multi Value query. Multi Value is not a substitute for having an ELB.