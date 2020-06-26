## EC2

### EC2 Instance Metadata

Allows EC2 instances to get its own information like IAM Role name (but not the IAM policy) by visiting `http://169.254.169.254/` inside the instance.

- Metadata: info about the EC2 instance
- Userdata: launch script of the EC2 instance

e.g. (always remember the last '/')

```
curl http://169.254.169.254/latest/meta-data/
```

### Security Group
Deny all inbound and allow all outbound traffic by default.

Can reference:
- IP address
- CIDR block
- Security group
- **Not**** DNS name

### EC2 User Data
Script runs only once when the instance started.

### EC2 Instance Types
- On Demand: Short workload, predictable pricing
    - Highest cost
    - Suitable for short-term and un-interrupted workloads
- Reserved: Long workloads (>= 1Y) - Up to 75% discount compared to On-demand
    - Convertible Reserved: Long workloads with flexible instances (54% discount)
    - Scheduled Reserved: e.g. 3 to 6 pm every Thursday
- Spot Instance: short workloads, for cheap, can lose instances (less reliable)
- Dedicated Instances:
    - No other customers will share your hardware (share within your account)
    - Hardware after stop/ start may move
- Dedicated Hosts: book an entire physical server, control instance placement

Great combo: Reserved instances for baseline + On-demand & Spot for peaks



### Elastic Network Interfaces (ENI)

Logical component in a VPC that represents a virtual network card.

The ENI can have the following attributes:

- Primary private IPv4, one or more secondary IPv4
- One Elastic IP per private IPv4
- One Elastic IP per public IPv4
- One or more security groups
- A MAC address

You can create ENI independently and attach them on the fly on EC2 instances for failover. But it's bound to a specific AZ.