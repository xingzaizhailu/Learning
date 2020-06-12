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
- Reserved: Long workloads (>= 1Y)
    - Convertible Reserved: Long workloads with flexible insatnces
    - Scheduled Reserved: e.g. 3 to 6 pm every Thursday
- Spot Instance: short workloads, for cheap, can lose instances (less reliable)
- Dedicated Instances: no other customers will share your hardware
- Dedicated Hosts: book an entire physical server, control instance placement
