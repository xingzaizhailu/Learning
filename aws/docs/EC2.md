## EC2

### EC2 Instance Metadata

Allows EC2 instances to get its own information like IAM Role name (but not the IAM policy) by visiting `http://169.254.169.254/` inside the instance.

- Metadata: info about the EC2 instance
- Userdata: launch script of the EC2 instance

e.g. (always remember the last '/')

```
curl http://169.254.169.254/latest/meta-data/
```

