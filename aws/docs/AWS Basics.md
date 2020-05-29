## AWS Basics

### Essential Concepts

- [**Region**](https://aws.amazon.com/about-aws/global-infrastructure/regions_az/)
  - A cluster of data Centers. Most AWS services are region-scoped ([Regional services availability](https://aws.amazon.com/about-aws/global-infrastructure/regional-product-services/)).
  - Each region has many availability zones.
- [**Availability Zone**](https://aws.amazon.com/about-aws/global-infrastructure/regions_az/)
  - One or more discrete data centres with redundant power, networking and connectivity. 
  - They're separate from each other => isolated from disaster.
  - Connected with high bandwidth, ultra-low latency networking.

### Identity and Access Management (IAM)

Your whole AWS security is there:

- **Users** - Usually a physical person. Must be created with proper permissions
  - Root account should never be used and shared
- **Groups** - by Functions (admins, devops) or teams (engineering, design)

- **Roles** - Internal usage within AWS resources
- **Policies** (JSON Docs) - Define what each of the above can and cannot do

#### IAM intro

- IAM has a **global** view
- Permissions are governed by _Policies_
- MFA (Multi Factor Authentication) can be setup
- IAM has predefined "'managed policies"
- Best give users the minimal amount of permissions they need to perform their job

#### IAM Federation

#### Policy

[Policy Simulator](https://policysim.aws.amazon.com/): A tool to test policies

### AWS CLI

#### Configure

```
aws configure [--profile profile_name]
```

If default region not specified, then `us-east-1` will be chosen.

#### Dry Runs

Use `--dry-run` option (put it first) when you just want to make sure you have the permission but don't want to actually run the commands.

#### STS Decode

```
aws sts decode-authorization-message --encoded-message [value]
```

#### MFA with CLI

1. Create a temporary session: `aws sts get-session-token --serial-number arn-of-the-mfa-device --token-code code-from-token --duration-seconds 3600`
2. `aws configure --profile mfa` with the return from step 1
3. Then manually add _SessionToken_ to the credential file.

#### AWS CLI Credentials Provider Chain

The CLI will look for credentials in this order:

1. Command line options: like `--region`, `--output` and `--profile`
2. Environment variables: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`
3. CLI credential file
4. CLI configuration file: the `config` file
5. Container credentials: for ECS tasks
6. Instance profile credentials: for EC2 instance profiles

##### AWS SDK Default Credentials Provider Chain

The Java SDK will look for credentials in this order:

1. Environment variables
2. Java system properties
3. Credential file
4. ECS container credentials
5. EC2 instance profile credentials

#### AWS Credential Best Practices

- Best practice is for credentials to be inherited from the credentials chain
  - If using working within AWS, use IAM Roles
    - EC2 Instance Roles for EC2 instances
    - ECS Roles for ECS tasks
    - Lambda Roles for Lambda functions
  - If working outside of AWS, use environment variables / named profiles
- Never ever store AWS credentials in you code!

### AWS SDK (Software Development Kit)

AWS CLI uses the Python SDK (boto3)

### AWS Limits (Quotas)

- API Rate Limits
  - e.g. 'DescribeInstances' API for EC2 --> 100 calls/ second, 'GetObject' on S3 --> 5500 'GET'/ seconds
  - For intermittent errors: implement Exponential Backoff
  - For consistent errors: request an API throttling limit increase
- Service Quotas (Service Limits)
  - e.g. Standard instances: 1152 vCPU
  - Request a service limit increase by **openning a ticket**
  - Request a service quota increase by using the **Service Quotas API**

#### Exponential Backoff

Retry mechanism included in SDK API calls. It doubles the retry interval every time.

### Signing AWS API requests

When you call the AWS HTTP API, you sign the request, using your AWS credentials (access key & secret key), so that AWS can identify you.

If you use the SDK or CLI, the HTTP requests are signed for you. Otherwise you shold sign an AWS HTTP request using _Signature v4_ (Sigv4) by yourself.

