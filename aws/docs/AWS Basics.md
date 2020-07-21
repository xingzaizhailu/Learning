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

Provides authorization. Two facets:

- Specification: Defining access policies
- Enforcement: Evaluating policies

#####  Policy specification basics

- JSON-formatted docs
- Contain a statement (permissions) that specifies:
  - Which **action** a **principal** can/ can't (**Effect**) perform
  - Which **resource** can be accessed (under what **condition**)

###### Principal

An entity that is allowed or denied access to a resource indicated by an Amazon Resource Name (ARN).

With IAM policies, the principal element is implicit (i.e. the user, group, or role attached)

```json
<!-- Everyone (anonymous users) -->
"Principal":"AWS":"*.*"

<!-- Specific account or accounts -->
"Principal":{"AWS":"arn:aws:iam::123456789012:root"}
"Principal":{"AWS":"123456789012"}

<!-- Individual IAM user -->
"Principal":"AWS":"arn:aws:iam::123456789012:user/username"

<!-- Federated user (using web identity federation) -->
"Principal":{"Federated":"www.amazon.com"}
"Principal":{"Federated":"graph.facebook.com"}
"Principal":{"Federated":"accounts.google.com"}

<!-- Specific role -->
"Principal":{"AWS":"arn:aws:iam::123456789012:role/rolename"}

<!-- Specific service -->
"Principal":{"Service":"ec2.amazonaws.com"}
```

###### Action

Describes the type of access that should be allowed or denied. You can find these in the docs or use the policy editor to get a drop-down list. Statements must include either an `Action` or `NorAction` element.

```json
<!-- EC2 action -->
"Action":"ec2:StartInstance"

<!-- IAM action -->
"Action":"iam:ChangePassword"

<!-- S3 action -->
"Action":"s3：GetObject"

<!-- Specify multiple values for the action element -->
"Action":["sqs:SendMessage","sqs:ReceiveMessage"]

<!-- Use wildcards (* or ?) as part of the action name. This would cover Create/ Delete/List/Update -->
"Action":"iam:*AccessKey*"
```

###### NotAction

Lets you specify an exception to a list of actions. Could result in shorter policies than using `Action` and denying many actions.

```json
# Allow everything but IAM APIs
{
  "Version": "2012-10-17",
  "Statement": [ {
    "Effect": "Allow",
    "NotAction": "iam:*",  # This is not a deny. A user could still have a separate policy that grants IAM:*
    "Resource": "*"
  	}
  ]
}

# Or
{
  "Version": "2012-10-17",
  "Statement": [{
    "Effect": "Allow",
    "Action": "*",
    "Resource": "*"
  },
  {
    "Effect": "Deny",
    "Action": "iam:*",  # If you want to prevent the user from ever being able to call IAM APIs, use an explicit deny
    "Resource": "*"
  }]
}
```

###### Resources

The object or objects that are being requested. Statements must include either a `Resource` or a `NotResource` element.

```json
<!-- S3 Buckt -->

<!-- SQS queue -->
"Resource":"arn:aws:sqs:us-west-2:123456789012:queue1"

<!-- Multiple DynamoDB tables -->
"Resource":["arn:aws:dynamodb:us-west-2:123456789012:table/books_table",
            "arn:aws:dynamodb:us-west-2:123456789012:table/manazines_table"]

<!-- All EC2 instances for an account in a region -->
"Resource":"arn:aws:ec2:us-east-1:123456789012:instance/*"
```

###### Conditions

Optional criterial that must evaluate to rule for the policy to evaluate as true (e.g. restrict to an IP address range).

- Can contain multiple conditions
- Condition keys can contain multiple values
- Multiple values for one key are evaluated using logical `OR`
- Multiple conditions (or multiple keys in a single condition) are evaluated using logical `AND`

```
"Condition" : {
  "DateGreaterThan" : {"aws:CurrentTime" : "2020-07-20T12:00:00Z"},
  "DateLessThan" : {"aws:CurrentTime" : "2020-07-20T15:00:00Z"},
  "IpAddress" : {"aws:SourceIp" : ["192.0.2.0/24", "203.0.113.0/24"]}
}
```

##### Policy variables - `${aws:userid}`

Predefined variables based on service request context

- Existing keys (`aws:SourceIP`, `aws:CurrentTime.etc`)
- Principal-specific keys (`aws:username`, `aws:userid`, `aws:principaltype`)
- Provider-specific keys (`graph.facebook.com:id`, `www.amazon.com:user_id`)
- SAML keys (`saml:aud`, `saml:iss`)
- Service-specific variables

Benefits:

- Simplifies policy management
- Reduce the need for hard-coded user-specific policies

Use cases examples:

- Easily set up user access to "home folder" in S3
- Limit access to specific EC2 resources

#### Managing your policies

- IAM policies
  - Managed policies: apply only to users, groups and roles -- not resources 
  - Inline policies
- Resource-based policies

##### IAM policies

- Managed policies (newer way)
  - Can be attached to multiple users, groups, and roles
  - AWS managed polices: Created and maintained by AWS
  - Customer managed polices: Created and maintained by you
    - Up to 5K per policy
    - Up to 5 versions of a policy so you can roll back to a prior version
  - You can attach 10 managed policies per user, group or role
  - You can limit who can attach which managed policies
- Inline policies (older way)
  - You create and embed directly in a single user, group or role
  - Variable policy size (2K per user, 5K per group, 10K per role)

##### Resource-based policies

IAM policies live with: IAM users, groups, or roles.

```json
{
  "Statement":
  {
    "Effect": "Allow",
    "Principal": {"AWS": "123456789012"},  # Required for resource-based policies
    "Action”： “sqs:SendMessage",
    "Resource": "arn：aws:sqs:us-east-1:123456789012:queue1"
  }
}
```

#### Testing and debugging

- Authoring - Policy editor
  - Policy validation checks
    - JSON errors
    - Policy grammar errors
  - Policy formatting
    - On-demand
    - Autoformatting
- Testing policies - [Policy Simulator](https://policysim.aws.amazon.com/)
- Debugging - Encoded authorization message (EC2)

##### Decoding the EC2 authorization message

Additional information about the authorization status of a request. The decoded message includes:

- Whether the request was denied due to an explicit deny or absence of an explicit allow
- The principal who made the request
- The requested action
- The requested resource
- The values of condition keys in the context of the user's request

Requires permissions to call `sts:DecodeAuthorizationMessage`.

#### Policy enforcement

1. Decision starts at Deny
   1. AWS retrieves all policies associated with the user and resource
   2. Only policies that match the action and conditions are evaluated
2. Evaluate all applicable policies
3. Is there an explicit deny?
   1. If yes - Final decision = "deny"
4. Is there an Allow?
   1. If yes - Final decision = "allow"
5. By default, an implicit deny is returned.

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

