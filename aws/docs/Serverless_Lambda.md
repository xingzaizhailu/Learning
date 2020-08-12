## Serverless

Serverless is a new paradigm in which the developers don't have to manage servers anymore. They just deploy code/ functions.

Initially, Serverless == FaaS (Function as a Service). Serverless was pioneered by AWS Lambda but now also includes anything that's managed "databases, messaging, storage, etc.".

Serverless doesn't mean there are no servers, it means you don't manage/ provision/ see them.

### Serverless in AWS

- AWS Lambda
- DynamoDB
- AWS Cognito
- AWS API Gateway
- Amazon S3
- AWS SNS & SQS
- AWS Kinesis Data Firehose
- Aurora Serverless
- Step Functions
- Fargate

## AWS Lambda

### Why AWS Lambda

| Amazon EC2                                        | Amazon Lambda                            |
| ------------------------------------------------- | ---------------------------------------- |
| Virtual Servers in the Cloud                      | Virtual functions - no servers to manage |
| Limited by RAM and CPU                            | Limited by time - short executions       |
| Continuously running                              | Run on-demand                            |
| Scaling means intervention to add/ remove servers | Scaling in automated                     |

### Benefits of AWS Lambda

- [Easy Pricing](https://aws.amazon.com/lambda/pricing):
  - Pay per request and compute time (in increment of 100 ms)
    - Free tier of 1,000,000 AWS Lambda requests and 400,000 GBs of compute time
    - $0.20 per 1million requests thereafter
    - $1.0 for 600,000 GB-seconds (600,000s if function is 1GB RAM)  
- Integrated with the whole AWS suite of services
  - API Gateway, Kinesis, DynamoDB, S3, CloudFront, CloudWatch Events EventBridge, CloudWatch Logs, SNS, SQS, Cognito...
- Integrated with many programming languages
  - Node.js, Python, Java, C# (.NET Core), Golang, Powershell, Ruby, Custom Runtime API (community supported, e.g. Rust)
- Easy monitoring through AWS CloudWatch
- Easy to get resources per functions (up to 3GB of RAM)
- Increasing RAM will also improve CPU and network

### Lambda categories

- Synchronous Invocation
- Asynchronous Invocation
- Event Source Mapping

### Lambda - Synchronous Invocations

Synchronous (invoked from CLI, SDK, API Gateway, Application Load Balancer):

- Result is returned right away
- Error handling must happen client side (retries, exponential backoff, etc...)

#### Synchronous Invocations Services

- User Invoked
  - Elastic Load Balancing (Application Load Balancer)
  - Amazon API Gateway
  - Amazon CloudFront (Lambda@Edge)
  - Amazon S3 Batch
- Service Invoked
  - Amazon Cognito
  - AWS Step Functions
- Other services
  - Amazon Lex
  - Amazon Alexa
  - Amazon Kinesis Data Firehose

#### From CLI

```bash
$ aws lambda invoke --function-name func-name --cli-binary-format raw-in-base64-out --payload '{"key1": "value1", "key2": "value2", "key3": "value3"}' --region au-southeast-2 response.json
```



#### Lambda Integration with ALB

To expose a Lambda function as an HTTP(S) endpoint, you can use the Application Load Balancer (or an API Gateway). The Lambda function must be registered in a target group.

Client <--HTTP/HTTPS--> ALB <--Invoke Sync--> Lambda in a Target Group

From ALB to Lambda: HTTP to JSON, From Lambda to ALB: JSON to HTTP.

##### ALB Multi-Header Values

ALB can support multi header values (ALB settings). When it is enabled, HTTP headers and query string parameters that are sent with multiple values are shown as arrays within the AWS Lambda event and response objects.

For example, in http://example.com/path?name=foo&name=bar, parameter `name` has multiple value. It will be converted into `"queryStringparameters":{"name": ["foo", "bar"]}` from ALB to Lambda.

#### Lambda@Edge

You have deployed a CDN using CloudFront. What if you wanted to run a global AWS Lambda alongside? Or how to implement request filtering before reaching your application? You can use Lambda@Edge: deploy Lambda functions alongside your CloudFront CDN. 

- Build more responsive applications
- You don't manage servers
- Customise the CDN content
- Pay only for what you use

You can use Lambda@Edge  to change CloudFront requests and responses:

- After CloudFront receives a request from a viewer (viewer request)

- Before CloudFront forwards the request to the origin (origin request)
- After CloudFront receives the response from the origin (origin response)
- Before CloudFront forwards the response to the viewer (viewer response)

You can also generate responses to viewers without ever sending the request to the origin.

##### Use Cases

- Website Security and Privacy

- Dynamic Web Application at the Edge

- Search Engine Optimisation (SEO)
- Intelligently Route Across Origins and Data Centers
- Bot Mitigation at the Edge
- Real-time Image Transformation
- A/B Testing
- User Authentication and Authorisation
- User Prioritisation
- User Tracking and Analytics

### Lambda Asynchronous Invocations

From S3, SNS, CloudWatch Events... The events are placed in an `Event Queue`. Lambda attempts to retry on errors:

- 3 tries total
- 1 minute wait after 1st, then 2 minutes wait

Make sure the processing is idempotent (in case of retries). If the function is retried, you will see duplicate logs entries in CloudWatch Logs. Can define a DLD (Dead-Letter Queue) - SNS or SQS - for failed processing (need correct IAM permissions).

![](/Users/leo/workspace/learning/aws/docs/images/Lambda_Asynchronous_Invocations.png)

Asynchronous invocations allows you to speed up the processing if you don't need to wait for the result (ex. you need 1000 files processes).

#### Asynchronous Invocations Services

**S3, SNS, CloudWatch Events/ EventBridge**, CodeCommit (new branch, tag, push), CodePipeline (invoke a lambda function during the pipeline, Lambda must callback)

Others: CloudWatch Logs, Simple Email Service, CloudFormation, Config, IoT, IoT Events

##### From CLI

```bash
$ aws lambda invoke --function-name func-name --cli-binary-format raw-in-base64-out --payload '{"key1": "value1", "key2": "value2", "key3": "value3"}' --invocation-type Event --region au-southeast-2 response.json
```

`--invocation-type Event` is the difference.

### Lambda - Event Source mapping

Applies to Kinesis Data Streams, SQS & SQS FIFO queue and DynamoDB Streams. And in this case, Lambda is invoked synchronously.

Common denominator: records need to be polled from the source.

Two categories:

- Streams
- Queue

#### Streams & Lambda: (Kinesis & DynamoDB)

An event source mapping creates an iterator for each shard, processes items in order at shard level. Start with new items, from the beginning or from time stamp.

- Processed items aren't remove from the stream (other consumers can read them)

- When Low traffic: use batch window to accumulate records before processing.
- When High traffic: You can process multiple batches in parallel
  - Up to 10 batches per shard
  - in-order processing is still guaranteed for each partition key

##### Streams & Lambda - Error Handling

By default, if your function returns an error, the entire batch is reprocessed until the function succeeds, or the items in the batch expire. To ensure in-order processing, processing for the affected shard is paused until the error is resolved. You can configure the event source mapping to:

- discard old events
  - Discarded events can go to a Destination
- restrict the number of retries
- split the batch on error (to work around Lambda timeout issues)

#### Queue & Lambda: (SQS & SQS FIFO)

Lambda scales up to process a standard queue as quickly as possible. Lambda also supports in-order processing for FIFO queues, scaling up to the number of active message groups.

Event Source Mapping will poll SQS (Long Polling).

- Specify batch size (1-10 messages).
- Recommended: Set the queue visibility timeout to 6x the timeout of your Lambda function
- To use a DLQ
  - Set-up on the SQS queue, not Lambda (DLQ for Lambda only works for async invocations)
  - Or use a Lambda destination for failures

When an error occurs, batches are returned to the queue as individual items and might be processed in a different grouping than the original batch. Occasionally, the event source mapping receive the same item from the queue twice, even if no function error occurred. Finally, Lambda deletes items from the queue after they're processed successfully. You can configure the source queue to send items to a dead-letter queue if they can't be processed.  

#### Lambda Event Mapper Scaling

- Kinesis Data Streams & DynamoDB Streams
  - One Lambda invocation per stream shard
  - If parallelisation, up to 10 batches processed per shard simultaneously
- SQS Stand
  - Lambda adds 60 more instances per minute to scale up
  - Up to 1000 batches of messages processed sinultaneously
- SQS FIFO
  - Messages with the same GroupID will be processed in order
  - The Lambda function scales to the number of active message groups

### Lambda Destinations

When doing asynchronous invocations or event mapper, it is hard to see if it has failed or succeeded.

[**Asynchronous invocations:**](https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html) can define destinations for successful and failed event as

- SQS/ SNS/ Lambda or EventBridge Bus

Note: AWS recommends you use destinations instead of DLQ now, because it has more options (but both can be used at the same time)

![](/Users/leo/workspace/learning/aws/docs/images/Lambda_Destination_for_Sync_invokations.png)

[**Event Source mapping:**](https://docs.aws.amazon.com/lambda/latest/dg/invocation-eventsourcemapping.html) for discarded event batches:

- SQS/ SNS

Note: you can send events to a DLQ directly from SQS 

### Lambda Permissions

#### Execution Role (IAM Role)

Grants the Lambda function permissions to AWS services/ resources.

When you use an event source mapping to invoke your function, Lambda uses the execution role to read event data. 

Best practice: create on Lambda Execution Role per function.

#### Resource Based Policies

Use resource-based policies to give other accounts and AWS services permission to use your Lambda resources. An IAM principal can access Lambda:

- if the IAM policy attached to the principal authorises it (e.g. user access)
- Or if the resource-based policy authorises (e.g. service access)

### Lambda Environment Variables

Adjust the function behaviour without updating code. Lambda Services adds its own system environment variables as well.

- Helpful to store secrets (encrypted by KMS)
- Secrets can be encrypted by the Lambda service key, or your own CMK

### Lambda monitoring

- CloudWatch Logs:
  - AWS lambda execution logs are stored in AWS CloudWatch Logs
  - Make sure your AWS Lambda function has an execution role with an IAM policy that authrises writes to CloudWatch Logs
- CloudWatch Metrics:
  - AWS Lambda metrics are displayed in AWS CloudWatch Metrics
  - Invocations, Duration, Concurrent Executions
  - Error count, Success Rates, Throttles
  - Async Delivery Failures
  - Iterator Age (Kinesis & DynamoDB Streams) 

###  Lambda Tracing with  X-Ray

- Enable in Lambda configuration (Active Tracing).

- Runs the X-Ray daemon for you.
- Use AWS X-Ray SDK in Code.
- Ensure Lambda Function has a correct IAM Execution Role
  - The managed policy is called AWSXRayDaemonWriteAccess
- Environment variables to communicate with X-Ray
  - \_X\_AMZN\_TRACE\_ID: contains the tracing header
  - AWS\_XRAY\_CONTEXT\_MISSING: by default, `LOG_ERROR`
  - AWS\_XRAY\_DAEMON\_ADDRESS: the X-Ray Daemon `IP_ADDRESS:PORT`

### Lambda in VPC

By default, your Lambda function is launched outside your own VPC (in an AWS-owned VPC). Therefore it cannot access resources in your VPC (RDS, ElastiCache, internal ELB...). In order to do that, you must define the VPC ID, the Subnets and the Security Groups.

- Lambda will create an ENI (Elastic Network Interface) in your subnets
- AWSLambdaVPCAccessExecutionRole 

#### Lambda in VPC - Internet Access

A Lambda function in your VPC does not have internet access. Deploying a Lambda function in a public subnet does not give it internet access or a public IP. But you can deploy a Lambda function in a private subnet gives it internet access if you have a NAT Gateway/ Instance. You can use VPC endpoints to privately access AWS services without a NAT.  