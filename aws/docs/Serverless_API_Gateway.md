## API Gateway

>  Client <=REST API=> API Gateway <=PROXY REQUEST=> Lambda <=CRUD=> DynamoDB

- No infrastructure to manage.
- Support for the WebSocket Protocol.
- Handle API versioning (v1, v2...)
- Handle different environments (dev, test, prod...)
- Handle security (Authentication and authorisation)
- Create API keys, handle request throttling
- Swagger/ Open API import to quickly define APIs
- Transform and validate requests and responses
- Generate SDK and API specifications
- Cache API responses

### API Gateway - Categories

- HTTP APIs
  - Low-latency, cost-effective AWS Lambda proxy, HTTP proxy API and private integration (no data mapping)
  - Support OIDC and OAuth 2.0 authorisation and build-in support for CORS
  - No usage plans and API keys
- Rest APIs
  - All features (except Native OpenID Connect/ OAuth 2.0)
- WebSocket
  - Two-way interactive communication between a user's browser and a server
  - Server can push information to the client
  - This is enables stateful application use cases
  - WebSocket APIs are often used in real-time applications such as chat apps, collaboration platforms, multiplayer games, and financial trading platforms.
  - Works with AWS Services (Lambda, DynamoDB) or HTTP endpoints

### API Gateway - Integrations High Level

- Lambda Function
  - Invoke Lambda function
  - Easy way to expose REST API backed by AWS Lambda
- HTTP
  - Expose HTTP endpoints in the backend
  - e.g. internal HTTP API on premise, Application Load Balancer...
  - Why? Add rate limiting, caching, user, authentications, API keys, etc...
- AWS Service
  - Expose any AWS API through the API Gateway
  - e.g. start an AWS Step Function workflow, post a message to SQS
  - Why? Add authentication, deploy public, rate control...

### API Gateway - Endpoint Types

- Edge-Optimized (default): For global clients
  - Requests are routed through the CloudFront Edge locations (improves latency)
  - The API Gateway still lives in only one region
- Regional:
  - For clients within the same region
  - Coud manually combine with CloudFront (more control over the caching strategies and the distribution)
- Private:
  - Can only be accessed from your VPC using an interface VPC endpoint (ENI)
  - Use a resource policy to define access

### API Gateway - Deployment Stages

Making changes in the API Gateway doesn't mean they're effective. You need to make a "deployment" for them to be in effect. Changes are deployed to "Stages" (as many as you want, use the naming you like).

- Each stage has its own configuration parameters.
- Stages can be rolled back as a history of deployment is kept.

#### API Gateway - Stage Variables

Stage variables are like environment variables for API Gateway. Use them to change often changing configuration values. They can be used in:

- Lambda function ARN
- HTTP Endpoint
- Parameter mapping templates

Use cases:

- Configure HTTP endpoints your stages talk to (dev, test, prod...)
- Pass configuration parameters to AWS Lambda through mapping templates

Stage variables are passed to the "context" object in AWS Lambda.

#### API Gateway Stage Variables & Lambda Aliases

We create a stage variable to indicate the corresponding Lambda alias. Our API Gateway will automatically invoke the right Lambda function.

### API Gateway - Canary Deployment

Choose the % of traffic the canary channel receives. This is blu/ green deployment with AWS Lambda & API Gateway.

- Possibility to enable canary deployments for any stage (usually prod).
- Metrics & Logs are separate (for better monitoring)
- Possibility to override stage variables for canary

After making sure every things looks good, do "promote canary" to move to newer version.

### API Gateway - Integration Types

- MOCK
  - API Gateway returns a response without sending the request to the backend
- HTTP / AWS (Lambda & AWS Services)
  - you must configure both the integration request and integration response
  - Setup data mapping using **mapping templates** for the request & response
- AWS_PROXY (Lambda Proxy)
  - incoming request from the client is the input to Lambda
  - The function is responsible for the logic of request/ response
  - No mapping template, headers, query string parameters are passed as arguments
- HTTP_PROXY
  - No mapping template
  - The HTTP request is passed to the backend
  - The HTTP response from the backend is forwarded by API Gateway

#### Mapping Templates (AWS & HTTP Integration)

Mapping templates can be used to

- modify request/ response
- rename/ modify query string parameters.
- modify body content
- add headers
- uses Velocity Template Language (VTL): for loop, if...
- filter ourput results (remove unnecessary data)

##### Mapping example: JSON to XML with SOAP

SOAP API are XML based, whereas REST API are JSON absed.

>  Clien <= RESTful, JSON Payload => API Gateway + Mapping Template <= XML Payload => SOAP API

In this case, API Gateway should:

- Extract data from the request: either path, payload or header
- Build SOAP message based on request data (mapping template)
- Call SOAP service and receive XML response
- Transform XML response to desired format (like JSON), and response to the user

### AWS API Gateway Swagger/ Open API spec

Common way of defining REST APIs, using API definition as code.

- Import existing Swagger/ OpenAPI 3.0 spec to API Gateway
  - Method
  - Method Request
  - Integration Request
  - Method response
  - \+ AWS extensions for API gateway and setup every single option
- Can export current API as Swagger/ OpenAPI spec
- Swagger can be written in YAML or JSON
- Using Swagger we can generate SDK for our applications

### API Gateway Caching

#### Caching API responses

Caching reduces the number of calls made to the backend. Default TTL (time to live) is 300 seconds (min: 0s, max: 3600s).

- Caches are defined per stage
- Possible to override cache settings per method
- Cache encryption option
- Cache capacity between 0.5 GB to 237 GB
- Cache is expensive, make sense in production, may not make sense in dev/ test

#### API Gateway Cache Invalidation

Able to flush the entire cache (invalidate it) immediately.

- Clients can invalidate the cache with header:
  - Cache-Control: max-age=0 (with proper IAM authorisation)

**Note:** If you don't impose an InvalidateCache policy (or choose the Require authorisation check box in the console), any client can invalidate the API cache.

### API Gateway - Usage Plans & API Keys

If you want to make an API available as an offering ($) to your customers.

- Usage Plan:
  - who can access one or more deployed API stages and methods
  - how much and how fast they can access them
  - use API keys to identify API clients and meter access
  - configure throttling limits and quota limits that are enforced on individual client
- API Keys:
  - alphanumeric string values to distribute to your customers
  - Can use with usage plans to control access
  - Throttling limits are applied to the API Keys
  - Quotas limits is the overall number of maximum requests

#### API Gateway - Correct Order for API Keys

To configure a usage plan:

1. Create one or more APIs, configure the methods to require an API key, and deploy the APIs to stages
2. Generate or import API keys to distribute to application developers (your customers) who will be using you API
3. Create the usage plan with de desired throttle and quota limits
4. Associate API stages and API keys with the usage plan

Callers of the API must supply an assigned API key in the x-api-key header in requests to the API.

### API Gateway Monitoring

#### Logging and Tracing

- CloudWatch Logs
  -  Enable CloudWatch logging at the Stage level (with log Level)
  - Can override settings on a per API basis (ex: ERROR, DEBUG, INFO)
  - Log contains information about request/ response body
- X-Ray
  - Enable tracing to get extra information about requests in API Gateway
  - X-Ray API Gateway + AWS Lambda gives you the full picture

#### API Gateway - CloudWatch Metrics

Metrics are by stage, possibility to enable detailed metrics:

- CacheHitCount & CacheMissCount: efficiency of the cache
- Count: The total number API requests in a given period
- IntegrationLatency: The time between when API Gateway relays a request to the backend and when it receives a response from the backend.
- Latency: The time between when API Gateway receives a request from a client and when it returns a response to the client.
  - The latency includes the integration latency and other API Gateway overhead
  - Max timeout is 29 seconds
- 4XXError (client-side) & 5XXError (server-side)

### API Gateway Throttling

Account Limit

- API Gateway throttles requests at 10000 rps across all API
- Soft limit that can be increased upon request

In case of throttling => 429 Too Many Requests (retriable error). Just like Lambda Concurrency, one API that is overloaded, if not limited, can cause the other APIs to be throttled.

- Can set Stage limit & Method limits to improve performance.
- Or you can define Usage Plans to throttle per customer

### API Gateway - Errors

- 4xx means Client errors
  - 400: Bad Request
  - 403: Access Denied, WAF filtered
  - 429: Quota exceeded, Throttle
- 5xx means Server errors
  - 502: Bad Gateway Exception, usually for an incompatible output returned from a Lambda proxy integration backend and occasionally for out-of-order invocations due to heavy loads
  - 503: Service Unavailable Exception
  - 504: Integration Failure - ex Endpoint Request Time-out Exception,
    - API Gateway requests time out after 29 second maximum

### API Gateway CORS

CORS must be enabled when you receive API calls from another domain. The options pre-flight request must contain the following headers:

- Access-Control-Allow-Methods
- Access-Control-Allow-Headers
- Access-Control-Allow-Origin

CORS can be enabled through the console.

###  API Gateway - Security

#### IAM Permissions

- Create an IAM policy authorisation and attach to User/ Role.
- Authentication = IAM  |  Authorisation = IAM Policy
- Good to provide access within AWS (EC2, Lambda, IAM users...)
- Leverage "Sig v4" capability where IAM credential are in headers

#### Resource Policies

Resource policies (similar to Lambda Resource Policy)

- Allow for Cross Account Access (combined with IAM Security)
- Allow for a specific source IP address
- Allow for a VPC Endpoint

``` json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Principal": {
        "AWS": [
          "arn:aws:iam::account-id-2:user/Alice",
          "account-id-2"
        ]
      },
      "Action": "execute-api:Invoke",
      "Recourse": [
        "arn:aws:execute-api:region:account-id-1:api-id/stage/GET/pets"
      ]
    }
  ]
}
```

#### Cognito User Pools

Cognito fully manager user lifecycle, token expires automatically. API gateway verifies identity automatically from AWS Cognito.

- No custom implementation required
- Authentication = Cognito User Pools  |  Authorisation = API Gateway Methods

Client retrieve a Authenticate token from Cognito User Pools and then send request to the REST API + Pass Token to the API Gateway. API Gateway will evaluate Cognito Token with Cognito User Pools. And finally access Lambda if verified success.

#### Lambda Authoriser (formerly Custom Authorisers)

Most flexible way.

- **Token-based authoriser** (bearer token) - ex JWT (JSON Web Token) or Oauth
- A **request parameter-based** Lambda authoriser (headers, query string, stage var)
- Lambda must return an IAM policy for the user, result policy is cached
- Authentication = External  |  Authorisation = Lambda function

Client first retrieve token from a 3rd party Authentication system and send the request with Bearer token or request params to the API Gateway. API gateway will send the context with token or request params to the Lambda Authoriser. After Lambda Authoriser checked the info with the 3rd party Authentication system, it will generate and return IAM Principal with IAM Policy to the API Gateway. And the API Gateway will save them into the Policy Cache and talk to the Lambda Backend.

#### API Gateway - Security Summary

- IAM
  - Great for users/ roles already within your AWS account + resource policy for cross account
  - Handle authentication + authorisation
  - Leverages Signature v4
- Cognito User Pool
  - You manage your own user pool (can be backed by Facebook, Google login etc...)
  - No need to write any custom code
  - Must implement authorisation in the backend
- Custom Authoriser
  - Great for 3rd party tokens
  - Very flexible in terms of what IAM policy is returned.
  - Handle Authentication verification + Authorisation in the Lambda function.
  - Pay per Lambda invocation, results are cached