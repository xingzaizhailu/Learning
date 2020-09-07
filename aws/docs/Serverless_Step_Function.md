## Step Functions

Build serverless visual workflow to orchestrate your Lambda functions.

- Represent flow as a JSON state machine
- Features: sequence, parallel, conditions, timeouts, error handling...
- Can also integrate with EC2, ECS, On premise servers, API Gateway
- Maximum execution time of 1 year
- Possibility to implement human approval feature
- Use cases:
  - Order fulfilment
  - Data processing
  - Web applications
  - Any workflow

### Error Handling

- Any state can encounter runtime errors for various reasons:
  - Stage machine definition issues (for example, no matching rule in a Choice stage)
  - Task failures (for example, an exception in a Lambda function)
  - Transient issues (for example, network partition events)
- By default, when a state reports an error, AWS Step Functions causes the execution to fail entirely
- Retrying failures - Retry: IntervalSeconds, MaxAttempts, BackoffRate
- Moving on - Catch: ErrorEquals, Next
- Best practice is to include data in the error messages

### Step Functions - Standard vs Express

|                                 | Standard Workflows                                           | Express Workflows                                            |
| ------------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| Maximum duration                | 1 year                                                       | 5 minutes                                                    |
| Supported execution start rate  | Over 2,000 per second                                        | Over 100,000 per second                                      |
| Supported state transition rate | Over 4,000 per second per account                            | Nearly unlimited                                             |
| Execution history               | Execution can be listed and described with Step Functions APIs, and visually debugged through the console. They can also be inspected in CloudWatch Logs by enabling logging on your state machine. | Execution can be inspected in CloudWatch Logs by enabling logging on your state machine. |
| Execution semantics             | Exactly-once workflow execution                              | At-least-once workflow execution                             |

