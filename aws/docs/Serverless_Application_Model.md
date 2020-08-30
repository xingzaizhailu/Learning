- [SAM examples](https://github.com/aws-samples/serverless-app-examples)

## Serverless Application Model

Framework for developing and deploying serverless applications.

- All the configurations is YAML code
- Generate complex CloudFormation from simple SAM YAML file
- Support anything from CloudFormation: Outputs, Mappings, Parameters, Resources...
- Only two commands to deploy to AWS
- SAM can use CodeDeploy to deploy Lambda functions
- SAM can help you to run Lambda, API Gateway, DynamoDB locally

### AWS SAM - Recipe

- Transform Header indicates it's SAM template
  - `Transform: 'AWS::Serverless-2016-10-31'`
- Write Code
  - AWS::Serverless::Function
  - AWS::Serverless::Api
  - AWS::Serverless::SimpleTable
- Package & Deploy
  - aws cloudformation package or sam package
    - sam package --s3-bucket <bucket_name> --template-file template.yaml --output-template-file gen/template-generated.yaml
    - Zip and upload Application code and Swagger File (optional) to Code S3 bucket
    - Transform SAM Template into CloudFormation Template
  - aws cloudformation deploy or sam deploy
    - sam deploy  --template-file gen/template-generated.yaml --stack-name <stack_name> --capabilities CAPABILITY_IAM
    - Create and execute change set and CloudFormation will apply them to the Stack.

### SAM Policy Templates

[List of templates](https://docs.aws.amazon.com/serverless-application-model/latest/developerguide/serverless-policy-templates.html) to apply permissions to Lambda Functions.

Important examples:

- S3ReadPolicy

- SQSPollerPolicy

  - ``` yam
    MyFunctions:
    	Type: 'AWS::Serverless::Function'
    	Properties:
    		CodeUri: ${codeurl}
    		Handler: hello.handler
    		Runtime: python3.7
    		Policies:
    			- SQSPollerPolicy:
    					QueueName:
    						!GetAtt MyQueue.QueueName
    ```

- DynamoDBCrudPolicy: Create Read Update Delete

### SAM with CodeDeploy

SAM framework natively uses CodeDeploy to update Lambda functions.

- Traffic Shifting feature
- Pre and Post traffic hooks features to validate deployment (before the traffic shift starts and after it ends)
- Easy & automated rollback using CloudWatch Alarms

#### SAM - exam
- SAM is build on CloudFormation
- SAM requires the Transform and Resources Sections
- Commands
- SAM Policy templates for easy IAM policy definition
- SAM is integrated with CodeDeploy to do deploy to Lambda aliases
