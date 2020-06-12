## CodeCommit

### CodeCommit Security

1. Interactions are done using git
2. Authentication
   1. SSH keys: canfigure in IAM Console
   2. HTTPS: Done through the AWS CLI Authentication helper or Generating HTTPS credential
   3. MFA: can be enabled
3. Authorization
   1. IAM Policies manage user / role rights to repos
4. Encryption
   1. Repos are automatically encrypted at rest using KMS
   2. Encrypted in transit (HTTPS and SSH are both secure)
5. Cross Account aceess
   1. Don't share your SSH keys
   2. Don't share AWS credentials
   3. Use IAM Role in your AWS Account and use AWS STS (with AssumeRole API)

### CodeCommit Notifications

* Use cases for notifications SNS / AWS Lambda notifications:
  * Deletion of branches
  * Trigger for pushes happens in master branch
  * Notify external Build System
  * Trigger AWS Lambda function to perform codebase analysis (credentials in the code?)

* Use cases for CloudWatch Event Rules:
  * Trigger for pull request updates (created / updated / deleted / commented)
  * Commit comment events
  * CloudWatch Event Rules goes into an SNS topic



## CodePipeline

### Concetps:

- Pipelines: how software changes go through a release process. made up of stages.
- Stages: Made of a series of or parallel actions
- Actions
- Pipeline Executions: A set of changes released by a pipeline.
  - A pipeline can process multiple executions at the same time, a pipeline stage processes only one execution at a time.
  - Pipeline executions traverse pipeline stages in order. Valid statuses for pipelines are `InProgress`, `Stopping`, `Stopped`, `Succeeded`, `Superseded`, and `Failed`. 
  - An execution with a `Failed` or `Superseded` status does not continue through the pipeline and cannot be retried.
  - Stopped Executions: can be retried. Two ways:
    - Stop and wait
    - Stop and abandon
- Action Executions: the process of completing a configured action that operates on designated [artifacts](https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-artifacts)
  - Valid statuses for actions are `InProgress`, `Succeeded`, and `Failed`.
- Transition: the point where a pipeline execution moves to the next stage in the pipeline
- Artifacts: produced by some actions and consumed by others. Refers to the collection of data, such as application source code, built applications, dependencies, definitions files, templates, and so on.
- Source revision: the version of a source change that triggers a pipeline execution.

### example App

- Stage - Source
  - action - Source (GitHub)
- Stage - Build
  - parallel actions - JenkinsOnECS (Jenkins) || NorifyDevelopers (lambda)
  - action - TestAPI (Runscope)
- Stage - Deploy
  - action -  JavaApp (Elastic BeanStalk)

### [How Pipeline executions work](https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts-how-it-works.html)

### CodePipeline Artifacts

Each pipeline stage can create "artifacts". they are pressed stored in Amazon S3 and passes on to the next stage.

### CodePipeline Troubleshooting

If CodePipeline fails a stage, your pipeline stops and you can get information in the console.

AWS CloudTrail can be used to audit AWS API calls.

## CodeBuild

- Fully managed build service.
- Alternative to other build tools such as Jenkins.
- Continuous scaling (no servers to manage or provision, no build queue)
- Pay for usage: the time it takes to complete the builds
- Leverages Docker under the hood for reproducible builds
- Secure: Integration with KMS for encryption of build artifacts, IAM for build permission, and VPC for network security, CloudTrail for API calls logging.

### Overview

Build instructions can be defined in code ('buildspec.yml' file)

Output logs to Amazon S3 & AWS CloudWatch Logs

Metrics to monitor CodeBuild statistics

Use CloudWatch Alarms to detect failed builds and trigger notifications

CloudWatch Events / AWS Lambda as a Glue

Ability to reproduce CodeBuild locally to troubleshoot in case of errors

Builds can be defined within CodePipeline or CodeBuild itself

### CodeBuild BuildSpec

* `buildspec.yml` file must be at the root of your code.

* Define environment variables:
  * Plaintext variables
  * Secure secrets: use SSM Parameter store

* Phases (specify commands to run):
  * Install: install dependencies you may need for your build
  * Pre build: final commands to execute before build
  * Build: actual build commands
  * Post build: finishing touches (zip output for example)

- Artifacts: What to upload to S3 (encrypted with KMS)
- Cache: Files to cache (usually dependencies) to S3 for future build speedup

### CodeBuild Local Build

In case of need of deep troubleshooting beyond logs... For this, leverage the [Code Build Agent](https://docs.aws.amazon.com/codebuild/latest/userguide/use-codebuild-agent.html).

