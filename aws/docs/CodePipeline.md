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