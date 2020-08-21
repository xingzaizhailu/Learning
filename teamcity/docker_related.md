## Docker
There are two ways todo Docker operations:
- Docker CLI
    - ``` kotlin
    object RunStuff : BuildType({
        ...

        steps {
            ...

            script {
                id = "..."
                name = "..."
                executionMode = BuildStep.ExecutionMode.RUN_ON_FAILURE
                scriptContent = "docker <command> [option]"
            }

            ...
        }
    })
      ```
- DockerCommand
    - ``` kotlin
    object RunStuff : BuildType({
        ...

        steps {
            ...

            dockerCommand {
                name = "..."
                commandType = other {  // options: push
                    subCommand = "pull"
                    commandArgs = "alpine:latest"
                }
                param("key", "val")
            }

            // Or
            dockerCommand {
                name = "..."
                commandType = push {  // options: push
                    namesAngTags = "registry_host/img_name:img_tag"
                }
                param("key", "val")
            }

            ...
        }
    })
      ```

### Access Docker Registries
#### AWS ECR
``` kotlin
object RunStuff : BuildType({
    params {
        param("aws_account", "123456789012")
        param("aws_region", "ap-southeast-2")
    }

    steps {
        steps {
            assumeRole {
                roleArn = "arn:aws:iam::%aws_account%:role/role_name"
                region = "%aws_region%"
            }
        }

        script {
            id = "LoginToECR"
            name = "Login to ECR"
            scriptContent = "eval ${'$'}(aws ecr get-login --region %deploy_region% --no-include-email)"
        }
    }
})
```
#### Other Registry
``` kotlin
object RunStuff : BuildType({
    params {
        param("USER", "...")
        param("PASSWORD", "<secured>")
        param("URL", "https://...")
    }

    steps {
        script {
            id = "LoginToRegistry"
            name = "Login to Registry"
            scriptContent = "docker login -u %USER% -p %PASSWORD% %URL%"
        }
    }
})
```
#### Tips and Tricks
Docker allows users to login to multiple docker registries, so you can connect to multiple registries in one `scriptContent` if needed.
