## [BuildSteps](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2.build-steps/index.html)

### Adding Build Steps

``` kotlin
package _Self.buildTypes

import jetbrains.buildServer.configs.kotlin.v2018_2.BuildType
import jetbrains.buildServer.configs.kotlin.v2018_2.buildSteps.script

object SampleBuild : BuildType({
    id("SampleBuild")
    name = "Sample Build"
    description = "This is a sample build."

    steps { 	// Add Build Steps here
        ... 
    }
})
```

#### Available Properties

Aside from the common `id` and `name` properties, you can use and set the values of the following properties for your build steps:

- **enabled** - Specifies whether the step is enabled, `true` by default
- **executionMode** - Build step execution mode
  - **DEFAULT** - step will not be executed if there are other failed steps
  - **RUN_ON_SUCCESS** - will execute step only if status on server is “successful”; server will be asked for build status
  - **RUN_ON_FAILURE** - will execute step even if previous step(s) has failed, server will not be queried for build status
  - **ALWAYS** - will execute step always, even if build was interrupted
- **type** - Build step type

You can also set or inherit a few functions that you can use within your step. One of them is `param` which allows you to set a parameter specific to the build step.

### Bundled Runners

TeamCity provides a few built-in steps or runners that can be used easily.

Below are the most common ones:

- [DockerCommandStep](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2.build-steps/-docker-command-step/index.html) - used for generic docker commands
- [DockerComposeStep](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2.build-steps/-docker-compose-step/index.html) - used for docker compose
- [ScriptBuildStep](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2.build-steps/-script-build-step/index.html) - runs a custom script

Refer to [this page](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2.build-steps/index.html) for a complete list of available steps.

#### Using DockerCommand Step

Must import `jetbrains.buildServer.configs.kotlin.v2019_2.buildSteps.dockerCommand` class.

```kotlin
steps {
    dockerCommand {
        id = "DockerPushImage"
        name = "Push image to ECR"
        commandType = push {
            namesAndTags = "<imagename>:<imagetag>"
        }
        param("docker.sub.command", "tag")
    }
}
```

#### Using ScriptBuild Step

The most used step in TeamCity. Import the `jetbrains.buildServer.configs.kotlin.v2019_2.buildSteps.script`.

``` kotlin
steps {
    script {
        id = "RunTests"
        name = "Run tests"
        workingDir = "someDir"
        scriptContent = """
            echo 'test'
            echo 'test again'
        """.trimIndent()
    }
}
```

## [Customizing Build Steps](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-build-steps/index.html)

To create customized build steps in TeamCity, you need to know what type of build step you will be customizing. The list of available steps [here](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2.build-steps/index.html).

### Creating Custom Build Step

```kotlin
package _Self.buildSteps

import jetbrains.buildServer.configs.kotlin.v2019_2.BuildSteps
import jetbrains.buildServer.configs.kotlin.v2019_2.buildSteps.ScriptBuildStep

class CustomStep : ScriptBuildStep() {
    lateinit var param1: String
    lateinit var param2: String

    constructor(init: CustomStep.() -> Unit) : this() {
        init()

        name = "Custom Step"
        scriptContent = "echo 'custom step'"
    }
}

fun BuildSteps.customStep(init: CustomStep.() -> Unit) : CustomStep {
    val result = CustomStep(init)
    step(result)
    return result
}
```

### Using Custom Build Step

```kotlin
package _Self.buildTypes

import _Self.buildSteps.customStep

import jetbrains.buildServer.configs.kotlin.v2018_2.BuildType

object SampleBuild : BuildType({
    id("SampleBuild")
    ...

    steps {
        customStep {
            param1 = "hello"
            param2 = "hi"
        }
    }
})
```