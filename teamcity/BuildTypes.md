## [BuildTypes](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-build-type/index.html)

### Creating a Basic BuildType

A **BuildType** represents a build configuration. Creating one would also be the same to creating a **Template** since they have the similar available properties. Make sure that the file name is the same to the build type name.

### Setting BuildType Properties

Some common properties that can be used are: (The same to Templates)

- VCS Settings
- Triggers
- Parameters
- Steps
- Agent Requirements
- Build Features
- Artifact Rules
- Failure Conditions

```kotlin
package _Self.buildTypes

import jetbrains.buildServer.configs.kotlin.v2019_2.BuildType
import jetbrains.buildServer.configs.kotlin.v2019_2.triggers.schedule
import jetbrains.buildServer.configs.kotlin.v2019_2.buildSteps.script
import jetbrains.buildServer.configs.kotlin.v2019_2.buildFeatures.dockerSupport

object SampleBuild : BuildType({
    id("SampleBuild")
    name = "Sample Build"
    description = "This is a sample build."

    // Adding VCS Settings
    vcs {
        root(_Self.vcsRoots)
        cleanCheckout = true
    }

    // Adding Triggers
    triggers {
        schedule {
            id = "DAILYBUILDS"
            schedulingPolicy = daily {
                hour = 9
            }
        }
    }

    // Setting Parameters
    params {
        param("some_param", "hello")
        param("passed_param", "%param1%")
    }

    // Adding Artifact Rules
    artifactRules = """
        test-results.xml
        screenshots.zip
    """.trimIndent()

    // Adding Build Steps
    steps {
        script {
            ...
        }
    }

    // Adding Build Features
    features {
        dockerSupport {
            id = "SOME_ID_HERE"
        }
    }

    // Adding Agent Requirements
    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
```

### Using the BuildTypes

To use the build type, simply import it and define it in your **Project** files. And you can also order them setting the `buildTypesOrder` into an array list of valid build types.

```kotlin
package SomeProject

import _Self.buildTypes.*
import jetbrains.buildServer.configs.kotlin.v2018_2.Project

object Project : Project({
    id("SomeProject")
    name = "Some Name"
    description = "Describe this project."

    buildType(SampleBuild)
    buildType(AnotherBuild)
    buildType(LastBuild)

    buildTypesOrder = arrayListOf(
        SampleBuild,
        AnotherBuild,
        LastBuild
    )

    ...
})
```

### Tips

For cases where you need to re-use a **BuildType**, you need to customise it and allow parameters to be passed to it. It must accept a parameter for the `ID` value since it has to be unique for all build configurations.