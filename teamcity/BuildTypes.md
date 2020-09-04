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

## Customizing BuildTypes

### Creating Custom BuildTypes

We can customize the build type by creating a derived class using the [BuildType class](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-build-type/index.html) as the base class.

#### Examples

##### Explicit Initializer Block

In Kotlin, a constructor cannot contain any code and so initialization code must be placed in initializer blocks, which uses the `init` keyword. The example below defines a secondary constructor and uses the `super` keyword to initialize the *BuildType* base class while also passing the initializer code that sets the values of derived properties.

``` kotlin
class CustomBuild : BuildType {
    constructor(
        buildId: String,
        buildName: String
        init: CustomBuild.() -> Unit = {}
    ) : super() {
        id(buildId)
        name = buildName
        ...

        init()		// Important: Remember to init()!
    }
}
```

An alternative would be to directly define the initializer block without defining a secondary constructor.

``` kotlin
class CustomBuild(
    buildId: String,
    buildName: String
) : BuildType() {
    init {
        id(buildId)
        name = buildName

        ...
    }
}
```

And another version:

``` kotlin
class CustomBuild(
    buildId: String,
    buildName: String,
    init: BuildType.() -> Unit = {}
) : BuildType({
    id(buildId)
    name = buildName
}) {
    init {
        init()
    }
}
```

##### Defining Parameters as Class Properties

This example is a bit the same to the first except this one defines the parameters as named properties.

```kotlin
class CustomBuild : BuildType {
    // `val` provides immutability to variables while `var` makes it mutable
    val buildId: String
    var buildName: String

    constructor(buildId: String, buildName: String) : super() {
        id(buildId)
        name = buildName

        ...
    }
}
```

##### Straight Forward Usage

No secondary constructor definition. No visible base class initialization and initializer block definition. This example defines the valid parameters in the derived class and directly pass the initializer block to the base class.

```kotlin
class CustomBuild(
    val buildId: String,
    val buildName: String
) : BuildType({
    id(buildId)
    name = buildName

    ...
})
```

### Using Custom BuildTypes

```kotlin
package ProjectOne

import _Self.buildTypes.CustomBuild

import jetbrains.buildServer.configs.kotlin.v2019_2.Project

object Project : Project({
    id("ProjectOne")
    name = "Project One"
    description = "Some description for project one."

    val custom1 = CustomBuild("ProjectOne_CustomBuild_One", "Build Number One")
    buildType(custom1)
    val custom2 = CustomBuild("ProjectOne_CustomBuild_Two", "Build Number Two")
    buildType(custom2)

    ...

    buildTypesOrder = arrayListOf(
        custom1,
        custom2
    )
})
```