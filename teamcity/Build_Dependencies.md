## [Build Dependencies](https://www.jetbrains.com/help/teamcity/build-dependencies-setup.html#BuildDependenciesSetup-Introduction)

Build dependencies enables [build chaining](https://www.jetbrains.com/help/teamcity/build-dependencies-setup.html#BuildDependenciesSetup-WhentoCreateBuildChain) which allows different build configurations to be triggered by other build configurations that is part of the chain.

### Type of Build Dependencies

There are two types of dependencies in TeamCity:

- Snapshot Dependency - dependency between two build configurations
- Artifact Dependency - dependency involving an output/ artifact from another build configuration

#### Creating Dependencies

All dependencies are defined within the `dependencies` block and it can be added to both `BuildTypes` and `Templates`.

``` kotlin
package _Self.buildTypes

import jetbrains.buildServer.configs.kotlin.v2019_2.BuildType

object SampleBuild : BuildType({
    id("SampleBuild")
    ...

    dependencies {
        snapshot(...)
        artifacts(...)
    }
})
```

#### Creating a [Snapshot Dependency](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-snapshot-dependency/index.html)

To create a snapshot dependency, you need to know the id or the reference of the build config you want to create a dependency with.

``` kotlin
import jetbrains.buildServer.configs.kotlin.v2019_2.FailureAction
import jetbrains.buildServer.configs.kotlin.v2019_2.ReuseBuilds

object SampleBuild : BuildType({
    id("SampleBuild")
    ...

    dependencies {
        snapshot(<SomeBuildIdHere>) {
            onDependencyFailure = FailureAction.CANCEL
            onDependencyCancel = FailureAction.CANCEL
            reuseBuilds = ReuseBuilds.NO
            runOnSameAgent = true
            synchronizeRevisions = true
        }
    }
})
```

You can have the following options for `ReuseBuilds` available:

- **NO** - Do not reuse builds
- **SUCCESSFUL** - Reuse successful builds only
- **ANY** - Reuse any builds

And the following for `FailureAction`:

- **IGNORE** - Run dependent build and don't add problem
- **ADD_PROBLEM** - Run dependent build, but add problem
- **FAIL_TO_START** - Mark dependent build as failed to start
- **CANCEL** - Cancel dependent build

#### Creating an [Artifact Dependency](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-artifact-dependency/index.html)

``` kotlin
object SampleBuild : BuildType({
    id("SampleBuild")
    ...

    dependencies {
        artifacts(<SomeBuildIdHere>) {
            id = "SOME_ID"
            buildRule = lastSuccessful()
            artifactRules = "test.xml"
            cleanDestination = true
            enabled = true
        }
    }
})
```

You can have the following functions in setting the value for `buildRule`:

- **build** - Creates a build rule matching a build with the specified build number
- **lastFinished** - Creates a build rule matching the last finished build in the specified
- **lastPinned** - Creates a build rule matching a last pinned build in the specified branch
- **lastSuccessful** - Create a build rule matching the last successful build in the specified branch
- **sameChainorLastFinished** - Used as default. Create a build rule matching the build from the same build chain or last finished build
- **tag** - Creates a build rule matching a build with the specified tag in the specified branch

### Adding Multiple Dependencies

``` kotlin
dependencies {
    snapshot(<SomeBuildIdHere>) { onDependencyFailure = FailureAction.CANCEL }
    snapshot(<AnotherBuildIdHere>) { onDependencyCancel = FailureAction.CANCEL }
    artifacts(<SomeOtherBuildIdHere>) {
        buildRule = lastSuccessful()
        artifactRules = "test-result.xml"
    }
    ... add more here...
}
```

### Referencing Parameters From Dependencies

You can use the [defParamRefs](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-build-type/dep-param-refs.html) and [reverseDepParamRefs](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-build-type/reverse-dep-param-refs.html) in getting the value of a parameter from dependencies.

This would be possible if you have a [custom-built build types](./customizing-buildtypes.md) that allows passing custom parameters. Given that you have passed a reference **BuildType** where you want to get the parameter value named `some.version` from `refBuild` which is the first build config in you chain.

``` kotlin
class SampleBuild(
    val refBuild: BuildType?
) : BuildType({

    params {
        param("some.version", "${refBuild.depParamRefs["some.version"]}")
    }

    ...
})
```

### Tips

You can use `sequence` in setting up dependencies as shown in [Setting up build chains](https://github.dev.xero.com/Xero/teamcity-kotlin/tree/master/getting-started/examples/05-build-chains/.teamcity) and [Advanced build chains](https://github.dev.xero.com/Xero/teamcity-kotlin/tree/master/getting-started/examples/06-build-chains-advanced/.teamcity). The only caveat is that it must be defined only within a `project` block in your `settings.kts` file.