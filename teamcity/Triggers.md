## [Triggers](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-triggers/index.html)

### Available Triggers

Triggers can be added to BuildTypes and Templates.

Ref: [Configuring Build Triggers](https://www.jetbrains.com/help/teamcity/configuring-build-triggers.html)

#### [VCS Triggers](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2.triggers/-vcs-trigger/index.html)

VCS triggers are used to initiate a build whenever there are changes pushed to the repo. To setup one, first import `jetbrains.buildServer.configs.kotlin.v2019_2.triggers.vcs` and add then add the settings within the **triggers** block.

``` kotlin
triggers {
    vcs {
        branchFilter = "+:refs/heads/master"
        // Exclude markdown and TC files
        triggerRules = """
            -:.teamcity/**
            -:images/**
            -:*.md
            -:*.env
            -:.gitignore
        """.trimIndent()
        perCheckinTriggering = false
        groupCheckinsByCommitter = true
        enableQueueOptimization = false
    }
}
```

#### [Schedule Triggers](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2.triggers/-schedule-trigger/index.html)

Basically, a [cron](https://en.wikipedia.org/wiki/Cron). These triggers are used to initiate a build at a specific time. Import `jetbrains.buildServer.configs.kotlin.v2019_2.triggers.schedule` before adding the trigger settings. Refer to [Configuring Schedule Triggers](https://www.jetbrains.com/help/teamcity/configuring-schedule-triggers.html#ConfiguringScheduleTriggers-Examples) on how to setup advanced time settings using cron-like expressions.

``` kotlin
triggers {
    schedule {
        id = "DAILYBUILDS"
        schedulingPolicy = daily {
            hour = 9
        }
        branchFilter = ""
        triggerBuild = always()
        withPendingChangesOnly = false
    }
}
```

#### [Retry Build Triggers](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2.triggers/-retry-build-trigger/index.html)

Setup this trigger if you want to automatically re-run a failing build. Import `jetbrains.buildServer.configs.kotlin.v2019_2.triggers.retryBuild` and set up the trigger settings.

```kotlin
triggers {
    retryBuild {
        delaySeconds = 0
        attempts = 2
        branchFilter = "+:refs/heads/someBranch"
    }
}
```

#### [Finish Build Triggers](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2.triggers/-finish-build-trigger/index.html)

Initiates a build when a specified build is finished running. Import `jetbrains.buildServer.configs.kotlin.v2019_2.triggers.finishBuildTrigger` and setup the trigger settings. Refer to [Configuring Finish Build Trigger](https://www.jetbrains.com/help/teamcity/configuring-finish-build-trigger.html)

```kotlin
triggers {
    finishBuildTrigger {
        buildType = "<build_id_for_build>"
        successfulOnly = true
        enabled = true
    }
}
```