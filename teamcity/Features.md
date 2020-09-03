## Features

Features are build-in functionalities provided by TeamCity. Features can be added to projects, templates and build configurations within the `Features` block.

### [Project Features](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-project-feature/index.html)

Project features adds functionalities to projects. In the example below, it adds a [GithubConnection](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2.project-features/-git-hub-connection/index.html) to the project.

``` kotlin
package _Self

import jetbrains.buildServer.configs.kotlin.v2019_2.Project

object Project : Project({
    id("ProjectName")
    ...

    features {
        feature {
            type = "OAuthProvider"
            id = "API"
            param("clientId", "<ClientId>")
            param("defaultTokenScope", "public_repo,repo,repo:status,write:repo_hook")
            param("secure:clientSecret", "<ClientSecret>")
            param("displayName", "GitHub.com")
            param("gitHubUrl", "https://github.com/")
            param("providerType", "GitHub")
        }
    }
})
```

### [Build Features](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-build-feature/index.html)

The most common feature added would be  the following:

- [DockerSupport](https://www.jetbrains.com/help/teamcity/docker-support.html)
- [PullRequests](https://www.jetbrains.com/help/teamcity/pull-requests.html) - used in setting up continuous integration
- [CommitStatusPublisher](https://www.jetbrains.com/help/teamcity/commit-status-publisher.html#Using+Commit+Status+Publisherused) - in setting up continuous integration

In the example below, we add the common build features to a build configuration.

``` kotlin
package _Self.buildTypes

import jetbrains.buildServer.configs.kotlin.v2019_2.BuildType
import jetbrains.buildServer.configs.kotlin.v2019_2.buildFeatures.dockerSupport
import jetbrains.buildServer.configs.kotlin.v2019_2.buildFeatures.PullRequests
import jetbrains.buildServer.configs.kotlin.v2019_2.buildFeatures.pullRequests
import jetbrains.buildServer.configs.kotlin.v2019_2.buildFeatures.commitStatusPublisher

object SampleBuild : BuildType({
    id("SampleBuild")
    ...

    features {
        dockerSupport {
            id = "SOME_ID"
        }

        pullRequests {
            vcsRootExtId = "${_Self.vcsRoots.RepoOne.id}"
            provider = github {
                authType = token {
                    token = "<secure_string_value>"
                }
                filterAuthorRole = PullRequests.GitHubRoleFilter.EVERYBODY
            }
        }

        commitStatusPublisher {
            vcsRootExtId = "${_Self.vcsRoots.RepoOne.id}"
            publisher = github {
                githubUrl = "https://api.github.com"
                authType = personalToken {
                    token = "<secure_string_value>"
                }
            }
        }
    }
})
```

