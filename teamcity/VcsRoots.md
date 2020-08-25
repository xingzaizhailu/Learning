## VcsRoots

[VcsSettings Doc](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-vcs-settings/index.html)

### Basic Settings

To setup a basic VCSRoot, you only need the following:

- name - preferably the same to the filename
- url - Repo URL
- checkoutSubmodules - Need to set to ignore since submodules are checked out by default
- authMethod

``` kotlin
package _Self.vcsRoots
import jetbrains.buildServer.configs.kotlin.v2019_2.vcs.GitVcsRoot

object MyVcsRoot : GitVcsRoot({
    name = "MyVcsRoot"
    url = "https://..."
    checkoutSubmodules = GitVcsRoot.CheckoutSubmodules.IGNORE
    
    authMethod = uploadedKey {
        uploadedKey = "buildserviceuser"
    }
})
```

#### Auth Methods

The auth methods applicable to be used depends on the URL used.

##### For the HTTPS URL

###### Password

``` kotlin
object MyVcsRoot : GitVcsRoot({
    name = "MyVcsRoot"
    url = "https://..."
    pushUrl = "https://..."
    checkoutSubmodules = GitVcsRoot.CheckoutSubmodules.IGNORE
    authMethod = password {
        userName = "%USERNAME%"
        password = "%PASSWORD%"
    }
})
```

TODO: [How to grab pwd](https://confluence.teamxero.com/pages/viewpage.action?spaceKey=API&title=Access+Github+User+Creds+from+ParameterStore+for+CI+Tool+Usage)

###### Anonymous

Anonymous method is used when defining a VCSRoot from public Github repository.

``` kotlin
object MyVcsRoot : GitVcsRoot({
    name = "MyVcsRoot"
    url = "https://github.com/Repo"
    pushUrl = "https://github.com/Repo"
    branch = "refs/heads/master"
    branchSpec = "refs/heads/master"
    userNameStyle = GitVcsRoot.UserNameStyle.FULL
    checkoutSubmodules = GitVcsRoot.CheckoutSubmodules.IGNORE
    authMethod = anonymous()
})
```

##### For SSH URL

###### UploadedKey

 Use `git` as username (optional.) and choose `buildserviceuser` as key. See the first example of this page.

###### CustomPrivateKey

###### DefaultUploadedKey

#### Branch Specification

Whenever there are other branches that you want to watch out for changes aside from the default branch set in `branch` property, then you need to add those other branches in the `branchSpec` property.

``` kotlin
...

object MyVcsRoot : GitVcsRoot({
    name = "MyVcsRoot"
    url = "git@...:.../....git"
    branch = "refs/heads/master"
    branchSpec = """
        refs/heads/master
        refs/heads/some-branch-i-want-changes-to-be-detected
    """.trimIndent()

    ...
})
```

#### Username Style

By default, the username consists of the full name (e.g Saylor Twift) and email (e.g [saylor@twifties.com](mailto:saylor@twifties.com)) using the authMethod settings from the VCS root.

The following settings can be used:

- **GitVcsRoot.UserNameStyle.FULL**     - Saylor Twift [saylor@twifties.com](mailto:saylor@twifties.com)
- **GitVcsRoot.UserNameStyle.EMAIL**   - [saylor@twifties.com](mailto:saylor@twifties.com)
- **GitVcsRoot.UserNameStyle.USERID** - saylor.twift
- **GitVcsRoot.UserNameStyle.NAME**   - Saylor Twift

#### Checkout Submodules Options

For checking out submodules, there are two options:

- **GitVcsRoot.CheckoutSubmodules.SUBMODULES_CHECKOUT**
- **GitVcsRoot.CheckoutSubmodules.IGNORE**

### Add VCSRoot to Project

To use the VCSRoot, it must be defined within the main **Project** as a **vcsRoot**.  If not defined in the project, the VCS root will be unavailable to the build types.

``` kotlin
object Project : Project({
    description = "Build configuration for TeamCity Kotlin recipe pages."

    vcsRoot(MyVcs)
    vcsRoot(SomeOtherVcs)

    ...

})
```

### Using VCSRoots and Setting Checkout Rules

Checkout rules are used to define the directory or folder where you want the repo to be checked out. This would be helpful if you have to checkout multiple repos and need to set the context for each repository.

This could be set within the **BuildType** declaration.

``` kotlin
package _Self.buildTypes

import jetbrains.buildServer.configs.kotlin.v2019_2.BuildType
import jetbrains.buildServer.configs.kotlin.v2019_2.CheckoutMode
import jetbrains.buildServer.configs.kotlin.v2019_2.triggers.vcs
import jetbrains.buildServer.configs.kotlin.v2019_2.buildSteps.script

object BuildAndDeployToPages : BuildType({
    name = "Build and Deploy to Pages"
    description = "Build static files and deploy to Github Pages."

    ...

    vcs {
        root(_Self.vcsRoots.MyVcs)
        root(_Self.vcsRoots.SomeOtherVcs, "+:. => SomeOtherVcs")
        branchFilter = ""
        checkoutMode = CheckoutMode.ON_AGENT
        checkoutDir = ""
        cleanCheckout = true
        showDependenciesChanges = true
        excludeDefaultBranchChanges = true
        buildDefaultBranch = true
    }

    ...

})
```

### [VCS Checkout Rules](https://www.jetbrains.com/help/teamcity/vcs-checkout-rules.html)

### Tips

Setup **a separate VCS root for syncing** the build configs between the one in the UI and the one in version control.