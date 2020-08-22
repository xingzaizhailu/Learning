## Projects
In Kotlin DSK, you can create a project and set it as the main Project or you can set it as a subProject of an existing Project.
You can define the following in projects:
- VCS Roots
- Templates
- Shared Parameters
- SubProjects
- Connections or Other Features

### Create Project

```kotlin
package _Self

import jetbrains.buildServer.configs.kotlin.v2019_2.Project

object Project : Project({
    id("ProjectName")
    name = "Project Name"
    description = "Kotlin DSL build configuration for the Project Name."

    // no need to import the sub project package(s) when defining them in the Project file.
    subProject(ProjectOne.Project)
    subProject(ProjectTwo.Project)
    ...
    
    // Ordering SubProjects
    subProjectsOrder = arrayListOf(
        ProjectOne.Project
        ProjectTwo.Project
        ...
    )
})
```

#### Create Sub Project

You can create a sub project in the root of `.teamcity` folder.

```kotlin
// ProjectOne/Project.kt
package ProjectOne

import jetbrains.buildServer.configs.kotlin.v2019_2.Project

object Project : Project({
    id("ProjectOne")
    name = "Project One"
    description = "Some description for project one."

    ...
})
```

### Project Properties

Add and set the following additional properties to a `Project` object:

- archived - Set to `true` to archive the project
- params
- vcsRoot
- defaultTemplate
- template
- subProject
- subProjectsOrder
- buildType
- buildTypesOrder
- features
- cleanup - Set up the projects cleanup rules

#### Adding BuildTypes

```kotlin
package _Self

import _Self.buildTypes.BuildOne

import jetbrains.buildServer.configs.kotlin.v2019_2.Project

object Project : Project({
    id("ProjectName")
    name = "Project Name"
    description = "Kotlin DSL build configuration for the Project Name."

    buildType(BuildOne)

    ...
})
```

#### [Adding VSCRoot](./vcsRoots.md)

All defined VCS roots in `_Self/vcsRoots` needs to be added in the main Project file if you want them used by sub projects and other build configs. (Don't forget to import the `vscRoots` pakcage)

``` kotlin
...
import _Self.vcsRoots.*

object Project : Project({
    ...
    
    vcsRoot(RepoOne)
    vcsRoot(RepoTwo)

    ...
})
```

#### Adding Templates

You need to import the templates and define them in this project so that they can be used by other projects and build configs.

``` kotlin
...
import _Self.templateConfigs.*

object Project : Project({
    ...
    // You can also set a defaultTemplate for a project.
    defaultTemplate = "ZeDefaultTemplate"

    template(FirstTemplate)
    template(SecondTemplate)

    ...
})
```

#### Adding Parameters

The **Project** file will be the best place to define project-specific parameters that will be used by multiple sub projects and build configs.

``` kotlin
object Project : Project({
    ...

    params {
        param("SOME_TOKEN", "<secured>")
        param("GENERIC_URL", "<url_used_by_all>")
    }

    ...
})
```

#### Adding Features

You can also setup project-wide [features](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2018_2/-project-feature/index.html) within the **Project** file.

### Tips

Although you can add build type declaration in the main **Project** file, you might want to consider creating a **subProject** and defining all build types for readability. It is also a good idea to visualise or map out how the builds would be organised to properly plan what sub projects you need to create.