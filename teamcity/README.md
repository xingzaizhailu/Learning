## Resources
- [Configuration as Code blogs](https://blog.jetbrains.com/teamcity/2019/03/configuration-as-code-part-1-getting-started-with-kotlin-dsl/)
- [jetbrains.buildServer.configs.kotlin.v2019\_2 Doc](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/index.html)

## Basics

### Directory Structure
All TeamCity Kotlin files must be within the `.teamcity` folder in the root directory of your project. In a `.teamcity` directory, the least files you would see are the following:

```
├── .teamcity
│   ├── settings.kts
│   └── pom.xml
└── <project files>
```
The settings.kts file can be divided into separate files for better readability and maintainability. For example:

```
├── _Self
│   ├── buildSteps
│   │   └── CustomStep.kt
│   ├── buildTypes
│   │   ├── CustomBuildType.kt
│   │   └── AnotherBuildType.kt
│   ├── templateConfigs
│   │   ├── BuildTemplate.kt
│   │   ├── PRTemplate.kt
│   │   └── DeployTemplate.kt
│   ├── vcsRoots
│   │   └── CustomVcsRoot.kt
│   ├── dataClasses
│   │   └── SomeDataClass.kt
│   └── Project.kt                          // The main project file
├── Rollback                                // A sub project folder
│   └── Project.kt
├── DeployToTest                            // Another sub project folder
│   └── Project.kt
├── pom.xml
└── settings.kts
```

### Kotlin DSL

- [Projects](./Projects.md)
- [BuildTypes](./BuildTypes.md)
- [BuildSteps](./BuildSteps.md)
