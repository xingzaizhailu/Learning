## [Agent Requirements](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-requirements/index.html)

### Assigning Agents

Currently, there is no way to assign builds or projects to certain agent pools using version control. It has to be done manually. Read the documentation on [agent pool assignment](https://www.jetbrains.com/help/teamcity/assigning-build-configurations-to-specific-build-agents.html#AssigningBuildConfigurationstoSpecificBuildAgents-Agentpools) for more information.

### Adding Agent Requirements

You can add agent requirements to both **BuildType** and **Template**. You need to place the settings within the `requirements` block.

``` kotlin
requirements {
	equals("teamcity.agent.jvm.os.name", "Linux")
    equals("aws_region", "us-east-1")
	exists("aws_account")
}
```

Most common functions used for setting requirements are:

- **contains** - Adds a requirement that the value of the specified parameter contains the given value
- **equals** - Adds a requirement that the value of the specified parameter is equal to the given value
- **exists** - Adds a requirement that the parameter with the specified name exists on the agent
- **startsWith** - Adds a requirement that the value of the specified parameter starts with the given value

Checkout more functions [here](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-requirements/index.html).

#### Adding Linux Agent Requirements

You can use the `equals("teamcity.agent.jvm.os.name", "Linux")` requirement to specify the build config must run on Linux.

#### Adding AWS-Specific Requirements

You can use the requirements settings below to specify that the build config must run on an agent in a specific AWS region. For cases where you need to assume a role, you need to add an additional requirement to run it on agents with `LinuxAgent` on their name.

``` kotlin
requirements {
    equals("aws_region", "us-east-1")
    exists("aws_account")
    contains("system.agent.name", "LinuxAgent")
}
```
