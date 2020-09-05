## [Parameters](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-parameter/index.html)

[Configuring build parameters](https://www.jetbrains.com/help/teamcity/configuring-build-parameters.html)

### Types of Parameters

Parameters are key-value pairs that you can define in a **Project**, **Template**, **BuildType** and **BuildSteps**.

There are three types of Parameters in TeamCity:

- Configuration Parameters
- Environment Variables
- System Properties

TeamCity also gives you some [predefined parameters](https://www.jetbrains.com/help/teamcity/predefined-build-parameters.html) ready to be used.

### Creating Parameters

To create a basic **Parameter**, simply define a `param` within the `params` block. The `param` can be set with two values, the first one would be the key or parameter name, and the other one would be the parameter value.

The name of a configuration parameter must satisfy the following requirements:

- contain only the following characters: `[a-zA-Z0-9._-*]`
- start with an ASCII letter

TeamCity allows users to create specific types of parameters aside from the basic one. You can create the following:

- Password
- Checkbox
- Select
- Text

To know more about the different types of parameters available, see the [ParameterizedWithType](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2018_2/-parametrized-with-type/index.html) class and TeamCity documentation on [parameter specification](https://www.jetbrains.com/help/teamcity/typed-parameters.html#TypedParameters-AddingParameterSpecification).

#### Parameter Display Type

Three options for `ParameterDisplay`:

- **NORMAL** - Parameter is shown as usual
- **PROMPT** - TeamCity will require a review of the parameter value when clicking the Run button (won't require the parameter if build is triggered automatically)
- **HIDDEN** - Parameter will not be shown in the build dialog, but will be sent to a build

You need to import the `jetbrains.buildServer.configs.kotlin.v2019_2.ParameterDisplay` enum class to use it.

#### Creating Password Parameters

To create a password parameter, you need to set the mandatory name and value. You can also add the following shared properties:

- **label** - label to use for the parameter in UI
- **description** - parameter description
- **display** - parameter display mode
- **readOnly** - whether the parameter is read-only, by default `false`

``` kotlin
params {
    password("token1", "<secure_value>")
    password("token2", "<secure_value>", label = "Token Two", description = "Another token", display = ParameterDisplay.HIDDEN, readOnly = true)
}
```

#### Creating Checkbox Parameters

To create a [checkbox](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-parametrized-with-type/checkbox.html) parameter, you need to set the mandatory **name** and **value**. You can also add the following additional properties in addition to the shared ones:

- **checked** - value of the parameter when checkbox is checked
- **unchecked** - value of the parameter when checkbox is not checked

``` kotlin
params {
    checkbox("box1", "Checkbox1")
    checkbox("box2", "Checkbox2", label = "Checkbox Two", description = "Checkbox for item 2", display = ParameterDisplay.NORMAL, readOnly = false, checked = "value1", unchecked = "value2")
}
```

#### Creating Select Parameters

To create a [select](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2018_2/-parametrized-with-type/select.html) parameter, you need to set the mandatory **name** and **value**. You can also add the following additional properties in addition to the shared ones:

- **allowMultiple** - whether multiple selected values are allowed
- **valueSeparator** - separator to use for multiple values, by default `,`
- **options** - possible parameter values. A string representation of an object in the list is used both as a option value and as a label in UI, unless it is a `Pair`, in which case a string representation of its first component is used as a label and a string representation of the second component is used as a value

``` kotlin
params {
    select("select1", "SelectBox1")
    select("select2", "SelectBox2", label = "Select Box Two", description = "Select box for item 2", display = ParameterDisplay.NORMAL, readOnly = false, allowMultiple = true, valueSeparator = "|", options = "Value1|Value2|Value3")
}
```

#### Creating Text Parameters

To create a [text](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2018_2/-parametrized-with-type/text.html) parameter, you need to set the mandatory **name** and **value**. You can also add the following additional properties in addition to the shared ones:

- **allowEmpty** - whether the empty value is allowed for this parameter
- **regex** - regex to use for validation
- **validationMessage** - message to show if validation fails

``` kotlin
params {
    text("text1", "Text1")
    text("text2", "Text2", label = "TextBox Two", description = "Textbox for something", display = ParameterDisplay.NORMAL, readOnly = false, allowEmpty = true)
    text("text3", "Text3", label = "TextBox Three", description = "Textbox for something", display = ParameterDisplay.NORMAL, readOnly = false, regex = "<someregex>", validationMessage = "Some message")
}
```

#### Creating Environment Variables

To create an **Environment Variable** you and prefix the parameter name with `env.`.

```kotlin
params {
    param("env.SOME_ENV", "envvalue")
}
```

#### Creating System Properties

To create a **System Property** you and prefix the parameter name with `system.`.

``` kotlin
params {
    param("system.SOME_PROPERTY", "propertyvalue")
}
```

### Using Parameters

Use parameters enclosed with a `%` character. Also, parameters are evaluated as strings.

``` kotlin
package _Self.buildTypes

import jetbrains.buildServer.configs.kotlin.v2019_2.BuildType
import jetbrains.buildServer.configs.kotlin.v2019_2.buildSteps.script

object SampleBuild : BuildType({
    id("SampleBuild")
    ...

    params {
        param("some_value", "some value")
    }

    steps {
        script {
            name = "Test"
            scriptContent = "echo %some_value%"
        }
    }
})
```

### Setting Parameters Dynamically

You can create a script that can set parameters dynamically. Below is an example of setting some git-specific parameters.

```kotlin
package _Self.buildTypes

import jetbrains.buildServer.configs.kotlin.v2019_2.BuildType
import jetbrains.buildServer.configs.kotlin.v2019_2.buildSteps.script

object SampleBuild : BuildType({
    id("SampleBuild")
    ...

    params {
        param("short_sha", "<GENERATED>")
        param("committer", "<GENERATED>")
        param("changelog", "<GENERATED>")
    }

    val commitLogUrl: String = _Self.vcsRoots.Repo1.url + "/commits/" + "%src_branch%"

    steps {
        script {
            id = "SetGitParameters"
            name = "Set git-specific parameters"
            scriptContent = """
                shortSha=$(git log --pretty=format:%h -1)
                committer=$(git log --pretty=format:%an -1)
                changelog=${commitLogUrl}
                echo "##teamcity[setParameter name='short_sha' value='${'$'}{shortSha}']"
                echo "##teamcity[setParameter name='committer' value='${'$'}{committer}']"
                echo "##teamcity[setParameter name='changelog' value='${'$'}{changelog}']"
            """.trimIndent()
        }
    }
})
```

Take note of the interpolation in setting the values. The `$` sign needs to be enclosed like this `${'$'}`.

### About Implicit Requirements

TeamCity will also set [implicit requirements](https://www.jetbrains.com/help/teamcity/agent-requirements.html#AgentRequirements-ImplicitRequirements) to agents whenever you set a parameter with a value referenced from another build configuration or a parent project. This is indicated by the `%` character.

In the example below, `%refparam%` must not be empty otherwise no agents will be assigned to your build configuration and you will get a warning.

```kotlin
params {
    param("some_value", "%refparam%")
}
```

### Tips

- You can pass a parameter to other build types by setting build dependencies.

- [Param](https://teamcity.jetbrains.com/app/dsl-documentation/jetbrains.build-server.configs.kotlin.v2019_2/-parametrized/param.html) only accepts `String` types so you must be either enclose names and values with double quotes. There are exceptions. For example, if you explicitly set the type of a variable to `String` then there is no need to interpolate the value and you can use it directly as a parameter value.

  - ``` kotlin
    package _Self.buildTypes
    
    import jetbrains.buildServer.configs.kotlin.v2018_2.BuildType
    
    object SampleBuild : BuildType({
        id("SampleBuild")
        ...
    
        val hello = "hello"
        val hi: String = "hi"			// explicitly set the type as String
    
        params {
            param("normal_name", "normal_value")
            param("unknown_type", "${hello}")
            param("known_type", hi)		// used directly as a parameter value
        }
    })
    ```