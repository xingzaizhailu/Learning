## Resources
- [Basic Syntax](https://kotlinlang.org/docs/reference/basic-syntax.html)
- [Idioms](https://kotlinlang.org/docs/reference/idioms.html): A collection of random and frequently used idioms in Kotlin.
- [Coding Conventions](https://kotlinlang.org/docs/reference/coding-conventions.html)
- [Jetbrain Education Products: Learn languages by doing](https://www.jetbrains.com/help/education/learner-start-guide.html) [Online Version](https://play.kotlinlang.org/koans/overview)
- [Kotlin Examples](https://play.kotlinlang.org/byExample)

## Running Code Snippets
- Scratches let us write and run code in a temporary file outside of our project in the IDE.
    - Click **File | New | Scratch file** and select the **Kotlin** type.
    - Need to select in `Use classpath of module` and `import` to use classes or functions from a project. (Remember to recompile after modification)
- Worksheets are like scratches, but they reside within projects.
    - Worksheets are project files: they are stored in project directories and tied to the project modules. Worksheets are useful for writing code parts that don't actually make a software unit but should still be stored together in a project. For example, you can use worksheets for education or demo materials.
    - Right-click the directory in the project tree and select **New | Kotlin Worksheet**.
    - Automatically get access to classes and functions from the module where it resides.
    - Enable automatically rebuild module before each run by selecting **Make before Run**.
- REPL (Read-Eval-Print-Loop) runs code in an interactive console.
    - **Tools | Kotlin | Kotlin REPL** in IntelliJ IDEA
    - or open `/bin/kotlinc-jvm` in command line

## Build and Run project
``` kotlin
# Compile
$ kotlinc main.kt -include-runtime -d hello.jar
$ kotlinc main.kt -d hello.jar

# Run
$ java -jar hello.jar
$ kotlin -classpath hello.jar HelloKt

# Run a Kotlin Script
$ kotlinc -script code.kts
```

## Project Build Tools
- [Ant](https://kotlinlang.org/docs/reference/using-ant.html)
- [Maven](https://kotlinlang.org/docs/reference/using-maven.html)
- [Gradle](https://kotlinlang.org/docs/reference/using-gradle.html)
