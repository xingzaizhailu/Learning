## [Basics](https://kotlinlang.org/docs/reference/basic-types.html)

### Variables
- `val` Read-only local variables
- `var` can be reassigned
``` kotlin
val a: Int = 1  // immediate assignment
val b = 2       // `Int` type is inferred
val c: Int      // Type required when no initializer is provided
c = 3           // deferred assignment
```

#### Swapping two variables
``` kotlin
var a = 1
var b = 2
a = b.also { b = a }
```

### String
``` kotlin
var a = 1
// simple name in template:
val s1 = "a is $a"

a = 2
// arbitrary expression in template:
val s2 = "${s1.replace("is", "was")}, but now is $a"
```

#### raw string
Raw string cannot contain escaping.
``` kotlin
var text1 = """
    for (c in "foo")
        print(c)
"""

// You can remove leading whitespace with trimMargin() function:
val text = """
    |Tell me and I forget.
    |Teach me and I remember.
    |Involve me and I learn.
    |(Benjamin Franklin)
    """.trimMargin()

// Backslash escaping not supported, but templates are supported
val price = """
${'$'}9.99
"""
```
By default `|` is used as margin prefix, but you can choose another character and pass it as a parameter, like `trimMargin(">")`.

### Ranges
``` kotlin
val list = listOf("a", "b", "c")

if (-1 !in 0..list.lastIndex) {
    println("-1 is out of range")
}

if (list.size !in list.indices) {
    println("list size is out of valid list indices range, too")
}
```

Or over a progression
``` kotlin
for (x in 1..9 step 2) { // including 9
    print(x)
}

for (x in 1 until 100) {...} // half-open range: not include 100

for (x in 9 downTo 0 step 3) {
    print(x)
}
```

### Collections
Using lambda expressions to filter and map collections:
``` kotlin
val fruits = listOf("banana", "avocado", "apple", "kiwifruit")
fruits
    .filter { it.startsWith("a") }
    .sortedBy { it }
    .map { it.toUpperCase() }
    .forEach { println(it) }
```

### Map
``` kotlin
// Read-only map
val map = mapOf("a" to 1, "b" to 2, "c" to 3)
// Accessing a map
map["key"] = value
```

### Null
- If not null shorthand
    - ``` kotlin
    files?.size
      ```
- If not null and else shorthand
    - ``` kotlin
    files?.size ?: "empty"
      ```
- Executing a statement if null
    - ``` kotlin
    val email = values["email"] ?: throw IllegalStateException("Email is missing!")
      ```
- Get first item of a possibly empty collection
    - ``` kotlin
    val mainEmail = emails.firstOrNull() ?: ""
      ```
- Execute if not null
     ``` kotlin
    value?.let {
        ... // execute this block if not null
    }
      ```
- Map nullable value if not null
    - ``` kotlin
    val mapped = value?.let { transformValue(it) } ?: defaultValue
      ```

### `when` expression
``` kotlin
fun describe(obj: Any = null): String {
    return when (obj) {
        1          -> "One"
        "Hello"    -> "Greeting"
        is Long    -> "Long"
        !is String -> "Not a string"
        else       -> "Unknown"
    }
}
```

### Class
``` kotlin
class Turtle {
    fun penDown()
    fun penUp()
    fun turn(degrees: Double)
    fun forward(pixels: Double)
}
```

### Ojbect
#### `apply` - Configuring properties of an object
This is useful for configuring properties that aren't present in the object constructor.
``` kotlin
val myRectangle = Rectangle().apply {
    length = 4
    breadth = 5
    color = 0xFAFAFA
}
```
#### `with` - Calling multiple methods on an object instance:

``` kotlin
val myTurtle = Turtle()
with(myTurtle) { //draw a 100 pix square
    penDown()
    for (i in 1..4) {
        forward(100.0)
        turn(90.0)
    }
    penUp()
}
```

### Function
#### Single-expression functions
fun theAnswer() = 42

#### Extension Functions
``` kotlin
fun String.spaceToCamelCase() { ... }

"Convert this to camelcase".spaceToCamelCase()
```

#### `TODO()`: Marking code as incomplete
Kotlin's standard library function that always throw a `NotImplementedError`. Its return type is `Nothing` so it can be used regardless of expected type.
There's also an overload that accepts a reason parameter:
``` kotlin
fun calcTaxes(): BigDecimal = TODO("Waiting for feedback from accounting")
```

### [Returns and Jumps](https://kotlinlang.org/docs/reference/returns.html)
Kotlin has three structural jump expressions:
- `return`   - By default returns from the nearest enclosing function or anonymous function.
- `break`    - Terminates the nearest enclosing loop.
- `continue` - Proceeds to the next step of the nearest enclosing loop.
