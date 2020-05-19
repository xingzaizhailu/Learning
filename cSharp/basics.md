

# Un familiar

Using

[class members](https://docs.microsoft.com/en-us/dotnet/csharp/tour-of-csharp/classes-and-objects#members)

# Basics

## Variables

#### nameof()

```
int height;
nameof(height)
```

### Numbers

```
int decimalNotation = 2_000_000;
int binaryNotation = 0b_0001_1110_0100_1000_0000;
int hexadecimalNotation = 0x_001E_8480;
decimalNotation == binaryNotation;			 	# true
decimalNotation == hexadecimalNotation;  		# true
```

```
sizeof(int) == 4
sizeof(double) == 8    // store wider range of number than decimal
sizeof(decimal) == 16  // more accurate. 12.75 is storeed as 1275 and
					   // a note to shift the decimal point two places left

double a = 0.1, b = 0.2;
decimal c = 0.1M, d = 0.2M;
a + b == 0.3  		   // false
c + d == 0.3M		   // true

// `double` type has useful special values
double.NaN
double.Epsilon
double.Infinity
```

**Tips:**

- Use `int` for whole numbers and `double` for real numbers that will not be compared to other values
- Use `decimal` for money, CAD drawings, general engineering and wherever accuracy is important

### String

#### Literal strings

```
// Escape characters like \t converted as a tab
string filePath = "C:\televisions\sony\bravia.txt";
```

#### Verbatim strings

```
string filePath = @"C:\televisions\sony\bravia.txt";
```

#### Interpolated strings

Prefixed with `$` to enable embedded formatted variables.

### Object

A special type can store any type of data. (Its flexibility brings messier code and possibly poor performance. Avoid it whenever possible.)

```
object height = 1.88;
object name = "Amir";

int length1 = name.Length; // gives compile error!
int lenght2 = ((string)name).Length;
```

### Dynamic types

Can also store any type of data, but even more than `object`, its flexibility comes at the cost of performance.

Compiler cannot check what the type is during build time. Instead, CLR checks for the member at runtime.

### Local variables

Defined inside methods.

`var` keyword can be used to declare local variables. The compiler will infer the type from the value. Inferred as `int` and `double` by default unless suffix`L` and `M' or 'F' is specified.

Use it only when the data type is obvious for code readability. If in doubt, spell it out!

### Getting default values for types

Most primitive types except `string` are **value types**, which means that they must have a value.

The `string` type is a `reference type`. This means that `string` variables contain the memory address of a value, not the value itself. A reference type can have a `null` value. `null` is a literal that indicates the variable doesn't reference anything yet and is the default for all reference types.

You can determine the default value of a type using the `default()` operator.

```
default(int)			// 0
default(bool)			// False
default(DateTime) // 01/01/0001 00:00:00
default(string)   // ""
```

### Working with null types

#### nullable value type

```
int thisCannotBeNull = null; // compile error!
int? thisCouldBenull = null;
Console.WriteLine(thisCouldBeNull.GetValueOrDefault());   // 0
```

#### Enabling nullable and non-nullable reference types

To enable the features at the project level:

```
<PropertyGroup>
	<Nullable>enable</Nullable>
</PropertyGroup>
```

To disable the feature at the file level:

```
#nullable disable
```

To enable the feature at the file level:

```
#nullable enable
```

#### Checking for null

Use the null-conditional operator `?.` when you are trying to use a member of a variable that might be `null`.

```
string authorName = null;
int x = authorName.Length;   // cause 'NullReferenceException'
int? y = authorName?.Length; // y == null
```

Use the null-coalescing operator `??` when you want to assign a variable to a result or an alternative value when the variable is null.

```
var result = authorname?.Length ?? 3;		// result == 3
```

## Displaying output




## Function

### Parameters



## Class

### Class constructor

```csharp
static ClassName()  // only executed once when the class is loaded
{
	...
}
```

### Instance constructor

```csharp
public ClassName()  // executed every time a new instance is created
{
    ...
}
```

### Field and Property

``` csharp
class Student
{
    public int StuId;      // field
    
    private int name;
    private int age;
    private bool canWork;  // property that's dynamically calculated
    
    public int Name{ get; set; }
    public int Age         // property
    {
        get { return age; }
        set
        {
            age = value;
            this.CalculateCanWork();  // suitable for get_canWork() is called more frequent than set_age(); otherwise calculate when get_canWork is called.
        }
    }
    pulic bool CanWork
    {
        get { return canWork; }
    }
    
    private void CalculateCanWork()
    {
        if (this.age >= 16)
        {
            this.canWork = true;
        }
        else
        {
            this.canWork = false;
        }
    }
}
```

### Indexer





## Delegate

`Func` is a delegate that points to a method that accepts one or more arguments and returns a value. `Action` is a delegate that points to a method which in turn accepts one or more arguments but returns no value.

### Action
```
	Action action = new Action(obj.method_no_return);
	action.invoke();    // for short: action();
```

### Func

```
	Func<int, int, int> func = new Func<int, int, int>(obj.method_return); // return, arg1, arg2
	func.invoke(x, y);
```

### Self defined delegate class

```
public delegate double Calc(double x, double y);
```

### Usage

Pass a method as a parameter

#### 

#### Callback

Callbacks are normally  at the end of the code and have no return.  

Decide either needed to call a function and which function to call.


