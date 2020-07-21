

# Un familiar

Using

[class members](https://docs.microsoft.com/en-us/dotnet/csharp/tour-of-csharp/classes-and-objects#members)

# Basics
Create a new console app:
```shell
dotnet new console -lang "F#" -name "ExploringConsole"
```
## Variables

#### nameof()

```csharp
int height;
nameof(height)
```

### Numbers

```csharp
int decimalNotation = 2_000_000;
int binaryNotation = 0b_0001_1110_0100_1000_0000;
int hexadecimalNotation = 0x_001E_8480;
decimalNotation == binaryNotation;			 	# true
decimalNotation == hexadecimalNotation;  		# true
```

```csharp
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

```csharp
// Escape characters like \t converted as a tab
string filePath = "C:\televisions\sony\bravia.txt";
```

#### Verbatim strings

```csharp
string filePath = @"C:\televisions\sony\bravia.txt";
```

#### Interpolated strings

Prefixed with `$` to enable embedded formatted variables.

### Object

A special type can store any type of data. (Its flexibility brings messier code and possibly poor performance. Avoid it whenever possible.)

```csharp
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

```csharp
default(int)			// 0
default(bool)			// False
default(DateTime) // 01/01/0001 00:00:00
default(string)   // ""
```

### Working with null types

#### nullable value type

```csharp
int thisCannotBeNull = null; // compile error!
int? thisCouldBenull = null;
Console.WriteLine(thisCouldBeNull.GetValueOrDefault());   // 0
```

#### Enabling nullable and non-nullable reference types

To enable the features at the project level:

```csharp
<PropertyGroup>
	<Nullable>enable</Nullable>
</PropertyGroup>
```

To disable the feature at the file level:

```csharp
#nullable disable
```

To enable the feature at the file level:

```csharp
#nullable enable
```

#### Checking for null

Use the null-conditional operator `?.` when you are trying to use a member of a variable that might be `null`.

```csharp
string authorName = null;
int x = authorName.Length;   // cause 'NullReferenceException'
int? y = authorName?.Length; // y == null
```

Use the null-coalescing operator `??` when you want to assign a variable to a result or an alternative value when the variable is null.

```csharp
var result = authorname?.Length ?? 3;		// result == 3
```

## Exploring console applications further

### Displaying output

Using `Write` and `WriteLine`.

#### Formatting using numbered positional arguments

```csharp
int nApples = 12;
decimal perApplePrice = 0.35M;

Console.WriteLine(
	format: "{0} apples costs {1:C}",
	arg0: nApples,
	arg1: perApplePrice * nApples);

string formatted = string.Format(
	format: "{0} apples costs {1:C}",
	arg0: nApples,
	arg1: perApplePrice * nApples);
```

#### Formatting using interpolated strings

A `string` prefixed with `$`:

```csharp
Console.WriteLine($"{nApples} apples costs {perApplePrice * nApples:C}");
// 12 apples costs ￡4.20
```

#### Understanding format strings

The full syntax of a format item is:

```csharp
{ index [, alignment ] [: formatString]}
```

Positive integers are right-aligned and negative integers are left-aligned with the number width of characters.

- `N0`: thousand separators and no decimal places
- `C`: currency which is determined by the current thread.
  - e.g. If code is ran on a PC in UK you'll get pounds sterling with commas as the thousand separators
  - if on a PC in Germany, get Euros with dots as the thousand separators

### Get text input from the user

The `ReadLine` method waits for user to type some text, then as soon as the user presses _Enter_, whatever the user has typed is returned as a `string ` value.

### Importing a namespace

```csharp
using System;
Console.WriteLine("");
```

### Getting key input from the user

```csharp
Write("Press any key combination: ");
ConsoleKeyInfo key = ReadKey();
WriteLine();
WriteLine($"Key: {key.Key}", Char: {key.KeyChar}, Modifiers: {key.Modifiers});
// k => Key: K, Char: k, Modifiers: 0
// K => Key: K, Char: K, Modifiers: Shift
// k => Key: F12, Char: , Modifiers: 0
```

### Getting arguments

```csharp
dotnet run firstarg second-arg third:arg "fourth arg"
```

### More references

- [**C# Keywords**](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/index)
- [**Main() and command-line arguments (C# Programming Guide)**](https:// docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main- and-command-args/)
- [**Types (C# Programming Guide)**](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/types/)
- [**Statements, Expressions, and Operators (C# Programming Guide)**](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/statements-expressions-operators/)
- [**Strings (C# Programming Guide)**](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [**Nullable Types (C# Programming Guide)**](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/nullable-types/)
- [**Nullable reference types**](https://docs.microsoft.com/en-us/dotnet/csharp/nullable-references)
- [**Console Class**](https://docs.microsoft.com/en-us/dotnet/api/system.console?view=netcore-3.0)

## Controlling Flow and Converting Types

### Operating on variables

#### Unary Operators

- `++`/` --`
- typeof()
- sizeof()

#### Logical Operators

`&`, `|`,`^` 

#### Conditional Logical Operators

`&&`, `||` - Short-circuiting

#### Bitwise and bianry shift operators

`&`, `|`, `^`, `<<`, `>>`

### Selection statements

#### Pattern matching

The `if` statement can use the `is` keyword in combination with declaring a local variable to make your code safer.

```csharp
object o = "3"; // add and remove the "" to change the behavior
if (o is int i)
{
	WriteLine($"{i} x 4 = {i * 4}");
}
else
{
	WriteLine("o is not an int so it cannot multiply!");
}
```

#### Switch statement

Compares a single expression against a list of multiple possible `case` statements.

Every `case` section must end with:

- The `break` keyword
- Or the `goto` keyword - goto a `case` or a Label
- Or they should have no statements

```csharp
A_label:
  var number = (new Random()).Next(1, 7);
  WriteLine($"My random number is {number}");
  switch (number)
  {
    case 1:
      WriteLine("One");
      break;
    case 2:
      WriteLine("Two");
      goto case 1;
    case 3:
    case 4:
      WriteLine("Three or four");
      goto case 1;
    case 5:
      System.Threading.Thread.Sleep(500);
      goto A_label;
    default:
    	WriteLine("Default");
    	break;
  }
```

#### Pattern matching with the switch statement

With pattern matching, the `case` can be patterns besides literal values.

```csharp
using System.IO;

string path = "/Users/Workspace/Code/Chapter03";
Stream s = File.Open(
	Path.Combine(path, "file.txt"), FileMode.OpenOrCreate)
string msg = string.Empty;

switch (s)
{
  // `s` will only be a match if the stream is a `FileStream`
  // and its `CanWrite` property is `true`
	case FileStream writeableFile when s.CanWrite:
		msg = "The stream is a file that I can write to.";
		break;
	case FileStream readOnlyFile:
		msg = "The stream is a read-only file.";
		break;
	case MemoryStream ms:
		msg = "The stream is a memory address";
		break;
	default: // always evaluated last despite its current position
		msg = "The stream is some other type."
		break;
	case null:
		msg = "The stream is null.";
		break;
}
WriteLine(msg);
```

#### Simplifying switch statements with switch expressions

Separated by "," and "_" is used to represent the default return value.

```csharp
msg = s switch
{
	FileStream writeableFile when s.CanWrite
		=> "The stream is a file that I can write to.",
	FileStream readOnlyFile
	  => "The stream is a read-only file.",
	MemoryStream ms
		=> "The stream is a memory address",
	null
		=> "The stream is null.",
	_
		=> "The stream is some other type."
}
```

### Iteration Statements

#### Looping with the `foreach` statement

Each item is usually read-only, if the sequence structure is modified during iteration, then an exception will be thrown.

Technically, the `foreach` statement will work on any type that follows these rules:

- The type must have a method named `GetEnumerator` that returns an object
- The returned object must have a property named `Current` and a method named `MoveNext`
- The `MoveNext` method must return `true` if there are more items to enumerate through or `false` if there are no more items.

The compiler turns the `foreach` statement in the preceding example into something similar to the following pseudocode:

```csharp
IEnumerator e = names.GetEnumerator();
while (e.MoveNext)
{
	string name = (string)e.Current; // Current is read-only;
	WriteLine($"{name} has {name.Length} characters");
}
```

### Casting and converting between types

Two varieties: implicit (automatically and safe) and explicit (manually and may lose info).

```csharp
int d = (int)9.8; // 9
```

#### Converting with the `System.Convert` type

```csharp
using static System.Convert;
double g = 9.8;
int h = ToInt32(g);
WriteLine($"g is {g} and h is {h}");  // g is 9.8 and h is 10
```

### Rounding numbers

Using `ToInt()` - Banker's Rounding:

- It will round *up* if the decimal part is the midpoint .5 and the non-decimal part is odd, but it will round *down* if the non-decimal part is even.

```csharp
ToInt(9.5) is 10
ToInt(10.5) is 10
```

#### Taking control of rounding rules

By using the `Round` method of the `Math` class enable the "away from zero" rounding rule (also known as rounding "up" or primary school rule).

```csharp
double[] doubles = new[]{ 9.49, 9.5, 9.51, 10.49, 10.5, 10.51 };
foreach (double n in doubles)
{
	WriteLine(format:
		"Math.Round({0}, 0, MidpointRounding.AwayFromZero) is {1}",
		arg0: n,
		arg1: Math.Round(value: n,
			digits: 0,
			mode: MidpointRounding.AwayFromZero));
}
// Math.Round(9.49, 0, MidpointRounding.AwayFromZero) is 9
// Math.Round(9.5, 0, MidpointRounding.AwayFromZero) is 10
// Math.Round(9.51, 0, MidpointRounding.AwayFromZero) is 10
// Math.Round(10.49, 0, MidpointRounding.AwayFromZero) is 10
// Math.Round(10.5, 0, MidpointRounding.AwayFromZero) is 11
// Math.Round(10.51, 0, MidpointRounding.AwayFromZero) is 11
```

### Converting from any type to a string

All types have a method named `ToString` that inherit from the `System.Object` class.

#### Converting from a binary object to a string

Sometimes you don't want to send the raw bits, because you don't know how those bits could be misinterpreted.

The safest way to do is to convert the binary object (e.g. image or video) into a `string` of safe characters. This is called **Base64** encoding.

```csharp
byte[] binaryObject = new byte[128];

// populate array with random bytes
(new Random()).NextBytes(bianryObject);

WriteLine("Binary Object as bytes:");
for (int idx = 0; idx < binaryObject.Length; idx++)
{
	Write($"{binaryObject[idx]:X} ");  // to format the value using hexadecimal notation
}
WriteLine();

// convert to Base64 string and output as text
string encoded = Convert.ToBase64String(binaryObject);
WriteLine($"Bianry Object as Base64: {encoded}");
```

### Parsing from strings to numbers or dates and times

`Parse` is opposite to `ToString`. Only a few types have a `Parse` method, including all the number types and the `DateTime`.

```csharp
int age = int.Parse("27");
DateTime birthday = DateTime.Parse("4 July 1980");
WriteLine($"My birthday is {birthday}.");   // 04/07/1980 00:00:00
WriteLine($"My birthday is {birthday:D}."); // 04 July 1980

int count = int.Parse("abc");  // => Unhandled Exception
```

#### Avoiding exceptions using the `TryParse` method

It returns `true` or `false`.

```csharp
Write("How many eggs are there? ");
int count;
string input = Console.ReadLine();
if (int.TryParse(intput, out count))
{
	WriteLine($"There are {count} eggs.");
}
else
{
	WriteLine($"I could not parse the input.");
}
```

**Good practice:** Avoid writing code might throw an exception whenever possible, e.g. do `if` checks. But sometimes you can't, you could catch it.

#### Handling exceptions when converting types

##### Catching all exceptions

```csharp
catch (Exception ex)
{
	WriteLine($"{ex.GetType()} says {ex.Message}");
}
```

##### Catching specific exceptions

The order in which you catch exceptions is important.

```csharp
catch (FormationException)
{
	...
}
catch (Exception ex)
{
	...
}
```

### Checking for overflow

#### Throwing overflow exceptions with the checked statement

The `checked` statement tells .NET to throw an exception when an overflow happens.

```csharp
try
{
	checked
  {
    int x = int.MaxValue - 1;
    WriteLine($"Initial value: {x++}");
    WriteLine($"After incrementing: {x++}");
    WriteLine($"After incrementing: {x++}");
    WriteLine($"After incrementing: {x++}");
  }
}
catch (OverflowException)
{
	WriteLine("...")
}

```

#### Disabling compiler overflow checks with the unchecked statement

A related keyword is `unchecked`. This keyword switches off overflow checks performed by compiler within a block of code.


## Function

### Documenting functions with XML comments

Improve the tooltip in VS Code by adding extra information with three '/'(C# XML Documentation Comments extension required).

```csharp
/// <summary>
/// Pass a 32-bit integer and it will be converted into its ordinal equivalent.
/// </summary>
/// <param name="number">Number is a cardinal value e.g. 1, 2, 3, and so on.</param>
/// <returns>Number as on ordinal value e.g. 1st, 2nd, 3rd, and so on.</return>
static string CardinalToOrdinal(int number)
{ ... }
```

### Debugging Functions

#### Customising breakpoints

##### **Edit Breakpoint**:

- Expression - like the `var > 9`. The breakpoint is activated when the expression is `true`.
- Hit Count - You'd have to hit the breakpoint `number` times before it activates
- Log Msg

#### SharpPad extension for VS code

```shell
$ dotnet add package SharpPad
```
Modify Program.cs to import the `SharpPad` and `System.Threading.Tasks` namespaces, change the return type of the Main method to async Task, define a complex object variable, and then `dump` it to the SharpPad window, as shown in the following code:
```csharp
using System;
using SharpPad;
using System.Threading.Tasks; using static System.Console;
namespace Dumping
{
  class Program
  {
    static async Task Main(string[] args)
    {
      var complexObject = new {
        FirstName = "Petr", BirthDate = new DateTime(
        year: 1972, month: 12, day: 25),
        Friends = new[] { "Amir", "Geoff", "Sal" }
      };
      WriteLine($"Dumping {nameof(complexObject)} to SharpPad.");
      await complexObject.Dump();
    }
	}
}

```

### Logging

**Good Practice:** Add code throughout your application to log what is happening, and especially when exceptions occur, so that you can review the logs and use them to trace the issue and fix the problem.

There are two types that can be used to add simple logging to your code:

- `Debug`: used to add logging that gets written during development
- `Trace`: used to add logging that gets written during both development and runtime

#### Instrumenting with Debug and Trace

The `Debug` and `Trace` classes can write to any **[trace listener](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.tracelistener?view=netcore-3.0)**. A trace listener is a type that can be configured to write output anywhere you like when the `Trace.WriteLine` method is called.

You can write your own by inheriting from `TraceListener` type.

##### Writing to the default trace listener

The `DefaultTraceListener` class is configured automatically and writes to VS Code's DEBUG CONSOLE window.

```csharp
using System.Diagnostics;

namespace Instrumenting
{
	class Program
	{
		static void Main(string[] args)
		{
			Debug.WriteLine("Debug says, I am watching!");  // automatically stripped out when you build the release version of App
			Trace.WriteLine("Trace says, I am watching!");  // always there even after release
		}
	}
}
```

##### Configuring trace listeners

We can configure another trace listener that will write to a text file.

```csharp
using System.Diagnostics;
using System.IO;

namespace Instrumenting
{
	class Program
	{
		static void Main(string[] args)
		{
			Trace.Listeners.Add(new TextWriterTraceListener(File.CreateText("log.txt")));
			Trace.AutoFlush = true;  // the default in-memory buffer could be confusing when debugging.
			Debug.WriteLine("Debug syas, I am watching!");
			Trace.WriteLine("Trace syas, I am watching!");
		}
	}
}
```

```shell
$ dotnet run --configuration Release		// avoid logging to console and only the "trace" line will show in file
$ dotnet run --configuration Debug      // both msgs
```

##### Switching trace levels

| Number | Word    | Description                             |
| ------ | ------- | --------------------------------------- |
| 0      | Off     | This will output nothing                |
| 1      | Error   | Output only errors                      |
| 2      | Warning | Output errors and warnings              |
| 3      | Info    | Output errors, warnings and information |
| 4      | Verbose | Output all levels                       |

Some packages needed to enable loading configuration settings from a JSON `appsettings.json` file.

```shell
$ dotnet add package Microsoft.Extensions.Configuration
$ dotnet add package Microsoft.Extensions.Configuration.Binder
$ dotnet add package Microsoft.Extensions.Configuration.Json
$ dotnet add package Microsoft.Extensions.Configuration.FileExtensions
```

Modify `appsettings.json` as:

```csharp
{
	"PacktSwitch": {
		"Level": "Info"   // or "Level": "2"
	}
}
```

```csharp
var builder = new ConfigurationBuilder()
	.SetBasePath(Directory.GetCurrentDirectory())
	.AddJsonFile("appsettings.json", optional: true, reloadOnChange: true);

IConfigurationRoot configuration = builder.Build();

var ts = new TraceSwitch(
	displayName: "PacktSwitch",
	description: "This switch is set via a JSON config.");

configuration.GetSection("PacktSwitch").Bind(ts);

Trace.WriteLineIf(ts.TraceError, "Trace error");
Trace.WriteLineIf(ts.TraceWarning, "Trace warning");
Trace.WriteLineIf(ts.TraceInfo, "Trace information");
Trace.WriteLineIf(ts.TraceVerbose, "Trace verbose");
```

### Unit Testing

- Arrange
- Act
- Assert

## Object-Oriented Programming

Concepts:

- Encapsulation - the combination of the data and actions that are related to an object.
  - Control what and how the internal state of an object can be accessed or modified from the outside
- Composition - about what an object is made of.
- Aggregation - about what can be combined with an object.
  - Two separate objects that are aggregated together to form a new component
  - e.g. A person is not part of a car object, but they could sit in the driver's seat
- Inheritance - about reusing code by having a subclass derive from a base or super class.
- Abstraction - about capturing the core idea of an object and ignore the details of specifics.
  - Base or super classes are often `abstract`
  - Abstraction is a tricky balance. More abstract, more classes would be able to inherit from it, but less functionality to share.
- Polymorphism - about allowing a derived class to override an inherited action to provide custom behaviour.

### Referencing an assembly

```csharp
<ItemGroup>
	<ProjectReference Include="../PacktLibrary/PacktLibrary.csproj" />
</ItemGroup>
```

### Understanding objects

All types ultimately inherit directly or indirectly from a special type named `System.Object`.

### Class

#### Members

##### Field and Property

``` csharp
class Student
{
    public int StuId;      // field

    private int name;
    private int age;
    private bool canWork;  // properties that's dynamically calculated

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

##### Fields

Used to store data. There are three specialised categories of field:

- Constant: The data never changes. The compiler copies the data into any code that reads it.
- Read-only: The data cannot change after the class is instantiated, but the data can be calculated or loaded from an external source at the time of instantiation.
- Event: The data references one or more methods that want to execute when something happens, such as clicking on a button, or responding to a request from other code.

###### Defining fields

```csharp
public class Person : object
{
	// fields
	public string Name;
	public DataTime DateOfBirth;
}
```

###### Storing a value using an enum type

A limited set of options for a value. Internally, it's stored in a lookup table from integer to string.

```csharp
public enum EnumAType
{
	Option1,		// 0
	Option2,		// 1
	Option3			// 2
}
```

```csharp
public Class Person
{
	public EnumAType FavoriteEnumA;
}

bob.FavoriteEnumA = EnumAType.Option2;
```

You can assign int values that are not listed in the enum. They will output as the int value instead of a name since a match will not be found.

###### Storing multiple values using an enum type

We could create a collection of instances of the enum, but there is a better way. We can combine multiple choices into a single value using **flags**, by decorating with `System.Flags` attribute.

```csharp
namespace Packt.Shared
{
  [System.Flags]		// when the value is returned it can automatically match with multiple values as a comma-separated string instead of returning an int value.
  public enum WondersOfTheAncientWorld : byte		// explicitly use byte to save memory
  {
    None 											= 0b_0000_0000, // i.e. 0
    GreatPyramidOfGiza				= 0b_0000_0001, // i.e. 1
    HangingGardensOfBabylon   = 0b_0000_0010, // i.e. 2
    StatueOfZeusAtOlympia		  = 0b_0000_0100, // i.e. 4
    TempleOfArtemisAtEphesus  = 0b_0000_1000, // i.e. 8
    MausoleumAtHalicarnassus  = 0b_0001_0000, // i.e. 16
    ColossusOfRhodes				  = 0b_0010_0000, // i.e. 32
    LighthouseOfAlexandria	  = 0b_0100_0000  // i.e. 64
  }
}
```

If we want to indicate that our bucket list includes the *Hanging Gardens* and *Mausoleum at Halicarnassus* ancient world wonders, then we would want the 16 and 2 bits set to 1. In `Person` class:

```csharp
public WondersOfTheAncientWorld BucketList;

bob.BucketList = WondersOfTheAncientWorld.HangingGardensOfBabylon
								 | WondersOfTheAncientWorld.MausoleumAtHalicarnassus;
// bob.BucketList = (WondersOfTheAncientWorld)18;
WriteLine($"{bob.Name}'s bucket list is {bob.BucketList}");
// => Bob Smith's bucket list is HangingGardensOfBabylon, MausoleumAtHalicarnassus
```

###### Making a field static

- *Instance members:* a different value of each field exists for each instance of the class that is created.
- *Static members*: only has one value that is shared across all instances.

```csharp
namespace Packt.Shared
{
  public class BankAccount
  {
    public string AccountName;					// instance member
    public decimal Balance;							// instance member
    public static decimal InterestRate; // shared member, get by BankAccount.InterestRate
  }
}
```

###### Making a field constant

The value of a field never change. In `Person` class:

```csharp
public const string Species = "Homo Sapien"
```

```csharp
WriteLine($"{bob.Name} is a {Person.Species}"); // To get the val of a constant field, must write the name of the class.

// System.Int32.MaxValue;
// System.Math.PI;
```

###### Making a field read-only

```csharp
public readonly string HomeCountry = "US";					// an instance read-only field
public static readonly string HomePlanet = "Earth";	// read-only field shared across all instances of the type
```

###### Setting fields with default literals

```csharp
public ThingOfDefaults()
{
  Population = default(int); // C# 2.0 and later
  When = default(DateTime);
  Name = default(string);
  People = default(List<Person>);
}
```

```csharp
public ThingOfDefaults()
{
  Population = default; 		// C# 7.1 and later
  When = default;
  Name = default;
  People = default;
}
```

##### Understanding access modifiers

By default it is `internal` for class and `private` for field.

| Access Modifier      | Description                                                  |
| -------------------- | ------------------------------------------------------------ |
| `private`            | Member is accessible inside the type only. (Default)         |
| `internal`           | Member is accessible inside the type and any type in the same assembly. |
| `protected`          | Member is accessible inside the type and any type  that inherits from the type |
| `public`             | Member is accessible everywhere.                             |
| `internal protected` | Member is accessible inside the type, any type in the same assembly, and any type that inherits from the type. Equivalent to a fictional access modifier named `internal_or_protected`. |
| `private protected`  | Member is accessible inside the type, or any type that inherits from the type and is in the same assembly. (Equal to `internal_and_protected`) |

##### Methods

Methods are members of a type that execute a block of statements. There are 4 categories of method:

- Constructor: The statements execute when you use the `new` keyword to allocate memory and instantiate a class.
- Property: The statements execute when you get or set data. Properties are the preferred way to encapsulate fields unless the memory address of the field needs to be exposed.
- Indexer: The statement execute when you get or set data using array syntax `[]`
- Operator: The statements execute when you use an operator like `+` or `/` on operands of your type.

###### Class constructor

```csharp
static ClassName()  // only executed once when the class is loaded
{
	...
}
```

###### Instance constructor

```csharp
public ClassName()  // executed every time a new instance is created
{
    ...
}
```

###### Combining multiple returned values using tuples

```csharp
public (string, int) GetFruit()
{
	return ("Apples", 5);
}

(string, int) fruit = bob.GetFruit();
```

Naming the fields of a tuple

To access the fields of a tuple, the default names are `Item1`,` Item2` and so on. You can explicitly specify the field names.

```csharp
public (string Name, int Number) GetNamedFruit()
{
	return (Name: "Apples", Number: 5);
}

main()
{
  var fruitNamed = bob.GetNamedFruit();
  WriteLine($"There are {fruitNamed.Number} {fruitNamed.Name}.");
}
```

Inferring tuple names:

If you are constructing a tuple from another object, you can use a feature introduced in C# 7.1 called **tuple name inference**.

```csharp
var thing1 = ("Neville", 4);
WriteLine($"{thing1.Item1} has {thing1.Item2} children.");

var thing2 = (bob.Name, bob.Children.Count);
WriteLine($"{thing2.Name} has {thing2.Count} children.");
```

###### Deconstructing tuples

```csharp
// store return value in a tuple variable with two fields (string name, int age) tupleWithNamedFields = GetPerson();
// tupleWithNamedFields.name
// tupleWithNamedFields.age

// deconstruct return value into two separate variables
(string name, int age) = GetPerson();
// name
// age
```

###### Controlling how parameters are passed

- By **value** (default): Think of these as being _in-only_.
- By **referencing** as a _ref_ parameter: Think of these as being _in-and-out_.
  - In C# 7.0 and later, the _ref_ keyword can also be applied to the return value. This allows an external variable to reference an internal variable and modify its value after the method call.
- As an `out` parameter: _out-only_
  - Must be initialised inside the method

#### Splitting classes using partial

When working on large projects with multiple team members, it is useful to be able to split the definition of a complex class across multiple files.

Imagine we want to add statements to the Person class that are automatically generated by a tool like an object-relational mapper that reads schema information from a database. If the class is defined as partial, then we can split the class into a manually edited code file and an auto-generated code file.

```csharp
// In `Person` class
namespace Packt.Shared
{
	public partial class Person
	{
}
  
// In a new file: `PersonAutoGen.cs`
namespace Packt.Shared
{
  public partial class Person
  {
  }
}
```

##### Controlling access with properties

A property is simply a method (or a pair of methods) that acts and looks like a field when you want to get or set a value, thereby simplifying the syntax.

```csharp
// Written in the `PersonAutoGen.cs` file

// a property defined using c# 1-5 syntax
public string Origin
{
	get
	{
		return $"{Name} was born on {HomePlanet}";
	}
}

// two peoperties defined using C# 6+ lambda expression syntax
public string Greeting => $"{Name} says 'Hello'!";

public int Age => System.DateTime.Today.Year - DateOfBirth.Year;
```

###### Settable Properties

```csharp
// In the PersonAutoGen.cs
public string FavoriteIceCream { get; set; }	// auto-syntax

private string favoritePrimaryColor;

public string FavoritePrimaryColor
{
	get
	{
		return favoritePrimaryColor;
	}
	set
	{
		switch (value.ToLower())
		{
			case "red":
			case "green":
			case "blue":
				favoritePrimaryColor = value;
				break;
      default:
      	throw new System.ArgumentException(
      		$"{value} is not a primary color. " +
      		"Choose from: red, green, blue.");
		}
	}
}
```

##### Indexers

Indexers allow the calling code to use the array syntax to access a property.

```
// In the `PersonAutoGen.cs` file
// indexers
public Person this[int index]
{
	get
	{
    return Children[index];
  }
	set {
		Children[index] = value;
	}
}
```

You can overload indexers so that different types can be used for their parameters. For example, as well as passing an int value, you could also pass a string value.