# [Python](https://docs.python.org/3/tutorial)

## Using the Python Interpreter
By default, Python source files are treated as encoded in UTF-8.  
To declare an encoding other than default one, a special comment line should be added as the first
line:

``` python
    # -*- coding: encoding -*-
```
e.g. Windows-1252 encoding is to be used:

``` python
    # -*- coding: cp-1252 -*-
```

One exception to the first line rule is when the source code starts with a UNIX “shebang” line. In
this case, the encoding declaration should be added as the second line of the file. For example:

``` python
    #!/usr/bin/env python3
    # -*- coding: cp-1252 -*-
```

## An Informal Introduction to Python
### Numbers
Division (/) always returns a float.  

``` shell
    >>> 17 / 3   # classic division returns a float
    5.666666666666667
    >>>
    >>> 17 // 3  # floor division discards the fractional part
    5
    >>> 17 % 3   # the % operator returns the remainder of the division
    2
```

``` python
    -2.33-1j  # 1不可省略
    (-2.33-1j).real # -2.33
    0b1001110 # binary
    0o7643210 # 八进制
    0xA20974BCF # 十六进制
    isinstance(2.33, float)
    type()
```

With Python, it is possible to use the \*\* operator to calculate powers [1]:

``` shell
  >>> 5 ** 2    # 5 squared
  25
```

In interactive mode, the last printed expression is assigned to the variable \_. 

### Strings
Enclosed in single quotes or double quotes.  

The print() function produces a more readable output, by omitting the enclosing quotes and by
printing escaped and special characters:  

``` shell
    >>> '"Isn\'t," she said.'
    '"Isn\'t," she said.'
    >>> print('"Isn\'t," she said.')
    "Isn't," she said.
    >>> s = 'First line.\nSecond line.'  # \n means newline
    >>> s  # without print(), \n is included in the output
    'First line.\nSecond line.'
    >>> print(s)  # with print(), \n produces a new line
    First line.
    Second line.
```

If you don’t want characters prefaced by \ to be interpreted as special characters, you can use raw
strings by adding an r before the first quote:

``` shell
    >>> print('C:\some\name')  # here \n means newline!
    C:\some
    ame
    >>> print(r'C:\some\name')  # note the r before the quote
    C:\some\name
```

String literals can span multiple lines. One way is using triple-quotes: """...""" or '''...'''.
End of lines are automatically included in the string, but it’s possible to prevent this by adding a \ at the end of the line. The following example:  

``` shell
    print("""\
    Usage: thingy [OPTIONS]
         -h                        Display this usage message
         -H hostname               Hostname to connect to
    """)
    # produces the following output (note that the initial newline is not included):

    Usage: thingy [OPTIONS]
         -h                        Display this usage message
         -H hostname               Hostname to connect to
```

Strings can be concatenated (glued together) with the + operator, and repeated with \*:

``` shell
    >>> # 3 times 'un', followed by 'ium'
    >>> 3 * 'un' + 'ium'
    'unununium
    >>> 'Py' 'thon'
    'Python'

    #This only works with two or more literals though, not with variables or expressions:
    >>> prefix = 'Py'
    >>> prefix 'thon'  # can't concatenate a variable and a string literal
    ...
    SyntaxError: invalid syntax
    >>> ('un' * 3) 'ium'
    ...
    SyntaxError: invalid syntax

    >>> prefix + 'thon'
    'Python'

    >>> word = 'Python'
    >>> word[0]  # character in position 0
    'P'
    # Note that since -0 is the same as 0, negative indices start from -1.
    >>> word[-1]  # last character
    'n'

    # slicing
    >>> word[0:2]  # characters from position 0 (included) to 2 (excluded)
    'Py'
    >>> word[2:5]  # characters from position 2 (included) to 5 (excluded)
    'tho'
    >>> word[:2] + word[2:]
    'Python'

    >>> word[42]  # the word only has 6 characters
    Traceback (most recent call last):
    File "<stdin>", line 1, in <module>
    IndexError: string index out of range

    #However, out of range slice indexes are handled gracefully when used for slicing:
    >>> word[4:42]
    'on'
    >>> word[42:]
    ''
```

Python strings cannot be changed — they are immutable.  

``` shell
    >>> word[0] = 'J'
    ...
    TypeError: 'str' object does not support item assignment
    >>> word[2:] = 'py'
    ...
    TypeError: 'str' object does not support item assignment
```

### Lists
Lists might contain items of different types, but usually the items all have the same type.  
All slice operations return a new list containing the requested elements.  

``` shell
    >>> squares = [1, 4, 9, 16, 25]
    >>> squares
    [1, 4, 9, 16, 25]
    >>> squares[:]
    [1, 4, 9, 16, 25]

    >>> squares[-1]
    25
    >>> squares[-3:]  # slicing returns a new list
    [9, 16, 25]

    >>> squares + [36, 49, 64, 81, 100]
    [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```

Unlike strings, which are immutable, lists are a mutable type.  
You can also add new items at the end of the list, by using the append() method.  

``` shell
    >>> cubes.append(216)  # add the cube of 6
    >>> cubes.append(7 ** 3)  # and the cube of 7
    >>> cubes
    [1, 8, 27, 64, 125, 216, 343]
```

Assignment to slices is also possible, and this can even change the size of the list or clear it
entirely:

``` shell
    >>> letters = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
    >>> letters
    ['a', 'b', 'c', 'd', 'e', 'f', 'g']
    >>> # replace some values
    >>> letters[2:5] = ['C', 'D', 'E']
    >>> letters
    ['a', 'b', 'C', 'D', 'E', 'f', 'g']
    >>> # now remove them
    >>> letters[2:5] = []
    >>> letters
    ['a', 'b', 'f', 'g']
    >>> # clear the list by replacing all the elements with an empty list
    >>> letters[:] = []
    >>> letters
    []
```

It is possible to nest lists.

``` shell
    >>> x = [['a', 'b', 'c'], [1, 2, 3]]
    >>> x[0]
    ['a', 'b', 'c']
    >>> x[0][1]
    'b'
```

## More Control Flow Tools
### if Statements
``` shell
    >>> x = int(input("Please enter an integer: "))
    Please enter an integer: 42
    >>> if x < 0:
    ...     x = 0
    ...     print('Negative changed to zero')
    ... elif x == 0:
    ...     print('Zero')
    ... elif x == 1:
    ...     print('Single')
    ... else:
    ...     print('More')
    ...
    More
```

### for Statements
Python’s for statement iterates over the items of any sequence (a list or a string).  

``` shell
    ... words = ['cat', 'window', 'defenestrate']
    >>> for w in words:
    ...     print(w, len(w))
```

If you need to modify the sequence you are iterating over while inside the loop (for example to
duplicate selected items), it is recommended that you first make a copy.   

``` shell
    >>> for w in words[:]:  # Loop over a slice copy of the entire list.
    ...     if len(w) > 6:
    ...         words.insert(0, w)
```
With for w in words:, the example would attempt to create an infinite list, inserting defenestrate
over and over again.

### The range() Function
When you do need to iterate over a sequence of numbers  
range(5): 0 - 4  
range(5, 10): 5 - 9  
range(-10, -100, -30): -10, -40, -70  

``` shell
    >>> for i in range(len(a)):
    ...     print(i, a[i])
```

In most such cases, however, it is convenient to use the enumerate() function, see Looping
Techniques.  

A strange thing happens if you just print a range:  

``` shell
    >>> print(range(10))
    range(0, 10)
```
In many ways the object returned by range() behaves as if it is a list, but in fact it isn’t.
It is an object which returns the successive items of the desired sequence when you iterate over it, but it doesn’t really make the list, thus saving space.  

We have seen that the for statement is such an iterator. The function list() is another; it creates
lists from iterables:  

``` shell
  >>> list(range(5))
  [0, 1, 2, 3, 4]
```

### break and continues Statements, and else Clauses on Loops
Loop statements may have an else clause; it is executed when the loop terminates through exhaustion
of the list (with for) or when the condition becomes false (with while), but not when the loop is
terminated by a break statement.  

``` shell
    >>> for n in range(2, 10):
    ...     for x in range(2, n):
    ...         if n % x == 0:
    ...             print(n, 'equals', x, '*', n//x)
    ...             break
    ...     else:
    ...         # loop fell through without finding a factor
    ...         print(n, 'is a prime number')
    ...
```
A try statement’s else clause runs when no exception occurs,
and a loop’s else clause runs when no break occurs.   

### pass Statements
The pass statement does nothing. It can be used when a statement is required syntactically but the
program requires no action. For example:

``` shell
    >>>
    >>> while True:
    ...     pass  # Busy-wait for keyboard interrupt (Ctrl+C)
    ...
    This is commonly used for creating minimal classes:

    >>>
    >>> class MyEmptyClass:
    ...     pass
    ...
```

Another place pass can be used is as a place-holder for a function or conditional body
when you are working on new code, allowing you to keep thinking at a more abstract
level. The pass is silently ignored:

``` shell
    >>> def initlog(*args):
    ...     pass   # Remember to implement this!
    ...
```

### Defining Functions
``` python
    def func(param):
      pass
```

The execution of a function introduces a new symbol table used for the local variables of the
function. More precisely, all variable assignments in a function store the value in the local symbol table; whereas variable references first look in the local symbol table, then in the local symbol tables of enclosing functions, then in the global symbol table, and finally in the table of built-in names.  
Thus, global variables cannot be directly assigned a value within a function (unless named in a global statement), although they may be referenced.

A function definition introduces the function name in the current symbol table. The value of the
function name has a type that is recognized by the interpreter as a user-defined function. This
value can be assigned to another name which can then also be used as a function. This serves as a
general renaming mechanism:

``` shell
    >>> fib
    <function fib at 10042ed0>
    >>> f = fib
    >>> f(100)
      0 1 1 2 3 5 8 13 21 34 55 89
```
In fact, even functions without a return statement do return a value, albeit a rather boring one.
This value is called None (it’s a built-in name). Writing the value None is normally suppressed by
the interpreter if it would be the only value written. You can see it if you really want to using
print():

``` shell
    >>> fib(0)
    >>> print(fib(0))
    None
```
Falling off the end of a function also returns None. 

### More on Defining Functions
#### Default Argument Values
# TODOOOO


#### Keyword Arguments
#### Arbitrary Argument Lists
#### Unpacking Argument Lists
#### Lambda Expressions
#### Documentation Strings
#### Function Annotations
### Intermezzo: Coding Style
## Data Structures
5.1. More on Lists
5.1.1. Using Lists as Stacks
5.1.2. Using Lists as Queues
5.1.3. List Comprehensions
5.1.4. Nested List Comprehensions
5.2. The del statement
5.3. Tuples and Sequences
5.4. Sets
5.5. Dictionaries
5.6. Looping Techniques
5.7. More on Conditions
5.8. Comparing Sequences and Other Types
## Modules
6.1. More on Modules
6.1.1. Executing modules as scripts
6.1.2. The Module Search Path
6.1.3. “Compiled” Python files
6.2. Standard Modules
6.3. The dir() Function
6.4. Packages
6.4.1. Importing * From a Package
6.4.2. Intra-package References
6.4.3. Packages in Multiple Directories
## Input and Output
7.1. Fancier Output Formatting
7.1.1. Old string formatting
7.2. Reading and Writing Files
7.2.1. Methods of File Objects
7.2.2. Saving structured data with json
## Errors and Exceptions
8.1. Syntax Errors
8.2. Exceptions
8.3. Handling Exceptions
8.4. Raising Exceptions
8.5. User-defined Exceptions
8.6. Defining Clean-up Actions
8.7. Predefined Clean-up Actions
## Classes
9.1. A Word About Names and Objects
9.2. Python Scopes and Namespaces
9.2.1. Scopes and Namespaces Example
9.3. A First Look at Classes
9.3.1. Class Definition Syntax
9.3.2. Class Objects
9.3.3. Instance Objects
9.3.4. Method Objects
9.3.5. Class and Instance Variables
9.4. Random Remarks
9.5. Inheritance
9.5.1. Multiple Inheritance
9.6. Private Variables
9.7. Odds and Ends
9.8. Iterators
9.9. Generators
9.10. Generator Expressions
## Brief Tour of the Standard Library
10.1. Operating System Interface
10.2. File Wildcards
10.3. Command Line Arguments
10.4. Error Output Redirection and Program Termination
10.5. String Pattern Matching
10.6. Mathematics
10.7. Internet Access
10.8. Dates and Times
10.9. Data Compression
10.10. Performance Measurement
10.11. Quality Control
10.12. Batteries Included
## Brief Tour of the Standard Library — Part II
11.1. Output Formatting
11.2. Templating
11.3. Working with Binary Data Record Layouts
11.4. Multi-threading
11.5. Logging
11.6. Weak References
11.7. Tools for Working with Lists
11.8. Decimal Floating Point Arithmetic
## Virtual Environments and Packages
12.1. Introduction
12.2. Creating Virtual Environments
12.3. Managing Packages with pip
13. What Now?
14. Interactive Input Editing and History Substitution
14.1. Tab Completion and History Editing
14.2. Alternatives to the Interactive Interpreter
15. Floating Point Arithmetic: Issues and Limitations
15.1. Representation Error
16. Appendix
16.1. Interactive Mode
16.1.1. Error Handling
16.1.2. Executable Python Scripts
16.1.3. The Interactive Startup File
16.1.4. The Customization Modules

//
233 == 233.0 # True
'233' == 233 # False
