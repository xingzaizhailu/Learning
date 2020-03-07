# [Python](https://docs.python.org/3/tutorial)

## [Liaoxue Feng Python](https://www.liaoxuefeng.com/wiki/0014316089557264a6b348958f449949df42a6d3a2e542c000)
Run
        python hello.py
To run python code directly on Mac and Linux, add this to the first line:

        #!/usr/bin/env python3
        print('hello world')
Grant authority:

        $ chmod a+x hello.py
        $ ./hello.py

## Using the Python Interpreter
By default, Python source files are treated as encoded in UTF-8.  
To declare an encoding other than default one, a special comment line should be added as the first
line:

``` python
    # -*- coding: <encoding> -*-
```
e.g. Windows-1252 encoding is to be used:

``` python
    # -*- coding: cp-1252 -*-
    or utf-8
    # -*- coding: utf-8 -*-
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

#### String and Encoded
    >>> ord('A')
    65
    >>> ord('中')
    20013
    >>> chr(66)
    'B'
#### Bytes
    b'ABC'
    
    >>> 'ABC'.encode('ascii')
    b'ABC'
    >>> '中文'.encode('utf-8')
    b'\xe4\xb8\xad\xe6\x96\x87'
    >>> b'ABC'.decode('ascii')
    'ABC'
    >>> b'\xe4\xb8\xad\xff'.decode('utf-8', errors='ignore')
    '中'
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

#### about immutable
    >>> a = ['c', 'b', 'a']
    >>> a.sort()
    >>> a
    ['a', 'b', 'c']
    
    >>> a = 'abc'
    >>> b = a.replace('a', 'A')
    >>> b
    'Abc'
    >>> a
    'abc'
### Lists

Lists might contain items of different types, but usually the items all have the same type.  
All slice operations return a new list containing the requested elements.  

        >>> classmates = ['a', 'b', 'c']
        >>> temp1 = classmates # give another name to `classmates`
        >>> temp2 = classmates[:] # have a copy of `classmates`
        >>> len(classmates)
        3
    
        >>> classmates.insert(1, 'Jack')
        >>> classmates.pop()
        'd'
        >>> classmates.pop(1)
        Jack
        >>> classmates.extend(['e', 'f'])
        >>> L = ['Apple', 123, True, ['a', 'b']]
        >>> classmates + ['g','h']
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

	>>> squares[:10:2]  # get every 2 elements of first 10 elements
	>>> squares[::5]   # get every 5 elements of all elements

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
when x is not 0, '', []. condition will be Ture, else False

        if x:
            print('True')


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

```
	def power(x, n=2):
		pass
```

#### only use immutable object as default value
        def add_end(L=[]):
            L.append('END')
            return L
    
        >>> add_end()
        ['END']
        >>> add_end()
        ['END', 'END']
        >>> add_end()
        ['END', 'END', 'END']`

update:

        def add_end(L=None):
            if L is None:
                L = []
            L.append('END')
            return L
#### Keyword Arguments

consider params as a dict

    def person(name, age, **kw):
        print('name:', name, 'age:', age, 'other:', kw)
    
    >>> person('Michael', 30)
    name: Michael age: 30 other: {}
    >>> person('Adam', 45, gender='M', job='Engineer')
    name: Adam age: 45 other: {'gender': 'M', 'job': 'Engineer'}
    
    >>> extra = {'city': 'Beijing', 'job': 'Engineer'}
    >>> person('Jack', 24, **extra)
    name: Jack age: 24 other: {'city': 'Beijing', 'job': 'Engineer'}
##### limited keyword params
    def person(name, age, *, city, job):
        print(name, age, city, job)
#### Arbitrary Argument Lists 

    	def calc(*numbers):
            sum = 0
            for n in numbers:
                sum = sum + n * n
            return sum
    	>>> calc(1, 2, 3) # consider params as a tuple
    
        >>> nums = [1, 2, 3]
        >>> calc(*nums)
        14
#### Lambda Expressions

#### sequence of params
必选参数、默认参数、可变参数、命名关键字参数和关键字参数。

    def f1(a, b, c=0, *args, **kw):
        print('a =', a, 'b =', b, 'c =', c, 'args =', args, 'kw =', kw)
    
    def f2(a, b, c=0, *, d, **kw):
        print('a =', a, 'b =', b, 'c =', c, 'd =', d, 'kw =', kw)
    
    >>> f1(1, 2)
    a = 1 b = 2 c = 0 args = () kw = {}
    >>> f1(1, 2, c=3)
    a = 1 b = 2 c = 3 args = () kw = {}
    >>> f1(1, 2, 3, 'a', 'b')
    a = 1 b = 2 c = 3 args = ('a', 'b') kw = {}
    >>> f1(1, 2, 3, 'a', 'b', x=99)
    a = 1 b = 2 c = 3 args = ('a', 'b') kw = {'x': 99}
    >>> f2(1, 2, d=99, ext=None)
    a = 1 b = 2 c = 0 d = 99 kw = {'ext': None}
最神奇的是通过一个tuple和dict，你也可以调用上述函数：

    >>> args = (1, 2, 3, 4)
    >>> kw = {'d': 99, 'x': '#'}
    >>> f1(*args, **kw)
    a = 1 b = 2 c = 3 args = (4,) kw = {'d': 99, 'x': '#'}
    >>> args = (1, 2, 3)
    >>> kw = {'d': 88, 'x': '#'}
    >>> f2(*args, **kw)
    a = 1 b = 2 c = 3 d = 88 kw = {'x': '#'}
所以，对于任意函数，都可以通过类似func(\*args, \*\*kw)的形式调用它，无论它的参数是如何定义的。

#### Documentation Strings
#### Function Annotations
### Intermezzo: Coding Style
## Data Structures
### Tuple
immutable

    >>> classmates = ('Michael', 'Bob', 'Tracy')
#### empty tuple

    >>> t = ()
    >>> t
    ()
#### one element
    >>> t = (1) # wrong!
    >>> t
    1
    
    >>> t = (1,)
    >>> t
    (1,)

#### 'mutable' tuple
    >>> t = ('a', 'b', ['A', 'B'])
    >>> t[2][0] = 'X'
    >>> t[2][1] = 'Y'
    >>> t
    ('a', 'b', ['X', 'Y'])

### Dictionaries

The key of Dict must be an immutable type (string, number or tuple)  

        >>> d = {'Michael': 95, 'Bob': 75, 'Tracy': 85}
        >>> d['Adam'] = 67
        >>> d['Adam']
        >>> d.keys()
        >>> d.values()
        >>> len(d)
#### avoid key not exist err
        >>> 'Thomas' in d
        False
    
        >>> d.get('Thomas') # return None
        >>> d.get('Thomas', -1)
        -1
### Set

        >>> s = set([1, 1, 2, 3])
        >>> s
        {1, 2, 3}
    
        >>> s.add(4)
        >>> s.remove(4)
    
        >>> 1 in s
        TRUE
#### operate
        >>> s1 = set([1, 2, 3])
        >>> s2 = set([2, 3, 4])
        >>> s1 - s2
        >>> s1 & s2
        {2, 3}
        >>> s1 | s2
        {1, 2, 3, 4}
### Looping
while ...:

        sum = 0
        n = 99
        while n > 0:
            sum = sum + n
                n = n - 2
5.8. Comparing Sequences and Other Types

## Functional Programming
### Return a function
### Partial
Fix partial of the parameters for a function.

```
    >>> int2 = functools.partial(int, base=2)
    >>> int2('1000000')
    64
```

## Modules

6.1. More on Modules
6.1.1. Executing modules as scripts
6.1.2. The Module Search Path
6.1.3. “Compiled” Python files
6.2. Standard Modules
6.3. The dir() Function

### Package
1. Create a folder to store related modules, folder name is the package name
2. Create an __init__.py file under the folder, could be empty
import by the expression:
```
    import package.module
```
#### Packages in Multiple Directories

## Input and Output

    name = input('plz input your name: ')
    print('hello', name)
    
    >>> print('\\\t\\')
    \       \
    >>> print(r'\\\t\\')  # ignore
    \\\t\\
    
    >>> print('''line1
        ... line2
        ... line3''')
### Fancier Output Formatting

    >>> 'Hi, %s, you have $%d.' % ('Michael', 1000000)
    'Hi, Michael, you have $1000000.'
    
    >>> print('%2d-%02d' % (3, 1))
     3-01
    >>> print('%.2f' % 3.1415926)
    3.14
    
    >>> 'growth rate: %d %%' % 7
    'growth rate: 7 %'
7.2. Reading and Writing Files
7.2.1. Methods of File Objects
7.2.2. Saving structured data with json

## Errors and Exceptions
8.1. Syntax Errors
### Exception
```
    try:
        expressions
    except Exception [as reason]:
        dealing Exception
    finally:
        must be executed with or without exception
```

### Types of exception

8.3. Handling Exceptions
8.4. Raising Exceptions
8.5. User-defined Exceptions
8.6. Defining Clean-up Actions
8.7. Predefined Clean-up Actions
## Classes
### Private attribute
```
    class Person:
        __name = 'Private name'

        def getName(self):
            return self.__name
    >> p = Person()
    >> p.getName()
    >> p._Person__name  # so it's only name mapping, fake private
```
### Limit the attributes that can be added into instance

```
class Student(object):
	# list attributes that's allowed to be added at running
	__slots__ = ('name', 'age')
```

Note: '\_\_slots\_\_' only works for instances of current class, not work for inherent classes, unless  it's '\_\_slots\_\_' of base class is merged into itself.

### Decorator

```
    import functools

    def log(func):
        @functools.wraps(func)
        def wrapper(*args, **kw):
            print('call %s():' % func.__name__)
            return func(*args, **kw)
        return wrapper

    @log
    def now():
        print('2015-3-25')   # like: now = log(now), without functools.wraps now.__name__ == 'wrapper'
```

A decorator with parameters

```
    import functools

    def log(text):
        def decorator(func):
            @functools.wraps(func)
            def wrapper(*args, **kw):
                print('%s %s():' % (text, func.__name__))
                return func(*args, **kw)
            return wrapper
        return decorator
    >>> now = log('execute')(now)
```

```
class Student(object):

    @property         # make this function a getter
    def score(self):  # this function will be called when stu.score is called.
        return self._score

    @score.setter     # make this function a setter
    def score(self, value):
        if not isinstance(value, int):
            raise ValueError('score must be an integer!')
        if value < 0 or value > 100:
            raise ValueError('score must between 0 ~ 100!')
        self._score = value
```



9.5.1. Multiple Inheritance

MixIn

```
class Dog(Mammal, RunnableMixIn, CarnivorousMixIn):
    pass
```

9.6. Private Variables
9.7. Odds and Ends

### type()

The definition of a class is established at running time in python. `type()` function can either return an object type or return a new created type (at running time).

```
>>> def fn(self, name='world'): print('Hello, %s.' % name)
#type(class name, base classes in tuple, bind func fn to hello)
>>> Hello = type('Hello', (object,), dict(hello=fn))  # create Hello class.
>>> h = Hello()
>>> h.hello()
Hello, world.
>>> print(type(Hello))
<class 'type'>
>>> print(type(h))
<`class '__main__.Hello'>
```

### Metaclass

可以把类看成是metaclass创建出来的实例

```
# metaclass是类的模板，所以必须从type类派生
class ListMetaclass(type):
	def __new__(cls, name, base, attrs):
		attrs['add'] = lamda self, value: self.append(value)
		return type.__new__(cls, name, basess, attrs)

# 
class MyList(list, metaclass=ListMetaclass):
	pass
```

TODO: too hard

### Magic functions

- _\_str\_\_ :is for print & \_\_repr\_\_ is for debug

- \_\_iter\_\_: for ... in

- \_\_getitem\_\_: get by index. but you need to handle for int, [3:5], [:,5], [:5:2], [-2]

- \_\_getattr\_\_: will be called only when the attributes not exist

  - ```
    class Student(object):
    
        def __getattr__(self, attr):
            if attr=='age':
                return lambda: 25
            raise AttributeError('\'Student\' object has no attribute \'%s\'' % attr)
    ```

- \_\_call\_\_: an_instance()/ callable()

### Iteration

        for key in dict:
            expression
    
        for value in dict.values():
            expression
    
        for k, v in dict.items():
            expression

Check if a data structure is iterable:

```
	>>> from collections import Iterable
    >>> isinstance('abc', Iterable)
    True
    >>> isinstance(123, Iterable)
    False
    >>> for i, value in enumerate(['A', 'B', 'C']):
    ...     print(i, value)
    ...
    0 A
    1 B
    2 C
```



9.8. Iterators

### Generator

```
    >>> g = (x * x for x in range(10))
    >>> g
    <generator object <genexpr> at 0x1022ef630>
    >>> next(g)
```

Key word: `yield`

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


