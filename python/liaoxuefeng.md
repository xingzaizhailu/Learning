# [Python 3.6](https://www.liaoxuefeng.com/wiki/0014316089557264a6b348958f449949df42a6d3a2e542c000)
Run

    python hello.py
To run python code directly on Mac and Linux, add this to the first line:

    #!/usr/bin/env python3
    print('hello world')
Grant authority:

    $ chmod a+x hello.py
    $ ./hello.py

## Basics
### I/O
    name = input('plz input your name: ')
    print('hello', name)

    >>> print('\\\t\\')
    \       \
    >>> print(r'\\\t\\')  # ignore
    \\\t\\

    >>> print('''line1
        ... line2
        ... line3''')
#### formatted output
    >>> 'Hi, %s, you have $%d.' % ('Michael', 1000000)
    'Hi, Michael, you have $1000000.'

    >>> print('%2d-%02d' % (3, 1))
     3-01
    >>> print('%.2f' % 3.1415926)
    3.14

    >>> 'growth rate: %d %%' % 7
    'growth rate: 7 %'

### Data structure & variant
#### None
#### division
    >>> 10 / 3
    3.3333333333333335
    >>> 10 // 3
    3
    >>> 9 / 3
    3.0

### String and Encoded
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

#### UTF-8 Encoded
To read code as UTF-8 encoded:

``` python
    # -*- coding: utf-8 -*-
```

### List & Tuple
#### List
    >>> classmates = ['a', 'b', 'c']
    >>> temp1 = classmates # give another name to `classmates`
    >>> temp2 = classmates[:] # have a copy of `classmates`
    >>> len(classmates)
    3
    >>> classmates[-1]
    c
    >>> classmates.append('d')
    >>> classmates.insert(1, 'Jack')
    >>> classmates.pop()
    'd'
    >>> classmates.pop(1)
    Jack
    >>> classmates.extend(['e', 'f'])
    >>> L = ['Apple', 123, True, ['a', 'b']]
    >>> classmates + ['g','h']
#### Tuple
immutable

    >>> classmates = ('Michael', 'Bob', 'Tracy')
##### empty tuple
    >>> t = ()
    >>> t
    ()
##### one element
    >>> t = (1) # wrong!
    >>> t
    1

    >>> t = (1,)
    >>> t
    (1,)

##### 'mutable' tuple
    >>> t = ('a', 'b', ['A', 'B'])
    >>> t[2][0] = 'X'
    >>> t[2][1] = 'Y'
    >>> t
    ('a', 'b', ['X', 'Y'])

### Condition
    if <condition1>:
        <expression1>
    elif <condition2>:
        <expression2>
    elif <condition3>:
        <expression3>
    else:
        <expression4>

when x is not 0, '', []. condition will be Ture, else False

    if x:
        print('True')

### Loop
for ... in ...:

    sum = 0
    for x in range(101):
          sum = sum + x

while ...:

    sum = 0
    n = 99
    while n > 0:
        sum = sum + n
            n = n - 2
#### break and continue
### Dict & Set
The key of Dict must be an immutable type (string, number or tuple)  

    >>> d = {'Michael': 95, 'Bob': 75, 'Tracy': 85}
    >>> d['Adam'] = 67
    >>> d['Adam']
    >>> d.keys()
    >>> d.values()
    >>> len(d)
##### avoid key not exist err
    >>> 'Thomas' in d
    False

    >>> d.get('Thomas') # return None
    >>> d.get('Thomas', -1)
    -1

#### Set
without values

    >>> s = set([1, 1, 2, 3])
    >>> s
    {1, 2, 3}

    >>> s.add(4)
    >>> s.remove(4)

    >>> 1 in s
    TRUE

##### operate
    >>> s1 = set([1, 2, 3])
    >>> s2 = set([2, 3, 4])
    >>> s1 - s2
    >>> s1 & s2
    {2, 3}
    >>> s1 | s2
    {1, 2, 3, 4}

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

## Functions
### Call functions
#### Type cast
    >>> int('123')
    123
    >>> int(12.34)
    12
    >>> float('12.34')
    12.34
    >>> str(1.23)
    '1.23'
    >>> str(100)
    '100'
    >>> bool(1)
    True
    >>> bool('')
    Fals
#### alias
    >>> a = abs
    >>> a(-1)
    1
### define functions
    def my_abs(x):
          if not isinstance(x, (int, float)):   # parameter type check
              raise TypeError('bad operand type')
          if x >= 0:
              return x
          else:
              return -x
default return None
#### Empty function
    def nop():
        pass
### Parameters
#### default value
    def power(x, n=2):
        s = 1
        while n > 0:
            n = n - 1
            s = s * x
        return s
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

#### variable params number
    def calc(*numbers):
        sum = 0
        for n in numbers:
            sum = sum + n * n
        return sum

    >>> calc(1, 2, 3) # consider params as a tuple

    >>> nums = [1, 2, 3]
    >>> calc(*nums)
    14
#### keyword params
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

##### sequence of params
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

### Recursion
    def fact(n):
        if n==1:
            return 1
        return n * fact(n - 1)
stack overflow!

#### Tail Recursion
    def fact(n):
        return fact_iter(n, 1)

    def fact_iter(num, product):
        if num == 1:
            return product
        return fact_iter(num - 1, num * product)

## Advanced Features
### Slice
    >>> list[0:3]   # get values of index 0, 1, 2
    >>> list[:3]    # get values of index 0, 1, 2
    >>> list[-3:]   # get last 3 elements
    >>> list[:10:2] # get every 2 elements of first 10 elements
    >>> list[::5]   # get every 5 element of all elements
So does to tuple and string.
### Iteration
    for key in dict:
        expression

    for value in dict.values():
        expression

    for k, v in dict.items():
        expression
Check if a data structure is iterable:
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

### List Comprehensions
