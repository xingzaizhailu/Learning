# else
When the loop did not encounter a `break` statement.  
``` python
    for n in range(2, 10):
        for x in range(2, n):
            if n % x == 0:
                print( n, 'equals', x, '*', n/x)
                break
        else:
            # loop fell through without finding a factor
            print(n, 'is a prime number')
```

``` python
    try:
        expressions
    except Exception [as reason]:
        print(str(reason))
    else:
        print('no exception')
```
# with
```
    with open("x.txt") as f:
        data = f.read()
        do something with data
```


## BIF
- A class is a subclass of itself. issubclass/ isinstance
- hasattr(object, name)/ getattr(object, name[, default])/ setattr(object, name, name)/ delattr(object, name)
- property(fget=None, fset=None, fdel=None, doc=None)


# Magic functions
- __new__(cls[, ...]): the first function been called. 
    ```
       class CapStr(str): # str is immutable
           def __new__(cls, string):
               string = string.upper()
               return str.__new__(cls, string) # return an object
    ```
- __init__(self[, ...])
- __del__(self)
    - del x != x.__del__()
    - it is called automatically when the object is released by garbage collection

## Factory functions
__add__, __sub__, __mul__ , __truediv__ , __floordiv__ , __mod__ , __divmod__ , __pow__ , __lshift__ , __rshift__ , __and__ , __xor__ , __or__

反运算符, 增量操作符，一元操作符, 类型转换

## attributes
```
    class C:
        def __getattribute__(self, name):
            print("getattribute")
            return super().__getattribute__(name)
        def __getattr__(self, name):
            print("getattr")
        def __setattr__(self, name, value):
            print("setattr")
            super().__setattr__(name, value)
        def __delattr__(self, name):
            print("delattr")
            super().__delattr__(name)
    >>> c = C()
    >>> c.x
    getattribute
    getattr
    >>> c.x = 1
    setattr
    >>> c.x
    getattribute
    1
    >>> del c.x
    delattr
```

## Descriptor
```
    class MyDescriptor:
        def __get__(self, instance, owner):
            print("getting...", self, instance, owner)
        def __set__(self, instance, value):
            print("setting...", self, instance, value)
        def __delete__(self, instance):
            print("deleting...", self, instance)
    >>> test = Test()
    >>> test.x
    >>> test
    >>> Test
    >>> test.x = 'X-man'
    >>> del test.x
```
Have a look of the implementation of property in doc.
