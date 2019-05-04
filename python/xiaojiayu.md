# File
# Exception
```
    try:
        expressions
    except Exception [as reason]:
        dealing Exception
    finally:
        must be executed with or without exception
```

## Types of exception

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
# Class
# and Object


