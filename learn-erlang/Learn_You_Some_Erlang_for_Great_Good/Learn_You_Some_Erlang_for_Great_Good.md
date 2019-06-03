# [Learn You Some Erlang for Great Good](http://learnyousomeerlang.com/content, "Online Version")
> Bad at : image, signal processing, OS device drivers
> Good at: large software, web servers, real-time bidding, distributed db impl, lifting coupled with other lang, higher-leverl protocol impl
## Chap1: Starting Out
### Shell
1. `q()` to exit
2. if shell freezes: `^G i Enter c Enter`

### Numbers
``` shell
    > 5/2
    2.5
    > 5 div 2
    2
    > 5 rem 2
    1
```
`Base#value`

    2#101010 -> 42

### Invariable Variable
Start with Upcase or `_`(only when it is unneccessary).  
erase value: `f(Variable)`  
erase all  : `f()`  

### Atom
inside `''` when no start with a low-case or contains other characters like `_` or `@`
e.g.
    
``` erlang
    'Atoms can be cheated!'
    atom = 'atom'.
```

# TODO
### Boolean
### Tuple
### Lists
### List Comprehensions
### Bit

## Chap2: Module


### Chap5: Hello Recursion

