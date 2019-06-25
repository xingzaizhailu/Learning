# **G**NU **D**e**b**ugger
## Start GDB
```
    $ gdb
    (gdb) run
    // or
    $ gdb executable
    (gdb) r [command-line arguments] // short for run
```

## Breakpoint
```
    (gdb) break [function name, line number]        // or b
    (gdb) next              // or n, will step forward one block of code
    (gdb) step              // or s, will step forward one line of code
    (gdb) list              // or l, to show source code
    (gdb) print variable    // or p

    (gdb) b filename:line_num
    (gdb) continue      // or c
    (gdb) info locals

    (gdb) bt                // shows you what series of funcs calls led you to
    the current point.
    (gdb) quit              // or q, quits GDB
```

### Reverse
```
    (gdb) reverse-continue
    (gdb) reverse-step
    (gdb) reverse-next
```
