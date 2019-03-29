# Makefile
## Why makefile
- When too many files need to be compiled
- No need to re-compile all of them when only one or a few files changes
- Move to a new machine
## Template
```makefile
    # Comment
    Target1: [dependencies]
        Command

    ...

    Target2: [dependencies]
        Command
```

## Example
```makefile
    # V1
    all: hello

    hello: main.o func1.o func2.o
        gcc main.o func1.o func2.o -o hello

    main.o: main.cpp
        gcc -c main.cpp

    func1.o: func1.cpp
        gcc -c func1.cpp

    func2.o: func2.cpp
        gcc -c func2.cpp

    clear:
        rm *.o
```

```makefile
    # V2
    CC=gcc
    CFLAGS=-c -Wall

    all: hello

    hello: main.o func1.o func2.o
        $(CC) main.o func1.o func2.o -o hello

    main.o: main.cpp
        $(CC) $(CFLAGS) main.cpp

    func1.o: func1.cpp
        $(CC) $(CFLAGS) func1.cpp

    func2.o: func2.cpp
        $(CC) $(CFLAGS) func2.cpp

    clear:
        rm *.o
```
