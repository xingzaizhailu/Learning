.default: all

PROGRAMS=week07
CC=c++1y

all: $(PROGRAMS)

clean:
	rm -rf $(PROGRAMS) *.o *.dSYM

week07: Node.o LinkedList.o main.cpp
	g++ -Wall -Werror -std=$(CC) -g -O -o $@ $^

%.o: %.cpp
	g++ -Wall -Werror -std=$(CC) -g -O -c $^
