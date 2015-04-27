
# CXXFLAGS=-std=c99 -Wall -Werror -pedantic -Wno-unused-result -ggdb -DDEBUG

# # CXXFLAGS=-I. -I/usr/local/include -std=c99 -Wall -Werror -pedantic -Wno-unused-result -Wextra
# LDFLAGS=-lglut -lGLEW -lGL -lm

# HEADERS=debug.h types.h basic.h list.h hash.h string.h memory.h symbol.h read.h print.h eval.h

# all: repl Makefile

# basic.o: basic.c ${HEADERS}
# 	gcc ${CXXFLAGS} -c basic.c

# list.o: list.c ${HEADERS}
# 	gcc ${CXXFLAGS} -c list.c

# hash.o: hash.c ${HEADERS}
# 	gcc ${CXXFLAGS} -c hash.c

# string.o: string.c ${HEADERS}
# 	gcc ${CXXFLAGS} -c string.c

# memory.o: memory.c ${HEADERS}
# 	gcc ${CXXFLAGS} -c memory.c

# symbol.o: symbol.c ${HEADERS}
# 	gcc ${CXXFLAGS} -c symbol.c


# read.o: read.c ${HEADERS}
# 	gcc ${CXXFLAGS} -c read.c

# print.o: print.c ${HEADERS}
# 	gcc ${CXXFLAGS} -c print.c

# eval.o: eval.c ${HEADERS}
# 	gcc ${CXXFLAGS} -c eval.c


# repl.o: repl.c ${HEADERS}
# 	gcc ${CXXFLAGS} -c repl.c

# clean:
# 	rm -f *.o repl

# repl: basic.o list.o hash.o string.o memory.o symbol.o read.o print.o eval.o repl.o

# .PHONY: all clean

CFLAGS=-std=c99 -Wall -Werror -pedantic -Wno-unused-result -ggdb -DDEBUG
CFLAGS += -MD -MP

SRC = $(wildcard *.c)
SRC += $(wildcard lisp/*.c)
TESTS_SRC = $(filter-out repl.c, $(SRC) $(wildcard tests_src/*.c))


all: repl test Makefile

repl: $(SRC:%.c=%.o)
	cc -o $@ $^

clean:
	rm -f *.o *.d repl    tests_src/*.o tests_src/*.d tests

tests: $(TESTS_SRC:%.c=%.o)
	cc -o $@ $^

test: tests
	timeout 30s ./tests

.PHONY: all clean

-include $(SRC:%.c=%.d) $(TESTS_SRC:%.c=%.d)
