
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

CFLAGS=-std=c99 -ggdb -DDEBUG
# CFLAGS += -D_GNU_SOURCE
CFLAGS += -D_POSIX_C_SOURCE=200809L
CFLAGS += -Wall -Werror # -pedantic -Wno-unused-result -Wuninitialized -Wmaybe-uninitialized -Winit-self -Wunused-result -Wunused
CFLAGS += -MD -MP
CFLAGS += `sdl2-config --cflags`
CFLAGS += `pkg-config --cflags cairo poppler-glib pangocairo`
LFLAGS= -lm -lreadline -lgc -lSDL2_net -lSDL2_image -lSDL2_mixer
LFLAGS += `sdl2-config --libs`
LFLAGS += `pkg-config --libs cairo poppler-glib pangocairo`

SRC = $(wildcard *.c)
SRC += $(wildcard lisp/*.c)
SRC += $(wildcard libs/*/*.c)
TESTS_SRC = $(filter-out main.c, $(SRC) $(wildcard tests_src/*.c))


all: worker test Makefile linecount

worker: $(SRC:%.c=%.o)
	cc $(LFLAGS) -o $@ $^

tests: $(TESTS_SRC:%.c=%.o)
	cc $(LFLAGS) -o $@ $^

test: tests
	timeout 300s ./tests && timeout 30s ./lisp_tests/lisp_test.py

clean:
	rm -f *.o *.d worker    lisp/*.o lisp/*.d    tests_src/*.o tests_src/*.d tests

linecount:
	echo -n "linecount: " && cat *.c *.h */*.c */*.h */*.py */*.zlt | wc -l

release:
	gcc $(CFLAGS) $(LFLAGS) $(SRC) -UDEBUG -UOPTION_DEBUG -g0 -O3 -o worker

.PHONY: all clean linecount release

-include .dummy
.dummy: Makefile
	@touch $@
	@$(MAKE) -s clean

-include $(SRC:%.c=%.d) $(TESTS_SRC:%.c=%.d)