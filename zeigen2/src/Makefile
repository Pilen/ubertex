
CXXFLAGS=-std=c99 -Wall -Werror -pedantic -Wno-unused-result -DDEBUG

# CXXFLAGS=-I. -I/usr/local/include -std=c99 -Wall -Werror -pedantic -Wno-unused-result -Wextra
LDFLAGS=-lglut -lGLEW -lGL -lm

HEADERS=debug.h ztypes.h zbasic.h zlist.h zhash.h zstring.h zmemory.h zsymbol.h zread.h zprint.h zeval.h

all: repl

zbasic.o: zbasic.c ${HEADERS}
	gcc ${CXXFLAGS} -c zbasic.c

zlist.o: zlist.c ${HEADERS}
	gcc ${CXXFLAGS} -c zlist.c

zhash.o: zhash.c ${HEADERS}
	gcc ${CXXFLAGS} -c zhash.c

zstring.o: zstring.c ${HEADERS}
	gcc ${CXXFLAGS} -c zstring.c

zmemory.o: zmemory.c ${HEADERS}
	gcc ${CXXFLAGS} -c zmemory.c

zsymbol.o: zsymbol.c ${HEADERS}
	gcc ${CXXFLAGS} -c zsymbol.c


zread.o: zread.c ${HEADERS}
	gcc ${CXXFLAGS} -c zread.c

zprint.o: zprint.c ${HEADERS}
	gcc ${CXXFLAGS} -c zprint.c

zeval.o: zeval.c ${HEADERS}
	gcc ${CXXFLAGS} -c zeval.c


clean:
	rm -f *.o repl

repl: zbasic.o zlist.o zhash.o zstring.o zmemory.o zsymbol.o zread.o zprint.o zeval.o

.PHONY: all clean
