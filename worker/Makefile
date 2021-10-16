CFLAGS=-std=c99 -ggdb -DDEBUG # -g -rdynamic
# CFLAGS += -D_GNU_SOURCE
CFLAGS += -D_POSIX_C_SOURCE=200809L
CFLAGS += -Wall -Wextra -Werror # -pedantic -Wno-unused-result -Wuninitialized -Wmaybe-uninitialized -Winit-self -Wunused-result -Wunused
CFLAGS += -MD -MP
CFLAGS += `sdl2-config --cflags`
CFLAGS += `pkg-config --cflags cairo poppler-glib pangocairo`
LFLAGS= -lm -lreadline -lgc -lSDL2_net -lSDL2_image -lSDL2_mixer
LFLAGS += `sdl2-config --libs`
LFLAGS += `pkg-config --libs cairo poppler-glib pangocairo`

SRC = $(wildcard *.c)
SRC += $(wildcard lisp/*.c)
SRC += libs/cairosdl/cairosdl.c
SRC += libs/pcg-c-basic-0.9/pcg_basic.c
# SRC += $(wildcard libs/*/*.c)
TESTS_SRC = $(filter-out main.c, $(SRC) $(wildcard tests_src/*.c))


all: worker test Makefile linecount

worker: $(SRC:%.c=%.o)
	cc $(LFLAGS) -o $@ $^

run_tests: $(TESTS_SRC:%.c=%.o)
	cc $(LFLAGS) -o $@ $^

test: run_tests
	timeout 300s ./run_tests && timeout 30s ./lisp_tests/lisp_test.py

clean:
	rm -f *.o *.d worker    lisp/*.o lisp/*.d    tests_src/*.o tests_src/*.d run_tests

linecount:
	echo -n "linecount: " && cat *.c *.h */*.c */*.h */*.py */*.wlt | wc -l


release: clean
	gcc $(CFLAGS) $(LFLAGS) $(SRC) -UDEBUG -UOPTION_DEBUG -g0 -O3 -o worker

coverage: CFLAGS += -fprofile-arcs -ftest-coverage
coverage: LFLAGS += -fprofile-arcs -ftest-coverage
coverage: clean worker test Makefile linecount
	rm -rf coverage;
	mkdir coverage;
	gcov -fp *.gcno lisp/*.gcno tests_src/*.gcno > coverage/report;
	find . -iname "*.gcov" -exec mv -t coverage "{}" +;
	find . \( -iname "*.gcda" -or -iname "*.gcno" \) -delete;
	$(MAKE) --no-print-directory clean
	tail -n 1 coverage/report

profile: CFLAGS += -g -pg
profile: LFLAGS += -g -pg
profile: clean worker
	echo -e "\n\n\nRemember, profiling information is only generated on a clean exit (exit_program)"

.PHONY: all clean linecount release coverage profile

-include .dummy
.dummy: Makefile
	@touch $@
	@$(MAKE) -s clean

-include $(SRC:%.c=%.d) $(TESTS_SRC:%.c=%.d)
