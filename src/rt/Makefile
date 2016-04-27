.PHONY: clean all

all: test test_mt

test: mainloop.c mainloop.h variable_queue.h echo_handwritten.c lib.h lib.c
	gcc -D_GNU_SOURCE=1 -g -std=c99 -Wall -o test mainloop.c lib.c echo_handwritten.c
#gcc -g -laio -std=c99 -Wall -o test mainloop.c echo_handwritten.c

test_mt: mainloop.c mainloop.h variable_queue.h echo_handwritten.c lib.h lib.c
	gcc -D_GNU_SOURCE=1 -g -pthread -DMT_RUNTIME=1 -std=c99 -Wall -o test_mt mainloop.c lib.c echo_handwritten.c

clean:
	-rm -f *.o *~ test
