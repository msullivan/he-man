.PHONY: clean

test: mainloop.c mainloop.h variable_queue.h echo_handwritten.c
	gcc -g -laio -std=c99 -Wall -o test mainloop.c echo_handwritten.c

clean:
	-rm -f *.o *~ test
