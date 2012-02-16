CFLAGS=-g -std=c99 -Wall -O2 -Wno-unused-variable
HEMAN_OBJ=Language/HeMan.o
RUNTIME_OBJ=rt/mainloop.o rt/lib.o
HEADERS=rt/mainloop.h rt/lib.h rt/variable_queue.h

all: rt/echod rt/httpd

.PHONY: all clean reallyclean

${HEMAN_OBJ}: Language/HeMan.hs
	ghc --make $<

rt/mainloop.o: rt/mainloop.c ${HEADERS}
rt/lib.o: rt/lib.c ${HEADERS}

# Echo server

rt/echod: rt/echo.o ${RUNTIME_OBJ}
	${CC} ${CFLAGS} $^ -o $@ -laio -lpthread

rt/echo.o: rt/echo.c ${HEADERS}
rt/echo.c: Echo.hs ${HEMAN_OBJ}
	ghc $< -e 'compile "$@" main_loop'

# HTTP server

rt/httpd: rt/http_lib.o rt/http.o ${RUNTIME_OBJ}
	${CC} ${CFLAGS} $^ -o $@ -laio -lpthread

rt/echo.o: rt/echo.c ${HEADERS}
rt/http.c: Http.hs ${HEMAN_OBJ}
	ghc $< -e 'compile "$@" main_loop'

clean:
	-rm -f Language/*.hi Language/*.o Language/HeMan/*.hi Language/HeMan/*.o \
	    rt/echo.c rt/http.c rt/*.o 

reallyclean: clean
	-rm -f rt/echod rt/httpd
