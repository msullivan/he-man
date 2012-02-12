CFLAGS=-g -laio -std=c99 -Wall -O2
HEMAN_OBJ=Language/HeMan.o
RUNTIME_OBJ=rt/mainloop.o rt/lib.o

.PHONY: all clean reallyclean

${HEMAN_OBJ}: Language/HeMan.hs
	ghc --make $<

all: rt/echo rt/http

# Echo server

rt/echo: ${RUNTIME_OBJ}

rt/echo.c: Echo.hs ${HEMAN_OBJ}
	ghc $< -e 'compile "$@" main_loop'

# HTTP server

rt/http: rt/http_lib.o ${RUNTIME_OBJ}

rt/http.c: Http.hs ${HEMAN_OBJ}
	ghc $< -e 'compile "$@" main_loop'

clean:
	-rm Language/*.hi Language/*.o Language/HeMan/*.hi Language/HeMan/*.o \
	    rt/echo.c rt/http.c rt/*.o 

reallyclean: clean
	-rm rt/echo rt/http
