.PHONY: clean

SRC=../src/Language/Foo

docs.pdf: docs.tex ${SRC}/CLib.lhs
	pdflatex $<

clean:
	-rm docs.aux docs.log
