.PHONY: clean

SRC=../src/Language/HeMan

docs.pdf: docs.tex ${SRC}/CLib.lhs
	pdflatex $<

clean:
	-rm docs.aux docs.log
