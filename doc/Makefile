.PHONY: clean

SRC=../src/Language/HeMan

docs.pdf: docs.tex ${SRC}/CLib.lhs ${SRC}/Backend.lhs
	pdflatex $< && pdflatex $<

clean:
	-rm docs.aux docs.log docs.toc docs.out
