.PHONY: clean

docs.pdf: docs.tex ../src/CLib.lhs
	pdflatex $<

clean:
	-rm docs.aux docs.log
