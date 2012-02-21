.PHONY: all clean reallyclean

all: paper.pdf

%.pdf: %.tex citations.bib
	pdflatex $< && bibtex $* && pdflatex $< && pdflatex $< 

clean:
	-rm -f *.aux *.log *.bbl *.blg *.out

reallyclean: clean
	-rm -f paper.pdf
