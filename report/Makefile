.PHONY: all clean reallyclean

all: proposal.pdf report.pdf

%.pdf: %.tex citations.bib
	pdflatex $< && bibtex $* && pdflatex $< && pdflatex $< 

clean:
	-rm *.aux *.log *.bbl *.blg

reallyclean: clean
	-rm proposal.pdf report.pdf
