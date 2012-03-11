.PHONY: all clean reallyclean

all: paper.pdf

paper.pdf: paper.tex citations.bib graph_100.pdf graph_16K.pdf graph_128K.pdf
	pdflatex $< && bibtex paper && pdflatex $< && pdflatex $< 

graph_100.ps: ../benchmarking/graph.py ../benchmarking/data/*
	../benchmarking/graph.py ../benchmarking/data/*
# Running the script produces all three graphs
graph_16K.ps: graph_100.ps
graph_128K.ps: graph_100.ps

%.pdf: %.ps
	ps2pdf $<

clean:
	-rm -f *.aux *.log *.bbl *.blg *.out

reallyclean: clean
	-rm -f paper.pdf
