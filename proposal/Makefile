.PHONY: clean reallyclean

proposal.pdf: proposal.tex
	pdflatex $<

clean:
	-rm proposal.aux proposal.log

reallyclean: clean
	-rm proposal.pdf
