.PHONY: clean reallyclean

slides.pdf: slides.tex graph.pdf
	pdflatex $<

graph.pdf: graph.dot
	dot -Tpdf $< > $@

clean:
	-rm *.aux *.log *.nav *.out *.snm *.toc *.vrb

reallyclean: clean
	-rm slides.pdf graph.pdf
