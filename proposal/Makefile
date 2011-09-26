.PHONY: clean reallyclean

NAME=proposal

${NAME}.pdf: ${NAME}.tex citations.bib
	pdflatex ${NAME} && bibtex ${NAME} && \
	pdflatex ${NAME} && pdflatex ${NAME}

clean:
	-rm ${NAME}.aux ${NAME}.log ${NAME}.bbl ${NAME}.blg

reallyclean: clean
	-rm ${NAME}.pdf
