rapport.pdf:		rapport.tex
	pdflatex $<
	pdflatex $<

clean:
	rm -f *.aux *.log *.out *.nav *.snm *.toc *.vrb
