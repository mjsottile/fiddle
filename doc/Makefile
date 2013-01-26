all:
	pdflatex fiddle.tex
	bibtex fiddle
	pdflatex fiddle.tex
	pdflatex fiddle.tex

clean:
	rm -f *~ *.aux *.log *.bbl *.blg
	rm -f *.spl
	rm -f fiddle.pdf
