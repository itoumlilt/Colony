all: build

#internship_architecture.pdf : internship_architecture.svg
#	inkscape internship_architecture.svg --export-pdf=internship_architecture.pdf

build: specification.tex
	pdflatex specification
	bibtex specification
	pdflatex specification
	pdflatex specification


clean:
	rm -f *.aux *.log *.spl *.out *.bbl *.blg *.toc *~ \
	*.fdb_latexmk *.fls *.synctex.gz 
