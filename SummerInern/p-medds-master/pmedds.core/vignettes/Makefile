all: p-medds.pdf 

%.tex: %.Rnw
	R CMD Sweave $^
%.pdf: %.tex
	R CMD texi2dvi --pdf --clean $^

clean:
	rm -f *tex *aux *dvi *toc

install:
	cp *pdf ../inst/doc

