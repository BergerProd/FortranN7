PROJECT=tp3
TEX=pdflatex
BUILDTEX=$(TEX) $(PROJECT).tex

all:
	$(BUILDTEX)

clean:
	rm -f *.log *.aux *~

view:
	evince $(PROJECT).pdf
