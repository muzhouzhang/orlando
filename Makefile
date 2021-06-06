# cd ~/Box/myBox/orlando/

.PHONY: fig tab
all: analysis fig tab main appendix clean
analysis: code/analysis.R
	Rscript -e 'source("code/analysis.R")'
fig: code/fig.R
	Rscript -e 'source("code/fig.R")'
tab: code/tab.R
	Rscript -e 'source("code/tab.R")'
main: tex/main.tex
	latexmk tex/main.tex -pdf
appendix: tex/appendix.tex
	latexmk tex/appendix.tex -pdf
clean:
	-rm -f *.aux
	-rm -f *.log
	-rm -f *.toc
	-rm -f *.bbl
	-rm -f *.blg
	-rm -f *.out
	-rm -f *.bcf
	-rm -f *.fdb_latexmk
	-rm -f *.fls
	-rm -f *.run.xml
	-rm -f *.nav
	-rm -f *.snm
	-rm -f .Rhistory
