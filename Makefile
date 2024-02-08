# How to use Make on Windows:
# https://stackoverflow.com/questions/2532234/how-to-run-a-makefile-in-windows
.PHONY: default

# This defines the final output file you intend to make use of
default: CV.pdf

# These are essential prerequisites that have to be updated
COMPONENTS = _edu.tex _talks.tex _grants.tex _software.tex _teaching.tex _mentoring.tex

# This command creates the components
$(COMPONENTS): build_functions.R build_tex_files.R
	Rscript "code/build_tex_files.R"
	echo 'Component files built'

# This command builds the PDF target (CV.pdf) from dependencies CV.tex, CV.bib,
# and the components above
%.pdf: %.tex %.bib $(COMPONENTS)
	latexmk -xelatex -g -pv -pdf $<


# This removes all dependencies/build files. Invoke with "make clean"
clean:
	rm -f *.pdf *.out *aux *bbl *blg *log *toc *bcf *run.xml *.ptb *.tod *.fls *.fdb_latexmk *.lof *out.ps *blg
