# Build a CV with a spreadsheet and some R code!

## Files required

- spreadsheet of entries in your CV    
An example/template can be found [here](https://docs.google.com/spreadsheets/d/11S8-U4vasXgPbW-IKQPCy1nO-1UsmvzNLz7GvdONN_w/edit?usp=sharing)
- [bib file of publications](CV.bib)
- CV TeX file

CV templates from overleaf for `moderncv` and `moderntimeline` are provided in templates. `CV.tex` is a highly modified form of the `moderntimeline` template.

## Software required

- `latexmk` (comes with texlive/tinytex)
- `xelatex`
- `biber`
- `moderncv` and `moderntimeline` latex packages


## How it works

1. `code/build_functions.R` contains functions to formulate tables of CV information as latex code
2. `code/build_tex_files.R` uses the functions in `code/build_functions.R`, parses the google sheet with the CV info, and generates `_XXX.tex` files for each section of the CV which is programmatically generated.
3. `CV.tex` includes each of the `_XXX.tex` files built in the previous step
4. `CV.tex` generates publication lists by parsing `CV.bib` using `biber` (a more advanced biblatex). This usually requires the document to compile multiple times. 
5. `CV.pdf` is generated from `CV.tex`
