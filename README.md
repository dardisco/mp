# mp - Make pdfs

*Author:* C. Dardis <christopherdardis@gmail.com><br>
*Version:* 0.2<br>
*URL:* [http://github.com/dardisco/mp](http://github.com/dardisco/mp)<br>

Makes a pdf from the materials in the `default-directory`
(or will search up the directory tree for
 the appropriate file type).

These may include .R, .tex, .Rnw, .org, .Rmd and .bib files.
Support is provided for indexes, glossaries and table of contents.
 
Installation:

This package can be installed using

    (package-install-from-file "/path/to/mp.el")

Or place the folliwng in your init.el file

    (add-to-list 'load-path "~/path/to/directory")
    (require 'mp)

### Usage

To generate a .pdf file use the `mp-mp` command.
    <kbd>M-x mp-mp RET</kbd>
This command can be called within the directory used to produce
the .pdf. It can also be called from one of the files used to
produce the .pdf. In the latter case, the file will first be
saved if there are any changes.

There are a number of addition commands which you may wish
to call independently:
- `mp-customize-common` - Set common customizable variables
- `mp-example` - Generate an example .pdf
- `mp-custom-example` - Call both commands above, in sequence
- `mp-customize-all` - Set all customizable variables
- `mp-customize-skeleton-string` - Set all customizable
    skeleton strings for the package (strings used by skeletons)
- `mp-clean` - Delete files in which have `mp-clean-extensions`
- `mp-tex-new` - Make a new .tex file
- `mp-chunk-new` - Insert a new chunk

You may wish to define a shortcut for calling
 `mp-chunk-new`.
`f8` is suggested as there are 8 characters in these prefixes.
That is, "## ---- " is 8 characters in length.
The prefixes follow the conventions established by knitr.
If so, also place something like this in your init.el file:
```
(global-set-key
 (kbd "<f8>")
 (lambda ()
   (interactive)
   (mp-chunk-new)))
```   
 
or, for example, if using ESS and `bind-key`:
 
```
(bind-key
 (kbd "<f8>")
 (lambda ()
   (interactive)
   (mp-chunk-new))
 ess-mode-map)
```

### Details

The purpose of `mp-mp` is to move from an R file or an
intermediary file (.Rnw, .Rmd, .org, .bib, .tex) to a .pdf
with a single command. This is done by selecting an entwiner
(named after the conventions of 'weaving' with Sweave or 'knitting'
with knitr; the default is knitr).

The simplest way to see this in action is to call `mp-example`.
This will generate a new directory in the current
`default-directory` for the files used to generate the final .pdf.
This may also be a good way to start a new project.
 
Assuming we start with an .R file, `mp-mp` generates an
intermediary file, as appropriate, then uses the entwiner to
generate a .tex file from the intermediary, then uses latexmk
to generate a .pdf from the .tex file, which is then displayed.
 
Alternatively, we may start with an intermediary e.g. a .Rnw file.
A .R file is generated from this before making the .tex file,
as above.
We may also start directly with a .tex file. In this case,
no intermediary or .R file is created; we proceed directly
to .pdf. Similarly in the case of a .bib file. Note that
a .pdf will *not* be generated from a .bib file without a
corresponding .tex file in the same directory.

Output from the various steps in compiling the .pdf appears in
the '*make-pdf*' buffer, which (by default) will by used to
display the .pdf once complete. Alternative .pdf viewers are
supported. If another viewer is used, this output will remain
visible in the '*make-pdf*' buffer.

Note that all code in the .R file is run via Rscript, to ensure
there are no errors with this and in case this modifies another
file in the workign directory e.g. a .bib file as is the case
when running `mp-example`.

### Updating R code chunks

An advantage of `mp-mp` is that code can be updated in either
the .R file or the intermediary. Whichever has been more recently
modified will be used as the source and the other as the destination.
This allows for editing of code in either the .R file or the
intermediary.

That is, when `mp-mp` is called from an .R file or an intermediary,
a corresponding .R or intermediary will be created or
updated, as appropriate.

This starts by reading the chunks from the working file.
This is the buffer currently open (if it has unsaved changes).
This will be an .R file, or an entwiner file (.Rnw, .Rmd, .org).

For example, when working with an .R file, if there is *no*
entwiner file (.Rnw, .org) with the same file name prefix in
the same directory, this file will be *created* before generating
a .tex file from this entwiner.

If an entwiner file *already exists*, this is *deleted*.
A new entwiner file is created after reading the code chunks
in the .R file.

As above, note that changes to a .tex file do *not* update the
corresponding intermediary. Thus free text (outside .R code chunks)
should generally be updated in the intermediary file, unless
no further changes to the .R code is planned.

### Customization

To avoid errors, it is best to set the following customizable
variables once at the start of a project:
- `mp-entwiner`
- `mp-latex`
- `mp-bib`

If these are changed mid-way through, it may be necessary to
first delete the intermediary, .tex and .pdf files before starting
afresh from the .R file in the directory.

This package contains a variety of defaults which may be modified.
For example, the preamble for a LaTeX document, which varies
depending on whether LaTeX or XeTeX/LuaLaTeX is used to generate
the .pdf.

### Multiple plots per chunk

All of the entwiners will save graphical output in one or more
separate files, then include some these in the final .pdf.

Only knitr supports multiple plots per chunk by default.

By default, both Sweave and Org will include only the first
plot/graph/figure produced.
They both use the TeX command `\includegraphics` for this.
The function `mp-tex-to-pdf` will replace instances of this
command with `\includepdf[pages=-, width=0.9\linewidth]`,
so that all plots/pages are included.

An alternative is to save each plot as a separate
file and then output the TeX command to include all of them.

An example of a code block using this approach follows below.
```
for(i in 1:4){
file=paste("f", i, ".eps", sep="")
postscript(file=file, paper="special", width=8, height=8)
plot(rnorm(100)+i)
dev.off()
cat(" \\includegraphics{", file, "}\n", sep="")
}
```
In Sweave, we would wrap the code above in:
```
<<plots, results=tex, strip.white=false>>=
**code**
@
```
In Org, we would wrap the code above in:
```
#+NAME: plots2
#+begin_src R :session *org-R-session* :exports both :results output :results verbatim :results latex
**code**
#+end_src
```

### History/goals

This package was initially developed in 2012 to help to
speed up the process of scientific writing. At that time,
there was no easy was to tranistion quickly from a .R file to
a .pdf and knitr was relatively new and was not being used
routinely e.g. to generate package documentation.

The range of options has expanded dramatically since.
In particulary, .Rmd appears to have become the most popular
choice for performing this type of work.

### Dependencies

The following should be installed and on your path:
- R, including the packages knitr and rmarkdown
- LaTeX
- pandoc
- bash commands: locate, grep

The following emacs packages are also recommended:
- org
- ess
- tex, bibtex
- doc-view
- markdown-mode
- polymode, poly-R

This is designed for Emacs on debian linux with bash.
It has not been evaluated on other systems.

### For developers

Function-local/temporary variables are named using
as name1 e.g. `v1`, `buffer1`.



---
Converted from `mp.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
