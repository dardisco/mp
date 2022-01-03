;;; mp.el --- Make pdfs. -*- lexical-binding:t -*-

;; Copyright 2018-2022 - Chris Dardis

;; Author: C. Dardis <christopherdardis@gmail.com>

;; Version: 0.2

;; Keywords: R, Sweave, knitr, latex, noweb, org
;; URL: http://github.com/dardisco/mp
;; Package-Requires: ((ess) (org) (org-element) (tex))

;; This program is free software: you can redistribute
;;  it and/or modify it under the terms of the
;;  GNU General Public License as publishe
;;  the Free Software Foundation, either version 3
;;  of the License, or (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;; You should have received a copy of the GNU
;;  General Public License along with this program.
;; If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Makes a pdf from the materials in the `default-directory'
;; (or will search up the directory tree for
;;  the appropriate file type).
;;
;; These may include .R, .tex, .Rnw, .org, .Rmd and .bib files.
;; Support is provided for indexes, glossaries and table of contents.
;; 
;; Installation:
;;
;; This package can be installed using
;;
;; (package-install-from-file "/path/to/mp.el")
;;
;; Or place the folliwng in your init.el file
;;
;; (add-to-list 'load-path "~/path/to/directory")
;; (require 'mp)
;;
;;; Usage:
;;
;; To generate a .pdf file use the `mp-mp' command.
;;     `M-x mp-mp RET'
;; This command can be called within the directory used to produce
;; the .pdf. It can also be called from one of the files used to
;; produce the .pdf. In the latter case, the file will first be
;; saved if there are any changes.
;;
;; There are a number of addition commands which you may wish
;; to call independently:
;; - `mp-customize-common' - Set common customizable variables
;; - `mp-example' - Generate an example .pdf
;; - `mp-custom-example' - Call both commands above, in sequence
;; - `mp-customize-all' - Set all customizable variables
;; - `mp-customize-skeleton-string' - Set all customizable
;;     skeleton strings for the package (strings used by skeletons)
;; - `mp-clean' - Delete files in which have `mp-clean-extensions'
;; - `mp-tex-new' - Make a new .tex file
;; - `mp-chunk-new' - Insert a new chunk
;;
;; You may wish to define a shortcut for calling
;;  `mp-chunk-new'.
;; `f8' is suggested as there are 8 characters in these prefixes.
;; That is, "## ---- " is 8 characters in length.
;; The prefixes follow the conventions established by knitr.
;; If so, also place something like this in your init.el file:
;; ```
;; (global-set-key
;;  (kbd "<f8>")
;;  (lambda ()
;;    (interactive)
;;    (mp-chunk-new)))
;; ```   
;;  
;; or, for example, if using ESS and `bind-key':
;; 
;; ```
;; (bind-key
;;  (kbd "<f8>")
;;  (lambda ()
;;    (interactive)
;;    (mp-chunk-new))
;;  ess-mode-map)
;; ```
;;
;; Details:
;;
;; The purpose of `mp-mp' is to move from an R file or an
;; intermediary file (.Rnw, .Rmd, .org, .bib, .tex) to a .pdf
;; with a single command. This is done by selecting an entwiner
;; (named after the conventions of 'weaving' with Sweave or 'knitting'
;; with knitr; the default is knitr).
;;
;; The simplest way to see this in action is to call `mp-example'.
;; This will generate a new directory in the current
;; `default-directory' for the files used to generate the final .pdf.
;; This may also be a good way to start a new project.
;; 
;; Assuming we start with an .R file, `mp-mp' generates an
;; intermediary file, as appropriate, then uses the entwiner to
;; generate a .tex file from the intermediary, then uses latexmk
;; to generate a .pdf from the .tex file, which is then displayed.
;; 
;; Alternatively, we may start with an intermediary e.g. a .Rnw file.
;; A .R file is generated from this before making the .tex file,
;; as above.
;; We may also start directly with a .tex file. In this case,
;; no intermediary or .R file is created; we proceed directly
;; to .pdf. Similarly in the case of a .bib file. Note that
;; a .pdf will *not* be generated from a .bib file without a
;; corresponding .tex file in the same directory.
;;
;; Output from the various steps in compiling the .pdf appears in
;; the '*make-pdf*' buffer, which (by default) will by used to
;; display the .pdf once complete. Alternative .pdf viewers are
;; supported. If another viewer is used, this output will remain
;; visible in the '*make-pdf*' buffer.
;;
;; Note that all code in the .R file is run via Rscript, to ensure
;; there are no errors with this and in case this modifies another
;; file in the workign directory e.g. a .bib file as is the case
;; when running `mp-example'.
;;
;; Updating R code chunks:
;;
;; An advantage of `mp-mp' is that code can be updated in either
;; the .R file or the intermediary. Whichever has been more recently
;; modified will be used as the source and the other as the destination.
;; This allows for editing of code in either the .R file or the
;; intermediary.
;;
;; That is, when `mp-mp' is called from an .R file or an intermediary,
;; a corresponding .R or intermediary will be created or
;; updated, as appropriate.
;;
;; This starts by reading the chunks from the working file.
;; This is the buffer currently open (if it has unsaved changes).
;; This will be an .R file, or an entwiner file (.Rnw, .Rmd, .org).
;;
;; For example, when working with an .R file, if there is *no*
;; entwiner file (.Rnw, .org) with the same file name prefix in
;; the same directory, this file will be *created* before generating
;; a .tex file from this entwiner.
;;
;; If an entwiner file *already exists*, this is *deleted*.
;; A new entwiner file is created after reading the code chunks
;; in the .R file.
;;
;; As above, note that changes to a .tex file do *not* update the
;; corresponding intermediary. Thus free text (outside .R code chunks)
;; should generally be updated in the intermediary file, unless
;; no further changes to the .R code is planned.
;;
;; Customization:
;;
;; To avoid errors, it is best to set the following customizable
;; variables once at the start of a project:
;; - `mp-entwiner'
;; - `mp-latex'
;; - `mp-bib'
;;
;; If these are changed mid-way through, it may be necessary to
;; first delete the intermediary, .tex and .pdf files before starting
;; afresh from the .R file in the directory.
;;
;; This package contains a variety of defaults which may be modified.
;; For example, the preamble for a LaTeX document, which varies
;; depending on whether LaTeX or XeTeX/LuaLaTeX is used to generate
;; the .pdf.
;;
;; Multiple plots per chunk:
;;
;; All of the entwiners will save graphical output in one or more
;; separate files, then include some these in the final .pdf.
;;
;; Only knitr supports multiple plots per chunk by default.
;;
;; By default, both Sweave and Org will include only the first
;; plot/graph/figure produced.
;; They both use the TeX command `\includegraphics' for this.
;; The function `mp-tex-to-pdf' will replace instances of this
;; command with `\includepdf[pages=-, width=0.9\linewidth]',
;; so that all plots/pages are included.
;;
;; An alternative is to save each plot as a separate
;; file and then output the TeX command to include all of them.
;;
;; An example of a code block using this approach follows below.
;; ```
;; for(i in 1:4){
;; file=paste("f", i, ".eps", sep="")
;; postscript(file=file, paper="special", width=8, height=8)
;; plot(rnorm(100)+i)
;; dev.off()
;; cat(" \\includegraphics{", file, "}\n", sep="")
;; }
;; ```
;; In Sweave, we would wrap the code above in:
;; ```
;; <<plots, results=tex, strip.white=false>>=
;; **code**
;; @
;; ```
;; In Org, we would wrap the code above in:
;; ```
;; #+NAME: plots2
;; #+begin_src R :session *org-R-session* :exports both :results output :results verbatim :results latex
;; **code**
;; #+end_src
;; ```
;;
;; History/goals:
;;
;; This package was initially developed in 2012 to help to
;; speed up the process of scientific writing. At that time,
;; there was no easy was to tranistion quickly from a .R file to
;; a .pdf and knitr was relatively new and was not being used
;; routinely e.g. to generate package documentation.
;;
;; The range of options has expanded dramatically since.
;; In particulary, .Rmd appears to have become the most popular
;; choice for performing this type of work.
;;
;; Dependencies:
;;
;; The following should be installed and on your path:
;; - R, including the packages knitr and rmarkdown
;; - LaTeX
;; - pandoc
;; - bash commands: locate, grep
;;
;; The following emacs packages are also recommended:
;; - org
;; - ess
;; - tex, bibtex
;; - doc-view
;; - markdown-mode
;; - polymode, poly-R
;;
;; This is designed for Emacs on debian linux with bash.
;; It has not been evaluated on other systems.
;;
;;; For developers:
;; 
;; Function-local/temporary variables are named using
;; as name1 e.g. `v1', `buffer1'.
;;

;;; Code:

(mapc #'require
      '(org ess tex bibtex
	    doc-view ibuffer
	    autorevert cl calendar timer))
;; in case org not set up to fontify R code blocks
(setq org-src-fontify-natively nil)

(defgroup mp nil
  "Make Pdf.
This is a series of variables and functions to simplity
 the process of pdf creation using R, LaTeX and
 the intermediaries (entwiners)
 knitr, Sweave and Org (`org-mode')."
  :prefix "mp-"
  :version 0.2
  :group 'mp)

(defgroup mp-common nil
  "Options that the user is most likely to wish to change.
This group is part of `mp'."
  :group 'mp)

(defun mp-custom-example ()
  "Run `mp-customize-common' then `mp-example'."
  (interactive)
  (mp-customize-common)
  (mp-example))

(defun mp-example ()
  "Generate and run an example.
Calls `mp-mp', in the subdirectory `mp-example-file-name'."
  (interactive)
  (mp-set-mp-dd)
  (let ((d1 (concat mp-dd mp-example-file-name "/")))
    (delete-directory d1 t)
    (mkdir d1 t)
    (cd d1)
    (setq mp-dd d1
	  mp-fnb mp-example-file-name
	  mp-fne "R")
    (mp-initialize)
    (message
     (concat "mp-example ... running in " (print mp-dd))))
  (mp-run mp-fne))

(defun mp-initialize ()
  "Set the following `defvar's to their staring value:
- `mp-df'
- `mp-fn'
- `mp-chunks'
Called by `mp-example' and `mp-mp'."
  (while (get-buffer-window "*make-pdf*")
    (delete-window
     (get-buffer-window "*make-pdf*")))
  (when (get-buffer "*make-pdf*")
    (kill-buffer "*make-pdf*"))
  (balance-windows)
  (setq mp-df (sort
	       ;; (directory-files DIRECTORY &optional FULL MATCH NOSORT)
	       (directory-files mp-dd nil
				(mapconcat
				 (lambda (x) (concat "\\." x "$"))
				 mp-file-name-extensions
				 "\\|") t)
	       #'file-newer-than-file-p)
	mp-fn (car mp-df)
	mp-chunks '()
	mp-exiting nil))

(defvar mp-dd ""
  "The `default-directory'; set when `mp-set-mp-dd' is run.
Defined here as a dynamicaly-bound variable so that
various functions can refer to this value.")

(defvar mp-fnb ""
  "The `file-name-base' (a string) for the working file in `mp-dd'.
Set when `mp-mp' is run.
Defined here as a dynamic variable so that
various functions can use this value.")

(defvar mp-fne ""
  "The `file-name-extension' (a string) for the working file in `mp-dd'.
This is set when `mp-mp' is run.
Defined here as a dynamic variable so that
various functions can use this value.")

(defvar mp-chunks '(("" "" ""))
  "Stores value for code chunks as a list of strings.
The value should be a `list' of 3 elements where
the first is the name of the chunk,
the second provides formattinhg information
the third is the content of the chunk.

The second element, chunk formatting should
reflect the entwiner in use; see also
`mp-sweave-opts' and `mp-org-opts'.")

(defun mp-set-mp-dd ()
  "Set the value of `mp-dd'.
If `mp-fnb' is an empty string, also set this to the same
value as `mp-dd'."
  (setq mp-dd
	(cond ((buffer-file-name)
	       (file-name-directory buffer-file-name))
	      ((equal major-mode 'dired-mode)
	       dired-directory)
	      (t (shell-command-to-string "printf '%s' $HOME/")))
	default-directory mp-dd)
  (message
   (concat "mp-set-mp-dd ... mp-dd set to " (print mp-dd)))
  (when (equalp "" mp-fnb)
    (setq mp-fnb
	  (file-name-nondirectory
	   (directory-file-name mp-dd)))
  (message
   (concat "mp-set-mp-dd ... mp-fnb set to " (print mp-fnb)))))

(defcustom mp-example-file-name "mp-example"
  "Name of directory and of `file-name-base' to use for an example.
Used by `mp-example'.
Avoid using special characters in this string.
See also `mp-valid-file-name-p'."
  :type '(string)
  :safe 'mp-valid-file-name-p
  :group 'mp)
;; (put 'mp-example-file-name 'safe-local-variable 'mp-valid-file-name-p)

(defun mp-valid-file-name-p (STRING)
  "Return t is STRING is a valid filename.
Uses `file-name-invalid-regexp', which depends on the `system-type'.
An example of an invalid filename is \"\\0\".
See also the following for additional restrictions on Windows:
URL `https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file'."
  (interactive "FFilename: ")
  (not
   (string-match file-name-invalid-regexp STRING)))
;; (mp-valid-file-name-p "\0")

(defvar mp-df '()
  "A list of the `directory-files' in `mp-dd'.
Defined here as a dynamicaly-bound variable so that
 various functions can refer to this value.")

(defcustom mp-timeout 0.2
  "Seconds to wait for user input when calling interactive functions.
A low value will not give the user time to respond to prompts.
In this case, the default values are used.
See the functions `with-timeout' and `y-or-n-p-with-timeout'.
Used by:
- `mp-find-file-name-interactive'"
  :type '(number)
  :group 'mp
  :group 'mp-common)

(defcustom mp-file-name-extensions
  '("R" "Rnw" "org" "Rmd" "md" "tex" "bib" "pdf")
  "The `file-name-extension's (as strings) with which `mp' works.
These age given as a list of strings.
They are not case sensitive (see `case-fold-search')."
  :type '(repeat (string :tag "extension"))
  :group 'mp)

(defvar mp-fn ""
  "The file-name i.e. `mp-fnb'.`mp-fne'.
Defined here as a dynamicaly-bound variable so that
 various functions can refer to this value.")

(defcustom mp-function-sequence
  '(("R" . 
     (cond
      ((equalp "Rnw" (car mp-entwiner))
       '(mp-R-entwiner mp-Rnw-to-tex mp-tex-to-pdf mp-view-pdf))
      ((equalp "org" (car mp-entwiner))
       '(mp-R-entwiner mp-org-to-tex mp-tex-to-pdf mp-view-pdf))
      ((equalp "Rmd" (car mp-entwiner))
       '(mp-R-entwiner mp-Rmd-to-pdf mp-view-pdf))))
    ("Rnw" .
     '(mp-R-entwiner mp-Rnw-to-tex mp-tex-to-pdf mp-view-pdf))
    ("org" .
     '(mp-R-entwiner mp-org-to-tex mp-tex-to-pdf mp-view-pdf))
    ("Rmd" . 
     '(mp-R-entwiner mp-Rmd-to-pdf mp-view-pdf))
    ("tex\\|bib" .
     '(mp-tex-to-pdf mp-view-pdf))
    ("pdf" .
     '(mp-view-pdf)))
  "An alist where:
The key is the file name extension, a regular expression.
The value is list of functions to run.
Used by `mp-run' as the overall control flow."
  :type '(alist
	  :key-type (string :format "\n %t \n %v"
			    :tag "            file extension, a string")
	  :value-type (sexp :format "%t \n %v"
			    :tag " sexp: a list, or function returning a list"))
  :group 'mp)

(defun mp-run ()
  "Run the appropriate list of functions from `mp-function-sequence'.
This is given by `mp-fne'."
  (let ((seq1 (block nil
		(dolist (x mp-function-sequence)
		  (when (string-match (car x) mp-fne)
		    (return (eval (cdr x))))))))
    (message
     (concat
      "mp-run, with " (car mp-entwiner) " and " (cdr mp-entwiner)
      ", starting with ." mp-fne " file ..."))
    (dolist (x seq1)
      (funcall x)))
  (mp-exit "mp-run... done"))

(defun mp-R-entwiner ()
  "Convert R to an entwiner or vice versa.
Calls `mp-chunks-read' to get code chunks from the current file.
If this is an .R file and an 'entwiner' file already exists,
 this is deleted and replaced.
 update the *least recently used* file
 i.e. the .R or the entwiner, using, as appropriate 
 `mp-update-entwiner-from-R' or `mp-update-R-from-entwiner'.
If no such file exists, make one with `mp-entwiner-new'."
  ;; rf1 = .R file
  ;; ef1 = entwiner file
  (let ((rf1 (car (cl-member (concat mp-fnb ".R")
			     mp-df :test #'equalp)))
	(ef1 (car (cl-member (concat mp-fnb "." (car mp-entwiner))
			     mp-df :test #'equalp))))
    (message "mp-R-entwiner ...")
    (if (not rf1)
	(progn
	  (mp-R-new)
	  (unless (mp-empty-string-to-nil ef1) 
	    (mp-entwiner-new)))
      (if ef1
	  (if (file-newer-than-file-p rf1 ef1)
	      (mp-update-entwiner-from-R)	 
	    (mp-update-R-from-entwiner))
	(progn
	  (mp-file-display (concat mp-fnb ".R"))
	  (mp-chunks-read)
	  (mp-entwiner-new)))))
  (when mp-bib
    (mp-bib))
    ;; run mp-fnb.R in case this affects mp.bib
    (mp-shell-sentinel "mp-Rscript-run")
    (message "mp-R-entwiner ... done"))

(defun mp-bib ()
  "Check if there is a file named `mp-fnb'.bib in `mp-dd'.
If not, make one by calling `mp-bib-skeleton'.
See also the variable `mp-bib'."
  (unless (directory-files
	   mp-dd nil (concat mp-fnb ".bib$"))
    (mp-file-display (concat mp-fnb ".bib"))
    (mp-bib-skeleton)
    (save-buffer)
    (kill-buffer)
    (delete-window)))

(defcustom mp-entwiner '("Rnw" . "R-pkg-knitr")
  "How to 'entwine' the files in `mp-dd'.
Given in the form of a cons, where:
- key-type   = `file-name-extension' (a string)
- value-type = method (a string)

The 'entwiner' is an intermediary/ bridging file, 
 which acts as a 'go-between' for .R and .tex files.

\"R-pkg-knitr\" and \"Sweave\" use an .Rnw file (see `Rnw-mode').
\"Org\" uses an .org file (see `org-mode')
See also info node `(org)LaTeX export'.
\"R-pkg-rmarkdown\" uses an .Rmd file (see `poly-markdown+r-mode')
 and an .md file (see `markdown-mode')."
  :type '(radio (const :doc "Rnw with knitr (the default)"
		       :value ("Rnw" . "R-pkg-knitr"))
		(const :doc "Rnw with Sweave"
		       :value ("Rnw" . "Sweave"))
		(const :doc "Org (uses .org file)"
		       :value ("org" . "Org"))
		(const :doc " md with knitr"
		       :value ("Rmd" . "R-pkg-rmarkdown")))
  :group 'mp
  :group 'mp-common)

(defun mp-R-new ()
  "Make an .R file.
If `mp-chunks' is not an empty list, use these,
otherwise use `mp-R-chunks-new-R'.
Called by `mp-R-entwiner' and `mp-update-R-from-entwiner'."
  (when (equalp '() mp-chunks)
    (when mp-bib
      (push
       (list "R citations" "" (funcall #'mp-R-chunk-R-citations))
       mp-chunks))
    (dolist (x (reverse (copy-tree mp-R-chunks-new-R)))
      (push x mp-chunks))
    (when (string-match "^R-pkg-" (cdr mp-entwiner))
      (push (copy-tree mp-R-chunk-defaults-R-pkg) mp-chunks)))
  (dolist (x mp-chunks)
    (mp-chunk-update-format x))
  (setq mp-fne "R")
  (let ((fn1 (concat mp-fnb ".R")))
    (unless (get-buffer fn1)
      (mp-file-display fn1))
    (with-current-buffer fn1
      (goto-char (point-min))
      (mapc #'mp-chunk-insert mp-chunks)
      (goto-char (point-max)))
    (save-buffer)
    (kill-buffer)
    (delete-window)))

(defun mp-chunk-update-format (CHUNK)
  "Update the format of a CHUNK, per the value of `mp-entwiner'.
The format is the second element of CHUNK, a string."
  (when (equalp "" (cadr CHUNK))
    (setcar (cdr CHUNK) (mp-chunk-format))))

(defcustom mp-R-chunks-new-R
  '(("Hello world" ""
     "print('Hello, World!')")
    ("Plots"
     (cond ((string-match "R-pkg-" (cdr mp-entwiner))
	    "results='asis'")
	   ((equalp "Sweave" (cdr mp-entwiner))
	    "results=verbatim, fig=TRUE, pdf=TRUE")
	   ((equalp "Org" (cdr mp-entwiner))
	    ":session *org-R-session* :exports both :results output :results graphics :file \"p1.pdf\""))
      "## The examples below are taken from ?graphics::plot
require('stats') # for lowess, rpois, rnorm
plot(cars)
lines(lowess(cars))
plot(sin, -pi, 2*pi) # see ?plot.function
## Discrete Distribution Plot:
plot(table(rpois(100, 5)), type='h', col='red', lwd=10,
     main='rpois(100, lambda=5)')
## Simple quantiles/ECDF, see ecdf() {library(stats)} for a better one:
plot(x <- sort(rnorm(47)), type='s', main='plot(x, type=\\\'s\\\')')
points(x, cex=0.5, col='dark red')")
    ("R session information"
     (cond ((string-match "R-pkg-" (cdr mp-entwiner))
	    "results='asis'")
	   ((equalp "Sweave" (cdr mp-entwiner))
	    "results=tex")
	   ((equalp "Org" (cdr mp-entwiner))
	    ":session *org-R-session* :exports both :results output :results verbatim :results latex"))
     (if (equalp "Rmd" (car mp-entwiner))
	 "utils::sessionInfo()"
     "utils::toLatex(utils::sessionInfo())")))
  "A `list' of chunks for a new .R file.
Each element is a list with 3 elements.
- first element  = chunk name, a string
- second element = chunk format, a string or
   a symbolic expression which evaluates to a string
- third element  = chunk content, a string or
   a symbolic expression which evaluates to a string
The second and third elements may e.g. be conditional,
e.g. depending on the value of `mp-entwiner'.
Be sure to escape:
- double quotes i.e. \"\"
- single quotes within single quotes e.g. 'abc \'def\' ghi'
When editing this with `customize', use
`electric-newline-and-maybe-indent' for carraige return."
  :type '(alist
	  :key-type (string :format "\n %t \n %v"
			    :tag "           Chunk name")
	  :value-type
	  (group
	   (sexp :format "%t \n %v" :tag "Chunk format")
	   (sexp :format "%t \n %v" :tag "Chunk content")))
  :group 'mp
  :group 'mp-skeleton-string)
 
(defun mp-R-chunk-R-citations ()
  "Return a string which is R code that writes a bibliography file.
This file is named '`mp-fnb'.bib.'.
A citation appears therein for each loaded R package.
Adds a handle to each citation, if necessary.
This is added to `mp-chunks' when `mp-bib' is non-nil.
Called by `mp-R-new'."
  (concat
   "print(.packages())
## vector for citations
vc1 <- unlist(sapply(X=rev(.packages()),
                     FUN=function(x)
                         utils::toBibtex(utils::citation(x))))
vc1 <- vc1[!vc1=='']
## names for citations
n1 <- vc1[which(grepl(pattern='.year', x=vc1))]
n1  <- paste0(
    sub(pattern='\\\\.year$', replacement='', x=names(n1)),
    regmatches(x=n1, m=regexpr(pattern='[0123456789]+', text=n1)))
## group as list
l1 <- vector(mode='list', length=length(n1))
names(l1) <- n1
end1 <- which(grepl(pattern='\\\\}$', x=vc1))
## remove trailing comma from last item in .bib entry, if necessary
vc1[end1-1] <- sub(pattern=',$', replacement='', x=vc1[end1-1])
end1 <- c(0, end1)
for (i in seq.int(length.out=length(l1)))
    l1[[i]] <- unname(vc1[seq.int(from=(end1[i] + 1L), to=end1[i+1])])
l1 <- l1[!duplicated(l1)]
## add a handle, if necessary
for (i in seq.int(length.out=length(l1)))
    if (grepl(pattern='\\\\\{,$', x=l1[[i]][1]))
        l1[[i]][1] <- sub(pattern=',$', 
                          replacement=paste0(names(l1[i]), ','),
                          x=l1[[i]][1])
if (!'mp-example.bib' %in% dir())
    write(x='', file='" mp-dd mp-fnb ".bib')
bib1 <- readLines('" mp-dd mp-fnb ".bib')
for (i in seq.int(length.out=length(l1)))
    if (!(l1[[i]][1] %in% bib1))
        write(x=l1[[i]], file='" mp-dd mp-fnb ".bib', append=TRUE)"))

(defun mp-file-display (FILE-NAME)
  "Displays FILE-NAME.
If the file is already open if a buffer, calls `pop-to-buffer'.
If not, calls `mp-file-open'.
Finally, calls `mp-set-mode' and `mp-chunk-set-adfixes'."
  (setq mp-fne (file-name-extension FILE-NAME))
  (let ((default-directory mp-dd)
	(bn1 (car
	      (member FILE-NAME
		      (mapcar 'buffer-name (buffer-list))))))
    (if bn1
	(pop-to-buffer bn1)
      (mp-file-open FILE-NAME)))
  (mp-set-mode)
  (mp-chunk-set-adfixes))

(defun mp-file-open (FILE-NAME)
  "Open FILE-NAME in a buffer; set buffer to appropriate mode.
Return buffer.
This function is derived from a combination of the functions
 `find-file-noselect' and `find-file-noselect-1'.
Called by `mp-file-display'."
  (let* (;; as file-truename below uses default-directory
	 (default-directory mp-dd)
	 (truename1 (abbreviate-file-name
		     (file-truename FILE-NAME)))
	 (buf1 nil))
    (if (featurep 'uniquify)
	;; buffer names created using uniquify can be ugly
	(let ((uniquify-managed
	       (progn
		 (make-local-variable 'uniquify-managed)
		 nil)))
	  (setq buf1 (create-file-buffer FILE-NAME)))
      (setq buf1 (create-file-buffer FILE-NAME)))
    (pop-to-buffer buf1 nil t)
    (set-buffer buf1)
    (set-buffer-multibyte t)
    (when (and (= (buffer-size buf1) 0)
	       (directory-files
		mp-dd nil
		(concat "^" FILE-NAME "$")))
      (insert-file-contents-literally truename1 t))
    (setq buffer-file-truename truename1
	  buffer-file-name (expand-file-name buffer-file-truename)
	  buffer-file-number
	  ;; inode number
	  (nthcdr
	   10
	   (file-attributes truename1)))))

(defun mp-chunk-set-adfixes ()
  "Set the prefixes and suffixes for chunks.
These depend on the `file-name-extension' of the curent buffer,
if applicable,  otherwise on `mp-fne'.
The following file-local variables are set:
 - `mp-chunk-heading-level-prefix'
 - `mp-chunk-heading-level-suffix'
 - `mp-chunk-name-prefix'
 - `mp-chunk-name-suffix'
 - `mp-chunk-format-prefix'
 - `mp-chunk-content-suffix'
Called by `mp-file-display'."
  ;; fe1 = file extension
  (let ((fe1 (car (cl-member (file-name-extension (buffer-file-name))
		     '("R" "Rnw" "org" "Rmd") :test #'equalp))))
    (unless fe1
      (setq fe1 mp-fne))
    (cond
     ((equalp "R" fe1)
      (setq mp-chunk-name-prefix mp-chunk-name-prefix-R
	    mp-chunk-format-prefix mp-chunk-format-prefix-R
	    mp-chunk-name-suffix ""
	    mp-chunk-content-suffix mp-chunk-content-suffix-R))
     ((equalp "Rnw" fe1)
      (setq mp-chunk-heading-level-prefix (elt mp-chunk-heading-level 0)
	    mp-chunk-heading-level-suffix "}"
	    mp-chunk-name-prefix mp-chunk-name-prefix-Rnw
	    mp-chunk-name-suffix mp-chunk-name-suffix-Rnw
	    mp-chunk-format-prefix mp-chunk-format-prefix-Rnw
	    mp-chunk-content-suffix mp-chunk-content-suffix-Rnw))
     ((equalp "org" fe1)
      (setq mp-chunk-heading-level-prefix (elt mp-chunk-heading-level 1)
	    mp-chunk-heading-level-suffix ""
	    mp-chunk-name-prefix mp-chunk-name-prefix-org
	    mp-chunk-format-prefix mp-chunk-format-prefix-org
	    mp-chunk-name-suffix ""
	    mp-chunk-content-suffix mp-chunk-content-suffix-org))
     ((equalp "Rmd" fe1)
      (setq mp-chunk-heading-level-prefix (elt mp-chunk-heading-level 2)
	    mp-chunk-heading-level-suffix ""
	    mp-chunk-name-prefix mp-chunk-name-prefix-Rmd
	    mp-chunk-name-suffix mp-chunk-name-suffix-Rmd
	    mp-chunk-format-prefix mp-chunk-format-prefix-Rmd
	    mp-chunk-content-suffix mp-chunk-content-suffix-Rmd)))
    (message (concat "mp-chunk-set-adfixes... set per for ."
		     fe1 " file extension"))))

(defcustom mp-chunk-name-prefix-R "## ---- "
  "Prefix for the chunk name in an .R file.
The default value follows the convention used by 'knitr'."
  :type '(string)
  :group 'mp)

(defcustom mp-chunk-format-prefix-R ", "
  "Prefix for the chunk format in an .R file.
The default value follows the convention used by 'knitr'."
  :type '(string)
  :group 'mp)

(defcustom mp-chunk-content-suffix-R "\n"
    "Suffix for the chunk content in an .R file."
  :type '(sexp)
  :group 'mp)

(defcustom mp-chunk-heading-level '("\\section{" "*** " "### ")
  "Prefixes indicating the heading level for a chunk.
Specify these in the form of a list of two in the form:
 '(heading-level-for-LaTeX heading-for-Org)

The value is used to prior to a chunk-name.
As a TeX command, a closing \"}\" is added e.g. 
'\\subsection{'chunk-name'}'.

Regarding levels of depth:
- LaTeX uses 7 heading levels by default
- `org-mode' supports up to 8 heading levels
  (only 7, * to *******, are provided as options here)
- markdown uses 6 heading levels, # to ######
Thus, we here restrict outselves to 7, bearing in mind that other 
formats can be more restrictive e.g. markdown supports 6.
This is not meant to indicate a direct correspondance between 
heading levels in org-mode and LaTeX.

These values are used by `mp-chunk-insert'.

For org-mode, see also `org-level-faces', `org-heading-components' and
`org-element-headline-parser'.
For markdown-mode, see also `markdown-regex-header'."
  :type '(choice
	  :tag ""
	  (list :tag "level 1 = part"
		(string :tag "LaTeX" :value "\\part{")
		(string :tag "org" :value "* ")
		(string :tag "Rnw" :value "# "))
	  (list :tag "level 2 = chapter
  [LaTeX - only for \\documentclass{book} or {report}]"
		(string :tag "LaTeX" :value "\\chapter{")
		(string :tag "org" :value "** ")
		(string :tag "Rnw" :value "## "))
	  (list :tag "level 3 = section"
		(string :tag "LaTeX" :value "\\section{")
		(string :tag "org" :value "*** ")
		(string :tag "Rnw" :value "# "))
	  (list :tag "level 4 = subsection"
		(string :tag "LaTeX" "\\subsection{")
		(string :tag "org" :value "**** ")
		(string :tag "Rnw" :value "## "))
	  (list :tag "level 5 = subsubsection"
		(string :tag "LaTeX" :value "\\subsubsection{")
		(string :tag "org" :value "***** ")
		(string :tag "Rnw" :value "### "))
	  (list :tag "level 6 = paragraph"
		(string :tag "LaTeX" :value "\\paragraph{")
		(string :tag "org" :value "****** ")
		(string :tag "Rnw" :value "###### "))
	  (list :tag "level 7 = sub-paragraph
  [markdown - not applicable]"
		(string :tag "LaTeX" :value "\\subparagraph{")
		(string :tag "org" :value "******* ")
		(string :tag "Rnw" :value ""))
	  (list :tag "none"
		(const :tag "LaTeX" :value "")
		(const :tag "org" :value "")
		(const :tag "Rnw" :value ""))
	  (list :tag "custom"
		(string :tag "LaTeX" :value "prefix-for-LaTeX")
		(string :tag "org " :value "prefix-for-org")
		(string :tag "Rnw" :value "prefix-for-Rnw")))
  :group 'mp
  :group 'mp-common)

(defvar-local mp-chunk-heading-level-prefix ""
  "Value of chunk heading level prefix.
Set with `mp-chunk-set-adfixes'.")

(defvar-local mp-chunk-heading-level-suffix ""
  "Value of chunk heading level suffix.
Set with `mp-chunk-set-adfixes'.")

(defvar-local mp-chunk-name-prefix ""
  "Value of chunk name prefix.
Set with `mp-chunk-set-adfixes'.")

(defvar-local mp-chunk-name-suffix ""
  "Value of chunk name suffix.
Set with `mp-chunk-set-adfixes'.")

(defvar-local mp-chunk-format-prefix ""
  "Value of chunk format prefix.
Set with `mp-chunk-set-adfixes'.")

(defvar-local mp-chunk-content-suffix ""
  "Value of chunk content suffix.
Set with `mp-chunk-set-adfixes'.")

(defconst mp-chunk-name-prefix-Rnw "<<"
  "Prefix for the chunk name in an .Rnw file.")

(defconst mp-chunk-name-prefix-org "#+NAME: "
  "Prefix for the chunk name in an .org file.
See also `mp-org-opts'.")

(defconst mp-chunk-name-prefix-Rmd "```{r "
  "Prefix for the chunk name in an .Rmd file.")

(defconst mp-chunk-format-prefix-Rnw ", "
  "Prefix for the chunk format in an .Rnw file.")

(defconst mp-chunk-format-prefix-org "\n#+begin_src R "
  "Prefix for the chunk format in an .org file.")

(defconst mp-chunk-format-prefix-Rmd ", "
  "Prefix for the chunk format in an .Rmd file.
Note that this is not essential i.e. a code chunk could begin
```{r chunk-name, echo=TRUE
rather than
```{r, chunk-name, echo=TRUE
The convention of an extra comma follows that used by knitr.")

(defconst mp-chunk-name-suffix-Rnw ">>="
  "Suffix for the chunk name in an .Rnw file.")

(defconst mp-chunk-name-suffix-Rmd "}"
  "Suffix for the chunk name in an .Rmd file.")

(defconst mp-chunk-content-suffix-Rnw "@"
  "Suffix for the chunk content in an .Rnw file.")

(defconst mp-chunk-content-suffix-Rmd "```"
  "Suffix for the chunk content in an .Rmd file.")

(defconst mp-chunk-content-suffix-org "#+end_src"
  "Suffix for the chunk content in an .org file.")

(defun mp-set-mode ()
  "Set the appropriate mode for a file."
  (cond ((equalp "R" mp-fne)
	 (R-mode))
	((equalp "Rnw" mp-fne)
	 (Rnw-mode))
	((equalp "org" mp-fne)
	 (org-mode))
	((equalp "Rmd" mp-fne)
	 (markdown-mode)
	 (unless poly-markdown+r-mode
	   (poly-markdown+r-mode)))
	((equalp "tex" mp-fne)
	 (tex-mode))
	((equalp "bib" mp-fne)
	 (bibtex-mode))
	(t (fundamental-mode))))

(defcustom mp-R-chunk-defaults-R-pkg
  '("Default options for knitr and rmarkdown"
  ""
  "### Defaults for chunks typeset with knitr
library('knitr')
### defaults for all chunks
opts\_chunk$set(
    eval=TRUE,
    ## text results
    echo=TRUE,
    results=c('markup', 'asis', 'hold', 'hide')[1],
    collapse=FALSE,
    warning=TRUE, message=TRUE, error=TRUE,
    split=FALSE, include=TRUE, strip.white=TRUE,
    ## code decoration
    tidy=FALSE, prompt=FALSE, comment='##',
    highlight=TRUE, size='normalsize',
    background=c('#F7F7F7', colors()[479], c(0.1, 0.2, 0.3))[1],
    ## cache
    cache=FALSE,
    ## plots
    fig.path=c('figure', 'figure/minimal-')[1],
    fig.keep=c('high', 'none', 'all', 'first', 'last')[1],
    fig.align=c('center', 'left', 'right', 'default')[1],
    fig.show=c('hold', 'asis', 'animate', 'hide')[1],
    dev=c('pdf', 'png', 'tikz')[1],
    fig.width=7, fig.height=7, #inches
    fig.env=c('figure', 'marginfigure')[1],
    fig.pos=c('', 'h', 't', 'b', 'p', 'H')[1])
opts_knit$set(out.format='latex')
knit_theme$set('biogoo')
### Set R options
options(formatR.arrow=TRUE, width=60)
knit_hooks$set(inline = function(x) {
    ## if (is.numeric(x)) return(knitr:::format_sci(x, 'latex'))
    highr::hi_latex(x)
})
### uncomment below to change theme
## knit_theme$get()
## opts_knit$set(out.format='latex')
## thm1 <- knit_theme$get('acid')
## knit_theme$set(thm1)")
  "Default setup options for 'knitr'.
Used by `mp-R-new'.
This list is not exhaustive.
Common options are given as vectors, with a choice indicated by
the index, in square brackets."
  :link '(url-link
	  :tag "Hooks - knitr documentation"
	  "http://yihui.name/knitr/hooks")
  :link '(url-link
	  :tag "Code chunks and package options"
	  "http://yihui.name/knitr/options")
  :type '(string)
  :group 'mp
  :group 'mp-skeleton-string)

(defun mp-chunk-insert (CHUNK)
  "Insert CHUNK, a list of the form (NAME FORMAT CONTENT).
Each element of CHUNK is a string.
No heading level is included when
- `mp-fne' is \".R\"
- `mp-chunk-heading-level' is 'none'."
  ;; heading
  (unless (or (equalp "R" mp-fne)
	      (equalp '("" "" "") mp-chunk-heading-level))
    (insert (concat
	     "\n"
	     mp-chunk-heading-level-prefix
	     (car CHUNK)
	     mp-chunk-heading-level-suffix "\n\n")))
  ;; name and format
  (insert (concat
	   mp-chunk-name-prefix
	   (car CHUNK)))
  ;; format
  (unless (equalp "" (cadr CHUNK))
    (insert (concat
	     mp-chunk-format-prefix
	     ;; enclose format in quotes if has comma and using Sweave
	     ;; (if (and (string-match "," (cadr CHUNK))
	     ;; 	      (equalp "Sweave" (cdr mp-entwiner)))
	     ;; 	 (concat "\"" (cadr CHUNK) "\"")
	       (eval (cadr CHUNK)))))
  (insert (concat mp-chunk-name-suffix "\n"))
  ;; content
  (insert (concat
	   (eval (car (cddr CHUNK)))
	   (unless (equalp "R" mp-fne)
	     "\n")
	   mp-chunk-content-suffix))
  (unless (or (equalp "R" mp-fne)
	      (looking-at "\n\n"))
    (insert "\n\n")))

(defun mp-chunk-format ()
  "Make a string for the default chunk format.
Depends on `mp-entwiner' as follows:
- 'R-pkg-knitr' or 'R-pkg-rmarkdown'
           --> `mp-chunk-format-default-R-pkg'
- 'Sweave' --> `mp-chunk-format-default-sweave'
- 'Org'    --> `mp-chunk-format-default-org'"
  (let ((v1 (cond ((string-match "R-pkg-" (cdr mp-entwiner))
		   mp-chunk-format-default-R-pkg)
		  ((equalp "Sweave" (cdr mp-entwiner))
		   mp-chunk-format-default-sweave)
		  ((equalp "Org" (cdr mp-entwiner))
		   mp-chunk-format-default-org))))
    (if (equalp "Org" (cdr mp-entwiner))
	(mapconcat
	 (lambda (x) (concat (cadr x) " " (cddr x)))
	 (assq-delete-all nil (copy-tree v1))
	 " ")
      (progn
	(setq v1 (rassq-delete-all
		  nil
		  (copy-tree v1)))
	(setq v1 (mapconcat (lambda (x) (car x)) v1 ", "))
	v1))))

(defcustom mp-chunk-format-default-R-pkg
  '(("results='markup'" . t)
    ("comment='##'" . nil))
  "Default format for code chunks with knitr or rmarkdown.
Used for all chunks when `mp-entwiner' is set to one of these.
See also the function `mp-chunk-format'.

Options are given as a list of (KEY . VALUE) pairs.
Non-nil (for the KEY) means the argument will be added.

There are too many possible options to include here for
customizing this variable.
For more options, see `mp-R-chunk-defaults-R-pkg'."
   :type '(alist 
	  :key-type (string :format "%t\n%v"
			    :tag "Option for formatting "
			    :value "")
	  :value-type (boolean :tag "Activate?"
			       :value nil))
  :options '("results='markup'"
	     "comment='##'"
	     "eval=TRUE"
	     "echo=TRUE"
	     "collapse=FALSE"
	     "warning=TRUE"
	     "message=TRUE"
	     "error=TRUE")
  :group 'mp
  :group 'mp-skeleton-string)

(defcustom mp-chunk-format-default-sweave
  '(("results=verbatim" . t)
    ("results=tex" . nil)
    ("echo=TRUE" . nil)
    ("fig=FALSE" . t)
    ("pdf=TRUE" . t)
    ("strip.white=false" . t))
  "Default Sweave format for code chunks.
Used for all chunks when `mp-entwiner' is set to 'Sweave'.
See also the function `mp-chunk-format'.

Options are given as a list of (KEY . VALUE) pairs.
Non-nil (for the KEY) means the argument will be added.

See the manual for more details, at URL
`http://stat.ethz.ch/R-manual/R-devel/library/utils/doc/Sweave.pdf'.

Only *one* plot per chunk is supported by 'Sweave'.
Manually changing to fig=TRUE in the .Rnw file generated
is one simple approach to including plots.

Other possible options include:

- results
  * results=verbatim - results will be read as R code
    (this is equivalent to the option in knitr: results='markup')
  * results=tex - results will be read as TeX
    (this is equivalent to the option knitr results='asis')
- echo
  * echo=TRUE - include R code in output
  * echo=FALSE - no R code is included in output
- fig
  * fig=TRUE - _one_ graphical figure is included
- png=TRUE
  * if png=FALSE, no .png graphics are generated
- strip.white
  * strip.white=false - no not strip blank lines
  * strip.white=all - *all* blank lines are removed
  * strip.white=true - blank lines are removed from top and bottom
- width=6
- height=6
  * inches, for figures
- print
  * print=TRUE - wrap all expressions in print()
- EPS
  * EPS=FALSE - no EPS (.eps) figures are produced.
    EPS figures are required for LaTeX but not PDFLaTeX
- keep.source
  * keep.source=TRUE, do *not* deparse source before 'echo'ing
    i.e. include original source 'as-is'
  * keep.source=FALSE
- quiet
  * quiet=TRUE - *all* progress messages are suppressed
  * quiet=FALSE
- split=FALSE
  * If =TRUE, split over multiple files
- term=TRUE
  * If =FALSE, only output from print() and cat() is 'echo'ed"
  :type '(alist
	  :key-type (string :format "%t\n%v"
			    :tag "Option for formatting "
			    :value "")
	  :value-type (boolean :tag "Activate?"
			       :value nil))
  :options '("results=verbatim"
	     "results=tex"
	     "echo=TRUE"
	     "fig=FALSE"
	     "png=TRUE"
	     "strip.white=false"
	     "strip.white=all"
	     "strip.white=true"
	     "width=6"
	     "height=6"
	     "print=FALSE"
	     "EPS=TRUE"
	     "keep.source=FALSE"
	     "quiet=TRUE")
  :group 'mp
  :group 'mp-skeleton-string)

(defcustom mp-chunk-format-default-org
  '((t . (":session" . "*org-R-session*"))
    (t . (":exports" . "both"))
    (t . (":results" . "output"))
    (t . (":results" . "verbatim"))
    (t . (":results" . "code")))
  "Default knitr format for code chunks.
Used for all chunks when `mp-entwiner' is set to 'Org'.
See also the function `mp-chunk-format'.

Options are given as a list of (KEY1 . (KEY2 . VALUE)) pairs.
For KEY1, a non-nil value means the argument will be added when
combinig these to string form with `mp-chunk-format'.

\":results\" appears multiple times as the options for results
 actually represent four sub-classes:
- collection e.g. \":results output\"
- type e.g. \":results verbatim\", \":results graphics\"
- format e.g. \":results code\", \":results latex\"
- handling e.g. \":results replace\", \":results append\"

For multiple plots per chunk, export the results to e.g. 'p1.pdf'
with the following chunk format options
\":results graphics\", \":file \"p1.pdf\"\".
Then in the .tex docuement change '\\includegraphics{p1.pdf}'
to '\\includepdf[pages=-]{p1.pdf}'.
The command '\\includepdf' is defined in
the LaTeX package 'pdfpages'.

See also info node `(org)Specific header arguments'."
  :type '(alist
	  :key-type (boolean :tag "Activate" :value nil)
	  :value-type
	  (choice :tag "Choose header"
		  (cons
		   :tag "Session"
		   (const :tag "Key" :value ":session" :format "%t %v \n")
		   (string :tag "Value" :value "*org-R-session*"
			   :doc "Name of process in which to run code.
If absent, then code chunks will be run in independent R sessions"
			   :format "%t %v %h \n"))
		  (cons
		   :tag "File"
		   (const :tag "Key" :value ":file" :format "%t %v \n")
		   (string :tag "Value" :value "\"f1.pdf\""
			   :doc "Use double quotes around file name"
			   :format "%t %v %h \n"))
		  (cons
		   :tag "Exports"
		   (const :tag "Key" :value ":exports" :format "%t %v \n")
		   (choice
		    :tag "Value = export what ? "
		    (const :value "both"
			   :doc "code and results"
			   :format "%v - %d")
		    (const :value "code")
		    (const :value "results")
		    (const :value "none")))
		  (cons
		   :tag "Results, class = 'collection'"
		   (const :tag "Key" :value ":results" :format "%t %v \n")
		   (choice :tag "Value = collect what?"
			   (const :value "value"
				  :doc "final value"
				  :format "%v - %d")
			   (const :value "output"
				  :doc "all ouput"
				  :format "%v - %d")))
		  (cons
		   :tag "Results, class='type'"
		   (const :tag "Key" :value ":results" :format "%t %v \n")
		   (choice
		    :tag "Value = convert to..."
		    (const :value "table"
			   :doc "table/ vector"
			   :format "%v - %d")
		    (const :value "list"
			   :doc "org-mode list"
			   :format "%v - %d")
		    (const :value "verbatim"
			   :doc "verbatim/ scalar"
			   :format "%v - %d")
		    (const :value "file"
			   :doc "path/to/file"
			   :format "%v - %d")
		    (const :value "graphics"
			   :doc "need to specify :file also"
			   :format "%v - %d")))
		  (cons
		   :tag "Results, class='format'"
		   (const :tag "Key" :value ":results" :format "%t %v \n")
		   (choice
		    :tag "Value = wrap chunk in what?"
		    (const :value "raw"
			   :doc "nothing; insert as-is"
			   :format "%v - %d")
		    (const :value "org"
			   :doc "BEGIN_SRC org block"
			   :format "%v - %d")
		    (const :value "html"
			   :doc "BEGIN_HTML block"
			   :format "%v - %d")
		    (const :value "latex"
			   :doc "BEGIN_LaTeX block"
			   :format "%v - %d")
		    (const :value "code"
			   :doc "parsable code block"
			   :format "%v - %d")
		    (const :value "pp"
			   :doc "code block for pretty printing
Used for elisp, python, ruby"
			   :format "%v - %d")
		    (const :value "drawer"
			   :doc "a RESULTS drawer"
			   :format "%v - %d\n")))
		  (cons
		   :tag "Results, class='handling'"
		   (const :tag "Key"
			  :value ":results"
			  :format "%t %v \n")
		   (choice
		    :tag "Value = what happens ?"
		    (const :value "replace"
			   :doc "overwrite"
			   :format "%v - %d")
		    (const :value "silent"
			   :doc "echo in minibuffer"
			   :format "%v - %d")
		    (const :value "append")
		    (const :value "prepend")))
		  (cons
		   :tag "Cache"
		   (const :tag "Key"
			  :value ":cache"
			  :format "%t %v \n")
		   (choice
		    :tag "Value = use cache ?"
		    (const :value "yes")
		    (const :value "no")))
		  (cons
		   :tag "Other"
		   (string ":other")
		   (string "other"))))
  :link '(url-link
	  :tag "orgmode manual - header arguments"
	  "http://orgmode.org/manual/Specific-header-arguments.html")
  :group 'mp
  :group 'mp-skeleton-string)

(defcustom mp-bib "bibtex"
  "When non-nil, indicates type of bibliography to use.
If 'none' when calling `mp-example',
no .bib file will be generated in `mp-dd'.
See also `bibtex-mode' for editing .bib files."
  :type '(radio 
	  (const :doc "BibTex (the default)" :value "bibtex")
	  (const :doc "biblatex" :value "biblatex")
	  (const :doc "None" :value nil))
  :group 'mp
  :group 'mp-common)
 
(defun mp-empty-string-to-nil (STRING)
  "Return STRING; if STRING is empty (i.e. \"\", ), return nil.
Called by `mp-find-file-name-interactive' and `mp-R-to-tex'."
  (if (equalp "" STRING)
      nil
    STRING))

(defun mp-entwiner-new ()
  "Make an .Rnw, .org or .Rmd file, as per the value of `mp-entwiner'.
Calls `mp-entwiner-skeleton'.
Called by `mp-R-to-tex'."
  (setq mp-fne (car mp-entwiner))
  (mp-file-display (concat mp-fnb "." mp-fne))
  (mp-entwiner-skeleton)
  (save-buffer)
  (kill-buffer)
  (delete-window))

(define-skeleton mp-entwiner-skeleton
  "Insert contents for an .Rnw or .org file, as per `mp-entwiner'.
Called by `mp-entwiner-new'."
  nil
  (when (equalp "Rmd" (car mp-entwiner))
     "---\n")
  '(mp-doc-class-tex-skeleton)
  (when (equalp "Rmd" (car mp-entwiner))
     "header-includes:\n")
  '(cond
    ((equalp "R-pkg-knitr" (cdr mp-entwiner))
     (mp-preamble-skeleton mp-preamble-knitr))
    ((equalp "Sweave" (cdr mp-entwiner))
     (mp-preamble-skeleton mp-preamble-sweave))
    ((equalp "Org" (cdr mp-entwiner))
     (mp-preamble-skeleton mp-preamble-org)))
  '(mp-preabmle-tex-skeleton)
  '(when (equalp "Rmd" (car mp-entwiner))
     (mp-yaml-end-skeleton mp-fnb))
   ;; after headers
  '(mp-entwiner-beginning-skeleton mp-fnb)
  ;; body of document
  '(goto-char (point-max))
  '(mapc #'mp-chunk-insert mp-chunks)
  \n
  '(when mp-bib
     (mp-insert-bib))
  \n
  '(when (equalp "Rnw" (car mp-entwiner))
     (goto-char (buffer-end 1))
     (mp-tex-end-skeleton))
  \n)

(define-skeleton mp-yaml-end-skeleton
  "Insert end of yaml metadata for an .Rmd file.
The string variable STR is used as the title for the document.
Called by `mp-entwiner-skeleton'."
  "Type title: "
  '(setq str (eval str))

  "\noutput: 
 pdf_document:
  keep_tex: true
  latex_engine: " mp-latex "
  citation_package: " (if (equalp "biblatex" mp-bib)
			  "biblatex"
			"default") "
title: " str \n
  "author: " (mp-author) \n
  "date: '`r format(Sys.Date(), \"%d %B, %Y\")`'
---"
  \n)

(define-skeleton mp-doc-class-tex-skeleton
  "Insert `mp-doc-class-tex' into an file.
Depends on the value of `mp-entwiner'."
  nil
  (cond ((equalp "org" (car mp-entwiner))
	 (concat "#+LATEX_CLASS: " mp-doc-class-tex "\n"))
	 ((or (equalp "Rnw" (car mp-entwiner))
	      (equalp "tex" mp-fne))
	  (concat "\\documentclass{" mp-doc-class-tex "}\n"))
	 ((equalp "Rmd" (car mp-entwiner))
	  (concat "documentclass: " mp-doc-class-tex "\n"))))

(define-skeleton mp-preamble-skeleton
  "Insert preamble, STR.
The string variable STR should be given as latex code.
This may be modified, depending on the value of `mp-entwiner':
Org --> adds `mp-org-latex-header'
Rmd --> changes the formatting so that latex can be interpreted
         as yaml frontmatter."
  "Type a line of latex code: "
  '(setq str (eval str))
  (cond ((equalp "org" (car mp-entwiner))
	 ;; add mp-org-latex-header as prefix to each line
	(concat
	 mp-org-latex-header
	 (replace-regexp-in-string
	  "\n"
	  mp-org-latex-header
	  str "\n")))
	((equalp "Rmd" (car mp-entwiner))
	 (concat
	  (mapconcat (lambda (x)
		       ;; format for yaml
		       (if (string-match "^#" x)
			   (concat " " x)
			 (concat " - '" x "'")))
		     (mapcar (lambda (x)
			       ;; change to yaml-style comments
			       (replace-regexp-in-string
				"^%+" "#" x))
			     (split-string str "\n"))
		     "\n")
	  "\n"))
	((equalp "Rnw" (car mp-entwiner))
	 (concat "\n" str))))

(define-skeleton mp-preabmle-tex-skeleton
  "Insert preamble for tex documents.
Called by `mp-entwiner-new' and `mp-tex-new'."
  nil
  '(mp-preamble-skeleton
    (if (equalp "pdflatex" mp-latex)
       (concat "%%% LaTeX font\n\\usepackage[T1]{fontenc}\n"
	       (string-trim-left mp-latex-font))
      mp-xetex-font))
  '(mp-preamble-skeleton mp-preamble-tex) 
  '(when mp-add-preamble-tex-extra
     (mp-preamble-skeleton mp-preamble-tex-extra))
  '(when mp-bib
     (mp-preamble-skeleton mp-preamble-bib))
  '(when (equalp "biblatex" mp-bib)
     (mp-preamble-biblatex-skeleton mp-fnb))
  \n)

(defcustom mp-preamble-bib
  "% for bibliography
\\usepackage{url}
\\usepackage{doi}
% ensure special characters like underscore appear unchanged in bibliography
% the starred form of renewcommand forbids the arguments from containing multiple paragraphs of text
\\renewcommand*{\\doi}[1]{\\href{https://doi.org/\\detokenize{#1}}{ \\detokenize{#1}}}
\\renewcommand*{\\doiurl}[1]{\\href{https://doi.org/\\detokenize{#1}}{ \\detokenize{#1}}}
\\renewcommand*{\\url}[1]{\\href{https://doi.org/\\detokenize{#1}}{ \\detokenize{#1}}}"
  "Preamble when using a bibliography.
Called by `mp-preabmle-tex-skeleton'."
  :type '(string)
  :group 'mp
  :group 'mp-skeleton-string)

(defcustom mp-preamble-biblatex
  "\\usepackage[backend=biber, bibstyle=numeric-verb]{biblatex}
\\ExecuteBibliographyOptions{maxnames=99}
\\ExecuteBibliographyOptions{sorting=ydnt, block=space}
\\ExecuteBibliographyOptions{giveninits=true, terseinits=true}
\\ExecuteBibliographyOptions{defernumbers=true}"
  "Additional preamble when `mp-bib' is \"biblatex\".
Caled by `mp-preabmle-tex-skeleton'.
The options available are too numerous to provide here in detail.
See the biblatex manual for more.
See `TeX-doc' i.e. (TeX-doc packageName) for
more details on particular packages.

When editing this with `customize', use
`electric-newline-and-maybe-indent' for carraige return."
  :type '(string)
  :link '(url-link
	  :tag "biblatex manual on CTAN"
	  "http://mirrors.ibiblio.org/CTAN/macros/latex/contrib/biblatex/doc/biblatex.pdf")
  :group 'mp
  :group 'mp-skeleton-string)

(define-skeleton mp-preamble-biblatex-skeleton
  "Insert `mp-preamble-biblatex'.
The string variable STR is the `file-name-base' for the .bib file.
Called by `mp-preabmle-tex-skeleton'."
  "Type file-name-base: "
  '(setq str (eval str))
  '(mp-preamble-skeleton mp-preamble-biblatex)
  '(mp-preamble-skeleton
    (concat "\\addbibresource{" str ".bib}")))

(defcustom mp-doc-class-tex "article"
  "The 'documentclass' used by tex."
  :type  '(radio 
	   (const :value "article"
		  :doc "The default.")
	   (const :value "ltxdoc"
		  :doc "Loads article class.")
	   (const :value "proc"
		  :doc "For poceedings; based on the article class.")
	   (const :value "minimal"
		  :doc "Only sets page size and base font.")
	   (const :value "standalone")
	   (const :value "report"
		  :doc "Can contain chapters.")
	   (const :value "book")
	   (const :value "novel"
		  :doc "Printed novels & short story collections.
Needs lualatex.")
	   (const :value "memoir"
		  :doc "Can contain chapters.")
	   (const :value "letter")
	   (const :value "slides"
		  :doc "Uses big sans serif letters.")
	   (const :value "beamer")
	   (string :value ""
		   :doc "Enter class below:"
		   :format "%d %v"))
  :link '(url-link
	  :tag "Packages with alternative LaTeX class(es)."
	  "http://ctan.org/topic/class")
  :link '(url-link
	  :tag "What are the available 'documentclass' types and their uses?\n"
	  "http://tex.stackexchange.com/questions/782")
  :group 'mp
  :group 'mp-skeleton-string)

(defcustom mp-preamble-knitr
  "%%% knitr defaults
\\usepackage[]{graphicx}
\\usepackage[]{color}
\\usepackage{framed}
% recommended with 'knitr'
\\usepackage{alltt}
\\usepackage{mathtools}
\\usepackage[sc]{mathpazo}
\\usepackage{geometry}
\\geometry{verbose, tmargin=2.5cm, bmargin=2.5cm,
  lmargin=2.5cm, rmargin=3cm}
\\setcounter{secnumdepth}{2}
\\setcounter{tocdepth}{2}
\\usepackage{url}
\\usepackage{hyperref}
\\hypersetup{unicode=true, pdfusetitle}
\\hypersetup{bookmarks=true, bookmarksnumbered=true}
\\hypersetup{bookmarksopen=true, bookmarksopenlevel=2}
\\hypersetup{breaklinks=false, pdfborder={0 0 1}}
\\hypersetup{backref=false}
\\hypersetup{colorlinks=true}
\\definecolor{myDarkBlue}{rgb}{0, 0, 0.5}
\\hypersetup{linkcolor=myDarkBlue}
\\hypersetup{citecolor=myDarkBlue}
\\hypersetup{pdfstartview={XYZ null null 1}}"
  "Default preamble when `mp-entwiner' is 'knitr'.

When editing this with `customize', use
`electric-newline-and-maybe-indent' for carraige return."
  :type '(string)
  :group 'mp
  :group 'mp-skeleton-string)

(defcustom mp-preamble-sweave
  "%%% Sweave defaults
\\usepackage{Sweave}
\\usepackage{lmodern}
% for multiple plots per .pdf
% with e.g. \includepdf[pages=-]{p1.pdf}
\\usepackage{pdfpages}
\\usepackage{color}
\\usepackage{hyperref}
\\hypersetup{colorlinks=true}
\\definecolor{myDarkBlue}{rgb}{0, 0, 0.5}
\\hypersetup{linkcolor=myDarkBlue}
\\hypersetup{citecolor=myDarkBlue}"
  "Default preamble when `mp-entwiner' is 'Sweave'.

Note that the file 'Sweave.sty' needs to be on the path
 used by TeX e.g. run the shell command
kpsewhich 'Sweave.sty'.
If not, see `mp-add-sweave-to-TeX-path'.

When editing this with `customize', use
`electric-newline-and-maybe-indent' for carraige return."
  :type '(string)
  :group 'mp
  :group 'mp-skeleton-string)

(defun mp-add-sweave-to-TeX-path ()
    "Add 'Sweave.sty' to the search path used by 'kpsewhich'."
    (start-process-shell-command
     "Add Sweave to TeX path" "*make-pdf*"
     "cp --recursive $(locate Sweave.sty | grep --only-matching .*texmf/) ~/; texhash"))

(defconst mp-org-latex-header "\n#+LATEX_HEADER: "
  "Prefix used when insterting latex preamble into an .org file.")

(defcustom mp-preamble-org
  "%%% Org-mode defaults
% for multiple plots per .pdf
% with e.g. \includepdf[pages=-]{p1.pdf}
\\usepackage{pdfpages}
% listings package
\\lstloadlanguages{[Auto]Lisp}
\\lstloadlanguages{R}
\\lstloadlanguages{[LaTeX]TeX}
\\lstset{frame=single}
\\lstset{framerule=0pt}
\\lstset{basicstyle=\\ttfamily\\small}
\\lstset{columns=fullflexible}
\\lstset{showstringspaces=false}
\\lstset{breaklines=true}
\\usepackage{textcomp} % for upquote option, below
\\lstset{upquote=true}
% custom colors (from package{color})
\\definecolor{myGrey}{gray}{0.95}
\\lstset{backgroundcolor=\\color{myGrey}}
\\definecolor{myOrange}{rgb}{0.85, 0.23, 0}
\\lstset{keywordstyle=\\color{myOrange}}
\\definecolor{mySteelBlue}{rgb}{0.233, 0.433, 0.6}
\\lstset{commentstyle=\\color{mySteelBlue}}
\\definecolor{myDarkSkyBlue}{rgb}{0.0, 0.636, 0.85}
\\lstset{stringstyle=\\color{myDarkSkyBlue}\\sffamily}
% hyperref package
\\definecolor{myDarkBlue}{rgb}{0, 0, 0.5}
\\hypersetup{colorlinks=true}
\\hypersetup{linkcolor=myDarkBlue}
\\hypersetup{citecolor=myDarkBlue}"
  "Default preamble when `mp-entwiner' is Org.

When editing this with `customize', use
`electric-newline-and-maybe-indent' for carraige return."
  :type '(string)
  :group 'mp
  :group 'mp-skeleton-string)

(defcustom mp-latex "pdflatex"
  "Type of latex to use.
Given as shell commend (a string),
that generates a .pdf from a .tex (TeX) file.
Options are \"pdflatex\", \"xelatex\" and \"lualatex\".
Used by `mp-shell-sentinel'."
  :type '(radio
	  (const
	   :doc "pdfTeX. The default."
	   :value "pdflatex")
	  (const
	   :doc "XeTeX. For use with OpenType fonts."
	   :value "xelatex")
	  (const
	   :doc "LuaTeX. For use with 'lua' scripts."
	   :value "lualatex"))
  :group 'mp)

(defcustom mp-latex-font
  "\\usepackage{mathpazo}
\\linespread{1.05}
\\usepackage[scaled]{helvet}
\\usepackage{courier}"
  "Font(s) to use when `mp-latex' is \"pdflatex\".
Value used by Called by `mp-entwiner-new'
when `mp-latex' is \"pdflatex\".

Many font options are specified successively as:
 1. serif
 2. sans-serif
 3. monospace font

When editing this with `customize', use
`electric-newline-and-maybe-indent' for carraige return."
  :link '(url-link
	  :tag
	  "'Fonts' in 'LaTeX Export for Org Mode'"
	  "http://orgmode.org/worg/org-tutorials/org-latex-export.html")
  :type '(radio
	  (string
	   :tag "Palatino family. The default.
An old-style serif font.\n" 
	   :value "\\usepackage{mathpazo}
\\linespread{1.05}
\\usepackage[scaled]{helvet}
\\usepackage{courier}")
	  (string
	   :tag "Bera family.
Bitstream's Vera family of fonts.\n"
	   :value "\\usepackage[scaled]{beraserif}
\\usepackage[scaled]{berasans}
\\usepackage[scaled]{beramono}")
	  (string
	   :tag "Charter family.\n"
	   :value "\\usepackage[bitstream-charter]{mathdesign}
\\usepackage[scaled=.9]{helvet}
\\usepackage{courier} % tt")
	  (string
	   :tag "Garamond family.
Elegant, old-style serif.\n"
	   :value"\\usepackage[urw-garamond]{mathdesign}
\\usepackage{lmodern}
\\usepackage{courier}
\\linespread{1.0609}")
	  (string
	   :tag "Kepler family.
Recommended for displaying math.\n"
	   :value "\\usepackage{kpfonts}")
	  (string
	   :tag "Libertine family.
A replacement for Times New Roman.\n"
	   :value "\\usepackage{libertine}
\\renewcommand*\\oldstylenums[1]{{\\fontfamily{fxlj}\selectfont #1}}
\\usepackage{lmodern}")
	  (string
	   :tag "Nimbus family.
Elegant, old-style serif.\n"
	   :value "\\usepackage{tgtermes}           % for Times New Roman
\\usepackage[scale=.85]{tgheros} % for Helvetica
\\usepackage{tgcursor}           % for Courier")
	  (sexp :tag "Custom"
		:value ""))
  :group 'mp
  :group 'mp-skeleton-string) 

(defcustom mp-xetex-font 
  "%%% XeTeX font 
% This needs to be placed after maths font packages,
% particularly 'euler'
\\usepackage{fontspec}
\\defaultfontfeatures{Mapping=tex-text}
% Main text font
\\setmainfont[Ligatures={Rare, TeX, NoCommon}, Numbers={Lowercase}]{Linux Libertine O}
\\fontsize{12 pt}{16 pt}
\\selectfont"
  "Font(s) to use when `mp-latex' is \"xelatex\" or \"lualatex\".
See also `mp-xetex-font-skeleton'.
Takes the form '\\setmainfont[options]{font}'.
- Common TeX commands include:
 * \\setmainfont{}
 * \\setsansfont{}
 * \\setmonofont{}

- TeX Gyre (opentype) equivalents for some
 common fonts include:  
 | Origin                 | TeX Gyre |
 |------------------------+----------|
 | Palatino               | Pagella  |
 | Times                  | Termes   |
 | Helvetica              | Heros    |
 | ITC Avant Garde Gothic | Adventor |
 | New Century Schoolbook | Schola   |

- Common font options include:
 * Ligatures
  - Required/NoRequired,
  - Common/NoCommon,
  - Rare/Discretionary,
  - Historic, TeX
 * Letters
  | Uppercase  | SmallCaps | UppercaseSmallCaps |
  | PetiteCaps | UppercasePetiteCaps | Unicase  |
 * Numbers
  - Uppercase, Lowercase
  - Proportional, Monospaced
  - SlashedZero
  - Arabic
 * Fractions
  - On, Alternate
 * Style
  | Alternate | Italic | Swash |
  | Historic  | TitlingCaps   ||"
  :link '(url-link
	  :tag
	  "Fontspec documentation"
	  "http://mirror.unl.edu/ctan/macros/latex/contrib/fontspec/fontspec.pdf")
  :link '(url-link
	  :tag "Free fonts at fontsquirrel"
	  "http://www.fontsquirrel.com/")
  :type '(string)
  :group 'mp
  :group 'mp-skeleton-string)

(defcustom mp-preamble-tex
  "%%% Default preamble for tex
% for nice tables
\\usepackage{booktabs}
% force floats to appear here with e.g.
% \\begin{figure}[H]
\\usepackage{float}
% for tables > 1 page
\\usepackage{longtable}
% for table entries spanning rows
\\usepackage{multirow}
% for command \setstretch{}
\\usepackage{setspace}
\\usepackage[math]{blindtext}
% page numbers appear in top-right corner
\\pagestyle{headings}
% suppress most error messages
\\hbadness=15000
\\vbadness=15000"
  "Default preamble for documents created with `mp-entwiner-new'.

See `TeX-doc' i.e. (TeX-doc packageName) for
more details on particular packages.

When editing this with `customize', use
`electric-newline-and-maybe-indent' for carraige return."
  :type '(string)
  :group 'mp
  :group 'mp-skeleton-string)

(defcustom mp-add-preamble-tex-extra nil
  "Insert `mp-preamble-tex-extra' ?"
  :type '(boolean)
  :group 'mp
  :group 'mp-skeleton-string)

(defcustom mp-preamble-tex-extra
  "%%% Additional default preamble
% for e.g. \\formatdate
\\usepackage{datetime}
% for command  \\setstretch{}
\\usepackage{setspace}
% for SI units
\\usepackage{siunitx}
\\sisetup{per-mode=symbol}
% for chemical symbols
\\usepackage[version=3]{mhchem}
% to use forced 'here'
% e.g. \\begin{figure}[H]
\\usepackage{float}
% for large numbers of floats
\\usepackage{morefloats}
% to keep floats in same section
\\usepackage[section]{placeins}"
  "Extra TeX preamble.
This is added when `mp-add-preamble-tex-extra' is t.

Some additional useful TeX packages.
Used by `mp-preamble-tex-extra-skeleton'.

See `TeX-doc' i.e. (TeX-doc packageName) for more details on particular packages.

When editing this with `customize', use
`electric-newline-and-maybe-indent' for carraige return."
  :type '(string)
  :group 'mp
  :group 'mp-skeleton-string)

(define-skeleton mp-entwiner-beginning-skeleton
   "Insert the beginning of an entwiner file.
This is an .Rnw, .org, or .Rmd file.
This is the material to appear after the headers which specify
options for the entwiner.
Title is the required variable str."
   "Type title: "
   \n 
   '(cond
      ((equalp "org" (car mp-entwiner))
       (mp-org-title-skeleton str))
      ((equalp "Rnw" (car mp-entwiner))
       (mp-tex-title-skeleton str))
      ((equalp "Rmd" (car mp-entwiner))
       nil))
   \n
   '(mp-tex-beginning-skeleton)
   \n)

(define-skeleton mp-tex-title-skeleton
  "Insert title and author into an .Rnw file.
The string variable STR (required) provides the title.
Author is given by `mp-author'.
Includes the TeX commands:
 - \\maketitle
 - \\pagestyle{}
 - \\tableofcontents
Called by `mp-entwiner-beginning-skeleton'."
  "Type title: "
  \n \n
  "%%%%" \n
  "\\begin{document}" \n
  "%%%%" \n \n
  "\\title{" str "}" \n
  "\\author{" (mp-author) "}" \n
  "\\maketitle" \n
  "\\tableofcontents")

(define-skeleton mp-org-title-skeleton
  "Insert title and author into an .org file.
The string variable STR (required) provides the title.
Author is given by the function `mp-author'.
Called by `mp-entwiner-beginning-skeleton'."
  "Type title: "
  \n \n
  "#+TITLE: " str \n
  "#+AUTHOR: " (mp-author)
  \n \n)

(defun mp-author ()
  "Return the variable `user-full-name'.
If this is an empty string, return the variable `user-login-name'"
  (if (equal user-full-name "")
      user-login-name
    user-full-name))

(define-skeleton mp-tex-beginning-skeleton
  "Insert `mp-tex-beginning'."
  nil
  \n
  mp-tex-beginning
  \n \n)

(defcustom mp-tex-beginning
  "\\medskip
Example of text. Here are some holoalphabetic sentences:

The quick brown fox jumps over the lazy dog.
PACK MY BOX WITH FIVE DOZEN LIQUOR JUGS.
Jackdaws love my big Sphinx of Quartz.

Here is an example of an equation:
\\begin{equation}
\\bar{x} = \\frac{1}{n}\\sum_{i=1}^{i=n} x_i = \\frac{x_1 + x_2 + \\dots{} + x_n}{n}
\\end{equation}

\\medskip"
  "Text to place at the beginning of a .tex file.
This is used to provide a sample of the font.

Used by the skeleton `mp-tex-beginning-skeleton'.

Called by `mp-entwiner-beginning-skeleton'.

When editing this with `customize', use
`electric-newline-and-maybe-indent' for carraige return."
  :type '(string)
  :group 'mp
  :group 'mp-skeleton-string)

(defcustom mp-bibtex-style "plain"
  "Bibliography style to use.
Used by `mp-bibtex-skeleton'.
It should be the `file-name-base' of a file ending in .bst,
which is on the 'path' as determined by
'kpsewhich --show-path=bst'."
  :type
  '(radio
    (const :doc "hyperlink URLs, dois"
	   :value "plainurl")
    (const :doc "a simple default"
	   :value "plain")
    (const :doc "abbreviate"
	   :value "abbrv")
    (const :doc "Association for Computing Machinery"
	   :value "acm")
    (const :doc "sorts by label"
	   :value "alpha")
    (const :doc "American Psychological Association"
	   :value "apalike")
    (const :doc "Institute of Electrical and Electronics Engineers - Transactions"
	   :value "ieeetr")
    (const :doc "Society for Industrial and Applied Mathematics"
	   :value "siam")
    (const :doc "unsorted"
	   :value "unsrt")
    (string :doc "other"
	    :value ""))
  :safe (lambda (x)
	  (= 0 (shell-command (concat "kpsewhich " x ".bst"))))
  :group 'mp
  :group 'mp-skeleton-string)

(define-skeleton mp-tex-end-skeleton
  "Insert `mp-tex-end'."
  nil
  mp-tex-end)

(defcustom mp-tex-end
  "\n%%%%\n\\end{document}"
  "Default ending for .tex files."
  :type '(string)
  :group 'mp
  :group 'mp-skeleton-string)

(defun mp-insert-bib ()
  "Insert bibliography.
Called by `mp-entwiner-new'.
See `mp-bibtex' and `mp-biblatex'."
  (cond ((equalp "none" mp-bib)
	 nil)
	((equalp "Rmd" (car mp-entwiner))
	 (mp-Rmd-skeleton))
	((equalp "bibtex" mp-bib)
	 (mp-bibtex-skeleton mp-fnb))
	((equalp "biblatex" mp-bib)
	 (mp-biblatex-skeleton))))

(defun mp-Rmd-skeleton ()
  "Insert bibliography commands."
  (if (equalp "biblatex" mp-bib)
	(insert "\\nocite{*}")
      (mp-bibtex-skeleton mp-fnb)))

(define-skeleton mp-bibtex-skeleton
  "Insert bibliography.
The string variable STR is the `file-name-base' for
the bibliography.
See also `mp-bibtex-style'."
  "Type file-name-base: "
  > "\\nocite{*}" \n
  > "\\bibliographystyle{" mp-bibtex-style "}" \n
  > "\\bibliography{" str "}"
  \n \n)

(define-skeleton mp-biblatex-skeleton
  "Insert bibliography."
  nil
  > "\\nocite{*}" \n
  > "\\printbibliography[title=References]" \n \n)

(defun mp-Rnw-to-tex ()
  "Generate a .tex (TeX) file from an .Rnw file.
Once complete, `mp-tex-to-pdf' will be run on the output."
  (message "mp-Rnw-to-tex...")
  (mp-shell-sentinel "mp-Rnw-to-tex")
  (message "mp-Rnw-to-tex...done"))

(defun mp-Rmd-to-pdf ()
  "Generate a .tex (TeX) file from an .Rnw file.
Once complete, `mp-latex-pdf' will be run on the output."
  (message "mp-Rmd-to-pdf...")
  (mp-shell-sentinel "mp-Rmd-to-pdf")
  (message "mp-Rmd-to-pdf... done"))

(defvar mp-sentinel-done nil
  "Value is 't' when no error results from `mp-shell-sentinel'.")

(defcustom mp-sentinel-sleep 0.01
  "Seconds to sleep to allow a process sentinel to complete.
A higher value provides more time for the buffer *make-pdf*
to update/ display messages from the process being called.
See also `sleep-for' and `set-process-sentinel'.
Called by `mp-Rnw-to-tex', `mp-latexmk' and `mp-shell-sentinel'."
  :type 'number
  :group 'mp)

(defun mp-shell-sentinel (CALLED-FROM)
  "Run appropriate `start-process-shell-command', per CALLED-FROM.
This is run with a sentinel, set via `set-process-sentinel.'
The purpose of the sentinel is to catch errors in the process,
as flagged when the return value of the process is not zero.
Such errors are returned as `warn'ings.
The shell command/ process that is run depends
on the value of CALLED-FROM and of the `cdr' of `mp-entwiner':

  CALLED-FROM     (cdr mp-entwiner)  process
  mp-Rnw-to-tex   knitr              Rscript -e knitr::knit
                  Sweave             R CMD Sweave
  mp-Rmd-to-pdf                      Rscript -e rmarkdown::render
  mp-Rscript-run                     Rscript `mp-fnb'.`mp-fne'
  mp-latexmk                         latexmk
  mp-view-pdf                        `mp-pdf-viewer'

The command latexmk is called with the arguments:
-bibtex, if `mp-bib' is non-nil.
-from `mp-latex-options'

An error in the process leads to calling `mp-exit'."
  (message
   (concat "mp-shell-sentinel, called from "
	   CALLED-FROM " ..."))
  (pop-to-buffer "*make-pdf*")
  ;; we pause here until the process we're calling here
  ;; exits with (thrwo)
  (lexical-let
      ((CALLED-FROM CALLED-FROM)
       (default-directory mp-dd)
       (buffer1 (if (equalp "mp-view-pdf" CALLED-FROM)
		    nil
		  "*make-pdf*"))
       (command1
	(cond
	 ((equalp "mp-Rscript-run" CALLED-FROM)
	  (concat
	   "Rscript --verbose --no-init-file " mp-fnb ".R"))
	  ((equalp "mp-Rnw-to-tex" CALLED-FROM)
	   (if (equalp "R-pkg-knitr" (cdr mp-entwiner))
	      ;; -e = expression,
	      ;; text following this needs to be in double quotes
	      (concat
	       "Rscript --verbose --no-init-file -e \"
knitr::knit('" mp-fnb ".Rnw')\"")
	    (concat "R CMD Sweave " mp-fnb ".Rnw")))
	 ((equalp "mp-Rmd-to-pdf" CALLED-FROM)
	  (concat
	   "Rscript --verbose --no-init-file -e \"
rmarkdown::render('" mp-fnb ".Rmd')\""))
	 ((equalp "mp-latexmk" CALLED-FROM)
	  (concat "latexmk "
		  (when (equalp "bibtex" mp-bib)
		    "-bibtex ")
		  mp-fnb ".tex " (mp-latex-options)))
	 ((equalp "mp-view-pdf" CALLED-FROM)
	  (concat mp-pdf-viewer " " mp-fnb ".pdf")))))
    (set-process-sentinel
     (start-process-shell-command
      CALLED-FROM buffer1 command1)
     (lambda (process1 event1)
       (if (= 0 (process-exit-status process1))
	   ;;      (if (equalp event1 "finished\n")
	   (progn
	     (throw 'exit nil)
	     (message
	      (concat "mp-shell-sentinel, called from "
		      CALLED-FROM " ... done")))
	 (progn
	   (kill-buffer "*make-pdf*")
	   (delete-window)
	   (setq mp-exiting t)
	   (mp-exit
	    (concat
	     "Error in mp-shell-sentinel, called from `"
	     (process-name process1) "'"))))))
    (recursive-edit)))

(defcustom mp-latex-options '(("-interaction=nonstopmode" . t))
  "Command-line options to be added to `mp-latex'.
These are given in the form of a list of (KEY . VALUE) pairs,
where KEY is the option.
When the VALUE is non-nil, the KEY will be added.
See also the function `mp-latex-options'
which combines these as a string."
  :type '(alist
	  :key-type (choice
		     :tag "other"
		     (string
		      :tag "argument"))
	  :value-type (boolean
		       :tag "Include ? "
		       :value nil))
  :options '("-interaction=nonstopmode"
	     "-shell-escape"
	     "-8bit"
	     "-interaction=errorstopmode"
	     "-enc"
	     "-etex"
	     "-mltex"
	     "-output-format=pdf")
  :group 'mp)

(defun mp-latex-options ()
  "Combine the variable `mp-latex-options' as a string."
  (let ((v1 (rassq-delete-all nil (copy-alist mp-latex-options))))
    (mapconcat #'car v1 " ")))

(defvar mp-exiting nil
  "Value is 't' when exiting from `mp-shell-sentinel'.")

(defun mp-latexmk ()
  "Make a .pdf file, using the method given by `mp-latex'.
Add file `latexmkrc' file to `mp-dd' if needed.
Once complete, open the file with `mp-view-pdf'.
Called by the functions
- `mp-tex-to-pdf'
- `mp-Rnw-to-tex'
- `mp-org-to-tex'.
See the manual for details:
URL `http://ctan.mackichan.com/support/latexmk/latexmk.pdf'."
  (message "mp-latexmk...")
  (unless (directory-files
	   mp-dd nil "^.latexmkrc$")
    (mp-file-display ".latexmkrc")
    (mp-latexmkrc-skeleton)
    (save-buffer)
    (kill-buffer)
    (delete-window))
  (mp-shell-sentinel "mp-latexmk")
  (message "mp-latexmk... done"))

(define-skeleton mp-latexmkrc-skeleton
  "Insert `mp-latexmkrc'.
Adds the value of `mp-latex'."
  nil
  mp-latexmkrc \n
  "$pdflatex = '" mp-latex " %O %S ';"
  \n)

(defcustom mp-latexmkrc
  "# Custom dependency for 'glossaries' package.
add_cus_dep('glo', 'gls', 0, 'makeglo2gls')\;
sub makeglo2gls{
 system(\"makeindex -s \\\"$_[0].ist\\\" -t \\\"$_[0].glg\\\" -o \\\"$_[0].gls\\\" \\\"$_[0].glo\\\" \")\;
}
# Add support for the [acronym] option in the' glossaries' package.
add_cus_dep('acn', 'acr', 0, 'makeacn2acr')\;
sub makeacn2acr{
 system(\"makeindex -s \\\"$_[0].ist\\\" -t \\\"$_[0].alg\\\" -o \\\"$_[0].acr\\\" \\\"$_[0].acn\\\" \")\;
}
# This is for the 'new glossary type' command
# \\newglossary[nlg]{notation}{not}{ntn}{Notation}
add_cus_dep('ntn', 'not', 0, 'makentn2not')\;
sub makentn2not{
 system(\"makeindex -s \\\"$_[0].ist\\\" -t \\\"$_[0].nlg\\\" -o \\\"$_[0].not\\\" \\\"$_[0].ntn\\\" \")\;
}
# Dependencies for custom indexes using the 'index' package
 add_cus_dep('adx', 'and', 0, 'makeadx2and')\;
sub makeadx2and{
 system(\"makeindex -o \\\"$_[0].and\\\" \\\"$_[0].adx\\\" \")\;
}
add_cus_dep('ndx', 'nnd', 0, 'makendx2nnd')\;
sub makendx2nnd {
 system(\"makeindex -o \\\"$_[0].nnd\\\" \\\"$_[0].ndx\\\" \")\;
}
add_cus_dep('ldx', 'lnd', 0, 'makeldx2lnd')\;
sub makeldx2lnd{
 system(\"makeindex -o \\\"$_[0].lnd\\\" \\\"$_[0].ldx\\\" \")\;
}
# Custom dependency and function for 'nomencl' package
add_cus_dep('nlo', 'nls', 0, 'makenlo2nls')\;
sub makenlo2nls{
 system(\"makeindex -s nomencl.ist -o \\\"$_[0].nls\\\" \\\"$_[0].nlo\\\" \")\;
}
# tex -> pdf
$pdf_mode = 1;
$postscript_mode = $dvi_mode = 0; "
  "The runcom file '.latexmkrc'.
It is an initialization file or 'runcom'
\(run commands, meaning 'run configuration') for latexmk.

This variable is used by `mp-latexmk'.
It is recommended to have this on your path.
It supports the use of glossaries, acronymns and indices amongst others.
No support is provided for the (now deprecated) 'glossary' package.

A link to the source is available as a link in the custmomize help.

When editing this with `customize', use
`electric-newline-and-maybe-indent' for carraige return."
  :type '(string :tag "# indicates comments")
  :link '(url-link
	  :tag "example: pdflatexmkrc"
	  "http://ctan.math.washington.edu/tex-archive/support/latexmk/example_rcfiles/pdflatexmkrc")
  :group 'mp
  :group 'mp-skeleton-string)

(define-skeleton mp-bib-skeleton
  "Insert `mp-bib-default'."
  nil
  mp-bib-default \n \n)

(defcustom mp-bib-default
  "%%% Example of an article from a magazine or a journal:
@Article{dardis,
   Author={Dardis, C. and Woolf, E.C. and Scheck, A.C.},
   Title={A tool for reproducible research: From data analysis (in R) to a typeset laboratory notebook (as .pdf) using the text editor Emacs with the 'mp' package},
   Journal={F1000 Research},
   Year={2016},
   Volume={4},
   Number={483},
   Pages={1--21},
   doi={10.12688/f1000research.6800.2}
}"
  "Default entry/entries for a .bib file.
Used by the skeleton `mp-bib-skeleton'.
See also `bibtex-mode'.

When editing this with `customize', use
`electric-newline-and-maybe-indent' for carraige return."
  :type '(string)
  :group 'mp
  :group 'mp-skeleton-string)

(defun mp-org-to-tex ()
  "Convert `mp-fnb'.org to `mp-fnb'.tex."
  (mp-file-display (concat mp-fnb ".org"))
  (mp-ox-settings)
  (let ((ess-ask-for-ess-directory nil))
    (org-latex-export-to-latex))
  (kill-buffer)
  (delete-window)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer "*org-R-session*")))

(defun mp-ox-settings ()
  "Set export settings for `org-mode'.
All variables are set as buffer-only (see `make-local-variable').

Adds support for 'R', 'latex' and 'emacs-lisp' to
`org-babel-load-languages'.
Sets the following to nil:
 - `org-confirm-babel-evaluate'
 - `org-latex-with-hyperref'
 - `org-latex-table-caption-above'.
Sets `org-latex-listings' to t.
Adds 'listings' and 'color' to `org-latex-packages-alist'."
  ;; org-export latex + org-babel language support
  (mapc #'require
	'(ox-latex ob-R ob-emacs-lisp ob-latex))
  (set 'org-startup-folded nil)
  (set 'org-latex-table-caption-above nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t) (latex . t) (emacs-lisp . t)))
  (set (make-local-variable
	'org-confirm-babel-evaluate) nil)
  (set (make-local-variable
	'org-latex-with-hyperref) nil)
  (set (make-local-variable
	'org-latex-listings) t)
  (add-to-list 'org-latex-packages-alist
	       '("" "listings"))
  (add-to-list 'org-latex-packages-alist
	       '("" "color")))

(defun mp-tex-to-pdf ()
  "Convert .tex to .pdf."
  ;; TODO consider add (mp-update-latex)
  (let ((b1 (concat mp-fnb ".bib"))
	(t1 (concat mp-fnb ".tex")))
    (when (string-match-p
	   (regexp-quote b1)
	   (format "%s" (buffer-list)))
      (with-current-buffer b1
	(save-buffer)
	(bibtex-validate)))
    (when (or (equalp "Org" (cdr mp-entwiner))
	      (equalp "Sweave" (cdr mp-entwiner)))
      (mp-file-display t1)
      (save-excursion
	(goto-char (point-min))
	(while (search-forward-regexp
		;; match up to first curly bracket {
		"^\\\\includegraphics[^{]*" nil t)
	  (replace-match "\\\\includepdf[pages=-, width=0.9\\\\linewidth]")))
      (save-buffer)
      (kill-buffer-and-window)))
  (mp-latexmk))

(defun mp-exit (STRING)
  "Exit 'mp' in case of error.
The `message' STRING takes the place of a call to `warn'.
`kill-buffer' is called on any buffers generated during the process."
  (let ((b1 (current-buffer)))
    (pop-to-buffer b1))
    (when mp-clean
      (mp-clean))
    (balance-windows)
    (message STRING))

(defun mp-view-pdf ()
  "View pdf with `mp-pdf-viewer'."
  (if (equalp "DocView" mp-pdf-viewer)
      (progn
	(pop-to-buffer "*make-pdf*")
	(erase-buffer)
	(insert-file-contents (concat mp-fnb ".pdf"))
	(doc-view-mode))
    (mp-shell-sentinel "mp-view-pdf")))

(defcustom mp-pdf-viewer "DocView"
  "Method used to view a .pdf file.
On Linux, this defaults to `doc-view-mode'.
Otherwise, this is the `shell-command' to view a .pdf file.
\(If so, this command should be an executable found on
 the `exec-path').

This value is used by the functions
 `mp-latex-pdf' and `mp-latexmk'.

Some useful URLs for downloads are given in the `custmomize' help."
  :type
  '(radio (const :doc "Default on Linux. Part of GNU Emacs."
		 "DocView")
	  (const :doc "Default on Windows. Cross platform."
		 "evince")
	  (const :doc "Alternative for Windows."
		 "sumatrapdf")
	  (const :doc "Good for Linux/Ubuntu."
		 "xpdf")
	  (const :doc "Adobe Acrobat Reader."
		 "acrord32")
	  (string :tag "Enter an alternative program here."
		  ""))
  :link '(url-link
	  :tag "DocView"
	  "http://www.gnu.org/software/emacs/manual/html_node/emacs/Document-View.html")
  :link '(url-link
	  :tag "evince"
	  "http://wiki.gnome.org/Apps/Evince")
  :link '(url-link
	  :tag "sumatra"
	  "http://www.sumatrapdfreader.org/free-pdf-reader.html")
  :link '(url-link
	  :tag "xpdf"
	  "http://www.foolabs.com/xpdf/")
  :link '(url-link
	  :tag "Adobe Acrobat Reader"
	  "http://get.adobe.com/reader/")
  :group 'mp)

(defun mp-chunks-read ()
  "Read a `list' of chunks of R code.
Overwrite `mp-chunks' with these new values.
Ensure no duplicate names with `mp-chunk-name-duplicates'.
Return `mp-chunks'."
  ;; (mp-chunk-set-adfixes) is called by (mp-file-display)
  (mp-chunk-set-adfixes)
  ;; cnp1 = chunk name prefix
  ;; cfp1 = chunk format prefix
  ;; ccs1 = chunk content suffix
  (let ((cnp1
	 (concat "^" (regexp-quote mp-chunk-name-prefix)))
	(cfp1 (if (string-match "," mp-chunk-format-prefix)
		  ;; match up to first comma
		  (concat "^[^,]+,")
		(regexp-quote mp-chunk-format-prefix)))
	(ccs1 (if mp-chunk-content-suffix
		  (concat
		   "^" (regexp-quote mp-chunk-content-suffix))
		nil))
	(name1 "")
	(format1 "")
	(beg1 0)
	(end1 0)
	(content1 ""))
    (setq mp-chunks '())
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp cnp1 nil t)
	;; format is on the line below the name in .org files
	(setq end1 (if (equalp "org" mp-fne)
		       2
		     1))
	(forward-line 0)
	;; name and format
	(if (save-excursion
	      (search-forward-regexp
	       cfp1 (line-end-position end1) t))
	    (setq beg1 (save-excursion
			 (search-forward-regexp cnp1 nil t))
		  end1 (search-forward-regexp
			cfp1 (line-end-position end1) t)
		  ;; end1 (+ 1 (search-backward-regexp cfp1))
		  name1 (string-trim
			 (buffer-substring-no-properties
			  beg1
			  (- end1
			     (- (length mp-chunk-format-prefix) 1))))
		  format1 (string-trim
			   (buffer-substring-no-properties
			    end1
			    (- (line-end-position)
			       (length mp-chunk-name-suffix)))))
	  (setq name1 (buffer-substring-no-properties
		       (search-forward-regexp cnp1 nil t)
		       (- (line-end-position)
			  (length mp-chunk-name-suffix)))
		format1 ""))
	;; content
	(beginning-of-line 2)
	(setq content1
	      (if (looking-at cnp1)
		  ""
		(progn
		  (setq beg1 (point)
			end1 (cond ((and ccs1
					 (not (equalp "R" mp-fne)))
				    (- (search-forward-regexp 
					ccs1 nil t) 
 				       (length mp-chunk-content-suffix)))
				   ((save-excursion
				      (search-forward-regexp 
				       cnp1 nil t))
				    (- (save-excursion
					 (search-forward-regexp 
					  cnp1 nil t))
				       (length cnp1)))
				   (t (point-max))))
		  (buffer-substring-no-properties
		   beg1 end1))))
	(push (list name1 format1 content1) mp-chunks))))
  (setq mp-chunks (nreverse mp-chunks))
  (unless (mp-chunk-name-duplicates (mapcar #'car mp-chunks))
    mp-chunks))

(defun mp-chunk-name-duplicates (LIST)
    "Return nil when LIST has no duplicates.
Otherise, call `mp-exit'."
    (let ((unique1 (remove-duplicates LIST :test #'string-equal)))
      (if (eq LIST unique1)
	  nil
	(mp-exit "Chunk names are not all unique"))))

(defun mp-update-entwiner-from-R ()
  "Make a new entwiner from the corresponding .R file.
This is done using `mp-chunks'."
  (mp-file-display (concat mp-fnb ".R"))
  (mp-chunks-read)
  (delete-file
   (concat mp-fnb "." (car mp-entwiner)) t)
  (mp-entwiner-new))

(defun mp-update-R-from-entwiner ()
    "Make a new .R file from the corresponding entwiner file.
This is done using `mp-chunks'."
  (mp-file-display (concat mp-fnb "." (car mp-entwiner)))
  (mp-chunks-read)
  (delete-file
   (concat mp-fnb ".R") t)
  (mp-R-new))

(defun mp-mp ()
  "'Entwine' the files in `mp-dd' to produce a .pdf.
Calls `save-buffer'.
The value of `mp-dd' is set with `mp-set-mp-dd'.
Then call `mp-run'."
  (interactive)
  (save-buffer)
  (mp-set-mp-dd)
  (mp-initialize)
  (if mp-fn
      (setq mp-fn (call-interactively #'mp-file-confirm)
	    mp-fnb (file-name-base mp-fn)
	    mp-fne (file-name-extension mp-fn)) 
    (setq mp-fnb (file-name-base (directory-file-name mp-dd))
	  mp-fne "R"))
  (mp-run mp-fne))

(defcustom mp-clean nil
  "Non-nil means call the function `mp-clean' during `mp-exit'."
  :type '(radio (const
		 :doc "Do not clean files on exit"
		 :value nil)
		(const
		 :doc "Clean files on exit"
		 :value t))
  :group 'mp
  :group 'mp-common)

(defun mp-clean ()
  "Delete files in `mp-dd' which have `mp-clean-extensions'.
Also delete files/ subdirectories given in `mp-clean-others'."
  (interactive)
  (when (equalp "" mp-dd)
    (mp-set-mp-dd))
  (let ((bl1 (buffer-list))
	(revert1 '("*Buffer-list*" "*Ibuffer*"
		   (file-name-nondirectory
		    (directory-file-name mp-dd))))
	(b1 (current-buffer)))
  (dolist (x bl1)
    (when (and
	    (equalp mp-fnb (file-name-base (buffer-name x)))
	    (member
	     (file-name-extension (buffer-name x))
	     mp-clean-extensions))
      (kill-buffer x)
      (delete-window)))
  (dolist (x mp-clean-extensions)
    (mapc #'delete-file
	  (file-name-all-completions
	   (concat mp-fnb "." x)
	   mp-dd)))
  (dolist (x mp-clean-others)
    (when (member x (directory-files mp-dd))
      (if (file-directory-p x)
	  (delete-directory x t)
	(delete-file x))))
  (setq bl1 (mapcar #'buffer-name (buffer-list)))
  (dolist (x revert1)
     (when (member x bl1)
       (with-current-buffer x
	 (revert-buffer))))))

(defcustom mp-clean-extensions
  '("Rnw" "org" "Rmd" "aux" "bbl" "blg" "fdb_latexmk"
    "fls" "log" "out" "toc")
  "File extensions to be deleted from the `mp-dd'.
The corresponding buffers are also killed, if applicable.
This value is used by `mp-clean'.
See also the functions `delete-file', `file-name-extension'."
  :type '(repeat (string :tag "file extension"))
  :group 'mp)

(defcustom mp-clean-others
  '("knit.R" ".latexmkrc" "auto" ".Rhistory")
  "Files (and/or directories) to be deleted from `mp-dd'.
This value is used by `mp-clean'."
  :type '(repeat (string))
  :group 'mp)

(defun mp-file-confirm (&optional DEFAULT)
  "Confirm `mp-fn' as the one to start the mp process. 
When DEFAULT is true, this is the most recently active file.
When called interactively, user has the option to enter a file name
\(base, extension, or both).
If none is given, defaults to the current file  or
the most recently active file in the `default-directory'.
Calls `mp-find-file-name'."
  (interactive
   (list (y-or-n-p-with-timeout
	  (concat "Use default (" mp-fn ") ? ")
	  mp-timeout t)))
  (if DEFAULT
      mp-fn
    (call-interactively #'mp-file-choose)))

(defun mp-file-choose (&optional FILE)
  "Confirm FILE as the one to start the mp process.
By default the most recently active file.
When called interactively, user has the option to enter a file name"
  (interactive)
  (let ((f1 mp-fn))
    (setq f1
	  (prin1
	   ;; (completing-read PROMPT COLLECTION &optional
	   ;; PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
	   ;; INHERIT-INPUT-METHOD KEYMAP)
	   (completing-read "Filename: " mp-df nil t nil nil mp-fn)))
    (message (concat
	      "Filename "
	      (if (equalp f1 mp-fn)
		  "unchanged : "
		"changed to : ")
	      f1))
    (setq mp-fn f1)))

(defvar mp-minor-mode-map nil
  ;; (let ((map1 (make-sparse-keymap)))
  ;;   (if (fboundp 'read-kbd-macro)
  ;; 	(define-key map1 (read-kbd-macro "C-M-|") 'mp-mp)
  ;;     (define-key map1 [(control meta ?|)] 'mp-mp))
  ;;   map1)
  "The keymap for the 'mp' minor mode.")

(define-minor-mode mp-minor-mode
  "Define the 'mp' minor mode (mp = make pdf).
'Mp' is a global minor mode.
It's LIGHTER (displayed on the mode line) is ' mp '."
  t " mp " mp-minor-mode-map
  (message "mp minor mode toggled")
  :group 'mp
  :global t
  :version 0.2)

(defun mp-customize-common ()
  "View/set common options for the 'mp' package.
\(Those the user is most likely to wish to change).
Opens help window.
Cutomization buffers are overlaid in this window.

These windows are overlaid.
Use 'q' to kill each of these new buffers
\(i.e. run `quit-window' then `bury-buffer').

See also the group `mp-common' and the function `mp-customize-all'."
  (interactive)
  (dolist (x (get 'mp-common 'custom-group))
    (cond ((eq (cadr x) 'custom-face)
	   (customize-face (car x)))
	  ((eq (cadr x) 'custom-variable)
	   (customize-option (car x)))
	  ((eq (cadr x) 'custom-group)
	   (customize-group (car x))))))

(defgroup mp-skeleton-string nil
  "These are strings used by a corresponding `define-skeleton'.
See also `skeleton-insert'.
This group is part of `mp'."
  :group 'mp)

(defun mp-customize-skeleton-string ()
  "View/set skeleton strings for the 'mp' package.
Opens help window.
Cutomization buffers are overlaid in this window.

These windows are overlaid.
Use 'q' to kill each of these new buffers
\(which calls `quit-window' then `bury-buffer').

See also
- the function `mp-customize-common'
- the output from the command '(customize-group 'mp)'"
  (interactive)
  (dolist (x (get 'mp-skeleton-string 'custom-group))
    (cond ((eq (cadr x) 'custom-variable)
	   (customize-option (car x)))
	  ((eq (cadr x) 'custom-group)
	   (customize-group (car x))))))

(defun mp-customize-all ()
  "View/set all options for the 'mp' package.
Opens help window.
Cutomization buffers are overlaid in this window.

These windows are overlaid.
Use 'q' to kill each of these new buffers
\(which calls run `quit-window' then `bury-buffer').

See also
- the function `mp-customize-common'
- the output from the command '(customize-group 'mp)'"
  (interactive)
  (dolist (x (get 'mp 'custom-group))
    (cond ((eq (cadr x) 'custom-face)
	   (customize-face (car x)))
	  ((eq (cadr x) 'custom-variable)
	   (customize-option (car x)))
	  ((eq (cadr x) 'custom-group)
	   (customize-group (car x))))))

(defun mp-chunk-new ()
  "Insert a new chunk.
Calls `mp-chunk-set-adfixes' to set these correctly."
  (interactive)
  (mp-chunk-set-adfixes)
  (let ((cf1 (mp-chunk-format)))
    (insert mp-chunk-name-prefix)
    (save-excursion
      (insert
       (concat
	mp-chunk-format-prefix
	cf1
	mp-chunk-name-suffix))
      (unless (looking-at "\n")
	(insert "\n"))
      (when mp-chunk-content-suffix
	(insert
	 (concat "\n" mp-chunk-content-suffix))))))
  
(define-skeleton mp-tex-skeleton
  "Generate a .tex file."
  nil
  '(mp-doc-class-tex-skeleton)
   '(mp-preabmle-tex-skeleton)
   \n
   '(mp-tex-title-skeleton)
   '(when mp-bib
     (mp-insert-bib))
   \n
    '(goto-char (buffer-end 1))
    '(mp-tex-end-skeleton))

(defun mp-tex-new ()
  "Make an .tex file.
The format depends, in part, in the value of `mp-entwiner'.
Calls `mp-set-mp-dd' and `mp-tex-skeleton'.
This is a 'standalone' function i.e. not called in the
usual sequence following `mp-mp'."
  (interactive)
  (setq mp-fnb ""
	mp-fne (car mp-entwiner))
  (mp-set-mp-dd)
  (mp-file-display (concat mp-fnb ".tex"))
  (erase-buffer)
  (mp-tex-skeleton)
  (when mp-bib
    (mp-bib))
  (save-buffer))

(provide 'mp)

;;; mp.el ends here
