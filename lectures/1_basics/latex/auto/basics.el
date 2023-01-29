(TeX-add-style-hook
 "basics"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("apacite" "natbibapa") ("newtxmath" "libertine")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "graphicx"
    "apacite"
    "newtxmath"
    "fancybox"
    "booktabs"
    "eurosym"
    "caption")
   (TeX-add-symbols
    '("rmsc" 1))
   (LaTeX-add-bibliographies
    "../../../bibtex/winter_school_refs.bib"))
 :latex)

