## R CMD check results

0 errors | 0 warnings | 0 notes

## This is a re-submission of the 'tinycodet' package, updating it to version 0.5.6.
As usual, I thank the CRAN team for all their hard work.


Change log:

* **Behaviour change:** The import functions no longer import `.Primitive` or `.Internal` functions, to comply with CRAN.
* **Behaviour change:** `import_as()` now allows hidden (i.e. dot-prefixed) names for `alias` objects.
