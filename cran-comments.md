## R CMD check results

0 errors | 0 warnings | 0 notes

## This is a re-submission of the 'tinycodet' package, updating it to version 0.5.0. Sorry for the quick up-date, but I found some bugs and some design mistakes that needed to be fixed. As always, I thank the CRAN team for all their hard, voluntary work.


Change log:

* **Feature Improvement:** `help.import()` now directly evaluates the arguments under `help()` if both arguments `i` and `alias` are missing.
* **Feature Improvement:** re_exports in `import_as()` can now include functions from core R if necessary, except 'base'.
* **Performance Improvement:** Replaced some of the internal code in the import system with 'C++' code via 'Rcpp' for some performance improvement.
* **Bug fix:** Fixed a (small) bug, where `help.import()` sometimes gave an unnecessary error, when searching topics of un-exported objects or non-functions via an alias object instead of searching functions directly.
* **Bug fix:** Fixed a (small) bug where class names were sometimes inconsistently assigned to functions and infix operators exposed by the 'tinycodet' import system.
* **Removed Feature:** Removed the `form()` function, as its use-case is a bit too rare to justify having a whole function for it (with all the tests and maintenance that come with it). I rarely remove functions; this is an exception.
* Fixed some mistakes in the documentation.
* Added more tests.
* `help.import()` can now also be called without any arguments, just like the original `help()` function.
* Moved the atomic type casting functions to the "DRY" category.
* Cleaned up the internal code of the import system a bit.
* Added the current year to the LICENSE file.

