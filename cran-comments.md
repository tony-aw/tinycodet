## R CMD check results

0 errors | 0 warnings | 0 notes


## This is a (new) re-submission
* @Benjamin Altmann: Thank you for checking my package and your insight. I am impressed how fast you went through my R-package. I have implemented your requests as best I could (see the next few points).
* Replaced usage of `installed.packages()` in the `%installed in%` operator with `find.package()`. Also added the sentence "As pkgs %installed in% lib.loc does not even load a package, the user can safely use it without fearing any unwanted side-effects." to the help page of said operator.
* Replaced usage of `installed.packages()` with a manually specified character vector in the R scripts/functions "internal_functions.R", "import_misc.R", "import_inops.R", "x.import.R".
* Replaced "\dontrun" with "\donttest" in help page "tinycodet_import", and set condition for examples.
* Re-implemented the tests with fake packages, but this time preventing false positives more elegantly using a tip from Duncan Murdoch (kudos to him!). I tested this with different OS, including https://win-builder.r-project.org/, and it works. Hopefully this new solution also works on CRAN's servers. Let me know if there are problems.


## General comments
* Thank you again for your patience, and for taking the time to review/quality-check R packages! I appreciate your hard, voluntary work.
* I have created over 800 tests, some of them using loops (to reduce the amount of coding/typing), to properly test this package (so about a dozen tests per function on average). No errors found. Some tests done only on GitHub (see comment above); again, no errors found.
* I personally use Windows 11 as my operating system. But I have also used the 'rhub' R-package to test & check the package on the Mac OS, and Linux. No errors found.
* There are no references describing my package.
* I have checked the spelling (EN-GB) and the URLs. No problems.
* Some of the code in some of the functions have been inspired by Stack Overflow comments. In these cases, references have been given in the help page of those functions.
