## R CMD check results

0 errors | 0 warnings | 0 notes

## This is an Update of 'tinycodet'.
* The current version present in CRAN is 0.1.0.6. This submission updates it to version 0.2. What follows is the change log.
* (Hopefully!) fixed the latex issue in the pdf file generation for users using R version < 4.2.0 with the Mac OS.
* Removed or changed "a few functions" in the title, description, introduction help page, and website, as 'tinycodet' actually turned out to be somewhat larger than anticipated.
* Changed the `@name` and `@rdname` parameters of the tinycodet overview help files, such that they get on the top of the pdf file.
* Removed the sentence "Note that only the recommended R-packages actually installed in your system are taken into consideration" in the `pkgs` help file, as this is no longer applicable (the list of recommended R packages is hard coded).
* Expanded the 'tinycodet_import' help page with more info.

## General comments (same as in previous version; nothing new or something)
* Thank you again for your patience, and for taking the time to review/quality-check R packages! I appreciate your hard, voluntary work.
* I have created over 800 tests, some of them using loops (to reduce the amount of coding/typing), to properly test this package (so about a dozen tests per function on average). No errors found.
* I personally use Windows 11 as my operating system. But I have also used the 'rhub' R-package to test & check the package on the Mac OS, and Linux. No errors found.
* There are no references describing my package.
* I have checked the spelling (EN-GB) and the URLs. No problems.
* Some of the code in some of the functions have been inspired by Stack Overflow comments. In these cases, references have been given in the help page of those functions.
