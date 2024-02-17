## R CMD check results

0 errors | 0 warnings | 0 notes

## This is a re-submission of the 'tinycodet' package, updating it to version 0.4.1.

Change log:

* Added an explanation in the string overview help page, regarding the usage of vector recycling.
* Provided additional clarification in the help file for the `%row%` and `%col~%` operators.
* Simplified the internal code of the decimal (in)equality testing operators.
* Added a few more tests.
* Moved `is_wholenumber()` to the decimal truth testing section.
* **Argument change:** Added the `rt` argument to `strfind()<-`, and moved the `i` and `rt` arguments more to the end of the functions. The `type` argument in `strcut_brk()` can now also directly accept a list produced by `stringi::stri_opts_brkiter()`.
