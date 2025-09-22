## R CMD check results

0 errors | 0 warnings | 0 notes

## This is a re-submission of the 'tinycodet' package, updating it to version 0.5.8.
As usual, I thank the CRAN team for all their hard work.


Change log:

* **Superseded:** The `as_*` functions are superseded by the functions of the same name in the 'broadcast' package, and will be removed from 'tinycodet' in a later version.
* **Removed:** The infix operators from the DRY category were removed.
* **Behaviour change:** The matrix operators now only accept a numeric (integer or double) matrix on the right hand side with the same dimensions as the left hand side.
* **Performance Improvement:** The logical operators are now slightly faster, as they now use R's own NA checks
* Moved the matrix operators to the DRY category.
* Simplified the matrix operators help page.