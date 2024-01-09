## R CMD check results

0 errors | 0 warnings | 0 notes

## Happy New Year!

## This is a re-submission of the 'tinycodet' package, updating it to version 0.4.0.

Change log:

* Clarified in the documentation that `%col~%` and `%row~%` strip attributes.
* Clarified the usage of `merge = FALSE` for pattern `charclass`,
in the documentation of the `stri_locate_ith()` function.
* Clarified in the documentation that locked objects are not protected from modification by reference.
* Clarified in the documentation of 'stringi' pattern searching infix operators that the `p` argument can also be a character vector of length 1.
* Clarified in the `stri_locate_ith()` documentation that one should not pass the `capture_groups` argument; also clarified how to capture the `ith` group using `stri_locate_ith`. And clarified that `stri_locate_ith()` does not support long vectors.
* Normalized the links in the "See Also" sections in the documentation.
* Renamed the "str_truth" page to "str_search", as that makes a little more sense.
* Removed the redundant `stringi::` piece in the example code for `import_LL()`.
* The help file for the `s_pattern` functions is now actually titled "s_pattern".
* The `help.import()` function now gives an error if neither `topic/package` nor `i/alias` is supplied, instead of just silently doing nothing.
* The messages returned by `import_as()` when aliasing packages is now slightly less verbose: removed the line "Methods work like normally", and replaced the line "Importing packages ..." with "Importing packages and registering methods...".
* The `strcut_brk()` function now includes the `tolist` argument to return a list. Moreover, the `n` argument may now also be specified (`n = -1L` by default).
* Improved safety against malformed conditions in the `transform_if()` function.
* `stri_locate_ith()` now gives a warning when an empty string or pattern is given.
* **Internal Re-write:** `stri_locate_ith()` has now been partially re-written, and now includes 'C++' code, making it a bit faster. Also cleaned up the internal code for some other functions here and there.
* **Added dependency:** 'Rcpp' is now added as an dependency, due to the aforementioned partial re-write in 'C++'.
* **New Features:** Added `%ss%` to the collection of string arithmetic operators, and added the `strfind()<-` method to the string search operators.
One can now supply the `at` argument in the list of the right-hand side for the `%s{}%` operator. Supplying `at = "start"` will check if the pattern appears at the start of a string. Supplying `at = "end"` will check if the pattern appears at the end of a string.
* **Optimization:** Managed to optimise `stri_locate_ith()` even further. Optimised the 'subset_if' operators a bit more. Also optimised the `%n&%`, and `%=strtype%` operators a bit more.
* **Argument change:** The `s_pattern` functions now wrap additional arguments in a list for the user, in case of using vector arguments, preventing potential confusion.
* **Argument change:** The `type` argument in `stri_locate_ith_boundaries()` is now not a mandatory argument; one can now also supply it through `stri_opts_brikiter()`, to keep it more consistent with the rest of 'stringi'.
* **Tests:** Improved the tests for the `s_pattern` operators. Added tests for `stri_locate_ith()` when using argument `merge = FALSE`. Added more error check tests for `help.import()`. Added tests for unequal vector sizes for the logic operators. Added tests for empty condition subsets for `transform_if()`. Added tests for the new functionalities introduced in this version. Added tests for malformed 'stringi' pattern searches in all relevant functions.
* **Bug fix:** There was a small bug where `s_coll()` worked properly when assigned to an object (as usual), but not when called directly (like nested inside a function). This is now fixed.

