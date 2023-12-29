## R CMD check results

0 errors | 0 warnings | 0 notes

## Happy New Year!

## This is a re-submission of the 'tinycodet' package, updating it to version 0.3.9.

Change log:

* Clarified in the documentation that `%col~%` and `%row~%` strip attributes (this was not really made clear in the documentation before).
* Clarified the usage of `merge = FALSE` for pattern `charclass`,
in the documentation of the `stri_locate_ith()` function.
* Clarified in the documentation that the locked objects are not protected from modification by reference.
* Clarified in the documentation of 'stringi' pattern-searching infix operators that the `p` argument can also be a character vector of length 1.
* Removed the redundant `stringi::` call in the example code for `import_LL()`.
* The help file for the `s_pattern` functions is now actually titled "s_pattern".
* Cleaned up the internal code here and there.
* The `help.import()` function now gives an error if neither `topic/package` nor `i/alias` is supplied, instead of just silently doing nothing.
* The messages returned by `import_as()` when aliasing packages is now slightly less verbose: removed the line "Methods work like normally", and replaced the line "Importing packages ..." with "Importing packages and registering methods...".
* The `strcut_brk()` function now includes the `tolist` argument to return a list. Moreover, the `n` argument may now also be specified (`n = -1L` by default).
* Improved safety against malformed condition in the `transformed_if()` function.
* **Optimization:** Managed to optimise `stri_locate_ith()` even further. Optimised the 'subset_if' operators. Optimised the `%sget%`, `%strim%`, `%n&%`, and `%=strtype%` operators.
* **Argument API change:** The `s_pattern` functions now wrap additional arguments in a list for the user, in case of using vector arguments, preventing potential confusion.
* **Tests:** Improved the tests for the `s_pattern` operators. Added tests for `stri_locate_ith()` when using argument `merge = FALSE`. Added more error check tests for `help.import()`. Added tests for unequal vector sizes for the logic operators. Added tests for empty condition subsets for `transform_if()`.
* **Bug fix:** There was a small bug where `s_coll()` worked properly when assigned to an object (as usual), but not when called directly (like nested inside a function). This is now fixed.

