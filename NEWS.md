# tinycodet 0.4.1
* Added an explanation in the string overview help page, regarding the usage of vector recycling.
* Provided additional clarification in the help file for the `%row%` and `%col~%` operators.
* Simplified the internal code of the decimal (in)equality testing operators.
* Added a few more tests.
* Moved `is_wholenumber()` to the decimal truth testing section.
* **Argument change:** Added the `rt` argument to `strfind()<-`, and moved the `i` and `rt` arguments more to the end of the functions. The `type` argument in `strcut_brk()` can now also directly accept a list produced by `stringi::stri_opts_brkiter()`.


# tinycodet 0.4.0
* Clarified in the documentation that `%col~%` and `%row~%` strip attributes.
* Clarified the usage of `merge = FALSE` for pattern `charclass`,
in the documentation of the `stri_locate_ith()` function.
* Clarified in the documentation that locked objects are not protected from modification by reference.
* Clarified in the documentation of 'stringi' pattern searching infix operators that the `p` argument can also be a character vector of length 1.
* Clarified in the `stri_locate_ith()` documentation that one should not pass the `capture_groups` argument; also clarified how to capture the `ith` group using `stri_locate_ith()`. And clarified that `stri_locate_ith()` does not support long vectors.
* Normalized the links in the "See Also" sections in the documentation.
* Renamed the "str_truth" page to "str_search", as that makes a little more sense.
* Removed the redundant `stringi::` piece in the example code for `import_LL()`.
* The help file for the `s_pattern` functions is now actually titled "s_pattern".
* The messages returned by `import_as()` when aliasing packages is now slightly less verbose: removed the line "Methods work like normally", and replaced the line "Importing packages ..." with "Importing packages and registering methods...".
* The `strcut_brk()` function now includes the `tolist` argument to return a list. Moreover, the `n` argument may now also be specified (`n = -1L` by default).
* **More safety checks:** Improved safety against malformed conditions in the `transform_if()` function. `stri_locate_ith()` now gives a warning when an empty string or pattern is given. The `help.import()` function now gives an error if neither `topic/package` nor `i/alias` is supplied, instead of just silently doing nothing. The `%sget%` and `%strim%` operators now give an explicit error message if the arguments do not have proper lengths.
* **Internal Re-write:** `stri_locate_ith()` has now been partially re-written, and now includes 'C++' code, making it a bit faster. Also cleaned up the internal code for some other functions here and there.
* **Added dependency:** 'Rcpp' is now added as an dependency, due to the aforementioned partial re-write in 'C++'.
* **New Features:** Added `%ss%` to the collection of string arithmetic operators, and added the `strfind()<-` method to the string search operators.
One can now supply the `at` argument in the list of the right-hand side for the `%s{}%` operator. Supplying `at = "start"` will check if the pattern appears at the start of a string. Supplying `at = "end"` will check if the pattern appears at the end of a string.
* **Optimization:** Managed to optimise `stri_locate_ith()` even further. Optimised the 'subset_if' operators a bit more. Also optimised the `%n&%`, `%sget%`, `%strim%`, and `%=strtype%` operators a bit more.
* **Argument change:** The `s_pattern` functions now wrap additional arguments in a list for the user, in case of using vector arguments, preventing potential confusion.
* **Argument change:** The `type` argument in `stri_locate_ith_boundaries()` is now not a mandatory argument; one can now also supply it through `stri_opts_brikiter()`, to keep it more consistent with the rest of 'stringi'.
* **Tests:** Improved the tests for the `s_pattern` operators. Added tests for `stri_locate_ith()` when using argument `merge = FALSE`. Added more error check tests for `help.import()`. Added tests for unequal vector sizes for the logic operators. Added tests for empty condition subsets for `transform_if()`. Added tests for the new functionalities introduced in this version. Added tests for malformed 'stringi' pattern searches in all relevant functions.
* **Bug fix:** There was a small bug where `s_coll()` worked properly when assigned to an object (as usual), but not when called directly (like nested inside a function). This is now fixed.


# tinycodet 0.3.0
* Expanded the import system documentation. Also tweaked the documentation of `import_as()` to be make it slightly clearer.
* Expanded the documentation of the `decimal_truth` operators.
* `import_as()` now does not allow more than 10 packages to be loaded under a single alias, to prevent abusive usage of `import_as()`.
* The import system now has a few more tiny safety checks, including checks for widely known meta-packages (such as "fastverse").
* The `pkg_get_deps()` function now also has the `shared_tidy` argument to ignore the shared 'tidyverse' libraries ('rlang', 'lifecycle', 'cli', 'glue', and 'withr'). Also changed the default values of the `recom`, and `rstudioapi` arguments.
* Added the `pkg_get_deps_minimal()` function. Also changed the default arguments of `pkg_get_deps()`.
* Changed the error message for wrong extension specifications: replaced "actual reverse-dependencies" into "actual extensions".
* Changed the usage of the word "load" to "import" in the import system documentation, to avoid confusion with the concept of loading a package.
* Added the `pversion_` functions to help in checking package versions.
* Removed the "versions" attribute from alias objects, in favour of the aforementioned `pversion_` functions.
* **Argument name change:** in the `import_as()` function, changed argument name `loadorder` to `import_order`, to avoid confusion with the concept of loading a package.
* **Tests:** Added more tests.
* **Optimization:** Minor optimizations for the `import_*` functions and for `transform_if()`.
* **Bug fix:** Fixed a bug in `pkg_get_deps()` where it didn't properly ignore all the recommended R-packages when `recom = FALSE`.


# tinycodet 0.2.2
* Small tweak to the pdf manual.
* Replaced the incorrect uses of the word "class" with "type" in the atomic type casting help file.
* Improved the performance of `stri_join_mat()`, and produced better examples in both help file and website.
* Fixed incorrect regex usage in one of the examples.


# tinycodet 0.2
* Removed or changed "a few functions" in the title, description, introduction help page, and website, as 'tinycodet' actually turned out to be somewhat larger than anticipated.
* Fixed the latex issue in the pdf file generation for users using R version < 4.2.0 with the Mac OS.
* Changed the `@name` and `@rdname` parameters of the tinycodet overview help files, such that they get on the top of the pdf file.
* Removed the sentence "Note that only the recommended R-packages actually installed in your system are taken into consideration" in the `pkgs` help file, as this is no longer applicable (the list of recommended R packages is hard coded).
* Expanded the 'tinycodet_import' help page with more info.


# tinycodet 0.1.0.6
* Replaced usage of `installed.packages()` in the `%installed in%` operator with `find.package()`. Also added the sentence "As pkgs %installed in% lib.loc does not even load a package, the user can safely use it without fearing any unwanted side-effects." to the help page.
* Replaced usage of `installed.packages()` with a manually specified character vector in the R scripts/functions "internal_functions.R", "import_misc.R", "import_inops.R", "x.import.R".
* Replaced "\dontrun" with "\donttest" in help page "tinycodet_import", and set condition for examples.
* Re-implemented the fake packages, but this time preventing false positives using a tip from Duncan Murdoch.


# tinycodet 0.1.0.5
* The fake packages will now only be tested on my GitHub page, and not in the package folder itself, to prevent false positives from CRAN checks.


# tinycodet 0.1.0.4
* Removed the last sentence in the Description of the DESCRIPTION file, which contained the word "vectorized", because both "vectorised" AND "vectorized" cause CRAN checks to complain.


# tinycodet 0.1.0.3
* Changed all the spellings of the word "vectorised", to "vectorized", despite the clear Language field.


# tinycodet 0.1.0.2
* Corrected the 'Authors@R' field in the DESCRIPTION file.
* Added 'Language: en-gb' to the DESCRIPTION file.
* Removed the "Maintainer" field, as it can be auto-generated from 'Authors@R'.


# tinycodet 0.1.0.1
* Corrected the LICENSE file.


# tinycodet 0.1
* Initial release.

