## R CMD check results

0 errors | 0 warnings | 0 notes

## This is a re-submission of the 'tinycodet' package, updating it to version 0.3.0. Sorry for the submission after slightly less than a month (instead of > 1 months), but I found a bug (despite my many unit tests) that really needed to be fixed quickly. I beg your understanding.

Change log:

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

