# tinycodet 0.2.3
* Minor optimizations for the `import_*` functions and for `transform_if()`.
* Expanded the import system documentation. Also tweaked the documentation of `import_as()` to be make it slightly clearer.
* `import_as()` now does not allow more than 10 packages to be loaded under a single alias, to prevent abusive usage of `import_as()`.
* The import system now has a few more tiny safety checks, including checks for widely known meta-packages (such as "fastverse").
* Fixed a tiny bug in `pkg_get_deps()` regarding getting the recommended R-packages.
* Changed the error message for wrong extension specifications: replaced "actual reverse-dependencies" into "actual extensions".
* Added more tests.


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
