## R CMD check results

0 errors | 0 warnings | 0 notes

## This is a re-submission of the 'tinycodet' package, updating it to version 0.5.3. As usual, I thank the CRAN team for all their hard work.


Change log:

* **Behaviour change:** the atomic typecasting functions now preserve names, dimensions, and dimnames (instead of all attributes), to be more in line with most of base 'R'.
* **Behaviour change:** `pkgs %installed in% lib.loc` will now return `NA` for "packages" that are part of core 'R' (i.e. 'base', 'stats', etc.).
* **Behaviour change:** All decimal truth testing operators now always give `NA` when `Inf` is compared with `Inf`, or when `-Inf` is compared to `-Inf`.
* Streamlined the internal code of the decimal truth testing operators.
* Added `%s><%` and `%s<>%` as aliases for `%sget%` and `%strim%`, respectively.
* Removed some superfluous text in the import system documentation.
* Re-written one 'C++' script to pure 'C' code.
* Some of the internal 'C' code now support long vectors, when appropriate.
* Small speed improvement in some of the internal code.
* Added new fake packages to the special tests, to perform more thorough tests on the `pversion_` - functions.
* Added even more tests.