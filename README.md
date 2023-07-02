
- [1 tinyoperators](#1-tinyoperators)
  - [1.1 Description](#11-description)
  - [1.2 Changelog and status](#12-changelog-and-status)
  - [1.3 Installation](#13-installation)
  - [1.4 Overview](#14-overview)
- [2 Additional logic operators](#2-additional-logic-operators)
- [3 Safer float truth testing
  operators](#3-safer-float-truth-testing-operators)
- [4 Matrix re-ordering operators](#4-matrix-re-ordering-operators)
- [5 String functions](#5-string-functions)
  - [5.1 Matrix joining](#51-matrix-joining)
  - [5.2 stri_locate_ith](#52-stri_locate_ith)
  - [5.3 Substr - functions](#53-substr---functions)
- [6 String infix operators](#6-string-infix-operators)
  - [6.1 String subsetting operators](#61-string-subsetting-operators)
  - [6.2 String arithmetic](#62-string-arithmetic)
  - [6.3 Specifying Pattern search attributes in string infix
    operators](#63-specifying-pattern-search-attributes-in-string-infix-operators)
- [7 “DRY” - operators](#7-dry---operators)
  - [7.1 The transform_if function, and related
    operators](#71-the-transform_if-function-and-related-operators)
  - [7.2 Generalized in-place (mathematical)
    modifier](#72-generalized-in-place-mathematical-modifier)
  - [7.3 In-place modifying string arithmetic and
    sub-setting](#73-in-place-modifying-string-arithmetic-and-sub-setting)
- [8 Import management](#8-import-management)
  - [8.1 Introduction](#81-introduction)
  - [8.2 import_as](#82-import_as)
  - [8.3 import_inops](#83-import_inops)
  - [8.4 import_data](#84-import_data)
  - [8.5 lib.loc](#85-libloc)
  - [8.6 On pronouns and aliases](#86-on-pronouns-and-aliases)
  - [8.7 Sourcing modules](#87-sourcing-modules)
  - [8.8 Miscellaneous functions and
    operators](#88-miscellaneous-functions-and-operators)
  - [8.9 An example](#89-an-example)
- [9 Multi-threading](#9-multi-threading)
  - [9.1 Substr-functions](#91-substr-functions)
- [10 Tinyverse solutions without external R
  packages](#10-tinyverse-solutions-without-external-r-packages)
- [11 Compatibility with other R
  packages](#11-compatibility-with-other-r-packages)
- [12 Recommended R packages](#12-recommended-r-packages)
- [13 Conclusion](#13-conclusion)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# 1 tinyoperators

<!-- badges: start -->

[![R build
status](https://github.com/tony-aw/tinyoperators/workflows/R-CMD-check/badge.svg)](https://github.com/tony-aw/tinyoperators/actions)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/ORCID-0000--0001--9498--8379-green.svg)](https://orcid.org/0000-0001-9498-8379)
<!-- badges: end -->

![](man/figures/tinyoperators.svg)  

## 1.1 Description

The `tinyoperators` R-package adds some infix operators, and a few
functions. It primarily focuses on 4 things:

1)  Float truth testing.
2)  Reducing repetitive code.
3)  Extending the string manipulation capabilities of the `stringi` R
    package.
4)  A new package and module import system, that combines the benefits
    of aliasing a package with the benefits of attaching a package.

The `tinyoperators` R-package has only one dependency, namely `stringi`,
though it does allows multi-threading of some of the string-related
functions (when appropriate) via the suggested `stringfish` R-package.
Most functions in this R-package are fully vectorized and have been
optimized for optimal speed and performance.

 

## 1.2 Changelog and status

WARNING: This package is still very much experimental. Function names,
argument names, and so on may change dramatically. Use it for testing
only, until it’s stable.

CHANGELOG (EXPERIMENTAL VERSIONS):

- 8 March 2023: `stringi` is now a dependency. Completely re-written the
  ReadMe file, Description, and documentation.
- 9 March 2023: added the “which”-operators.
- 10 March 2023: `s_strapply()` now uses `stringi`, and uses `apply()`
  instead of `sapply()`. Renamed the which operators `%[sp]%` and
  `%[!sp]%` to `%[grep]%` and `%[!grep]%` to make their meaning more
  obvious. Added this Change log to the ReadMe file.
- 11 March 2023: replaced the “which”-operators with the
  `transform_if()` function and the subset_if operators.
- 13 March 2023: changed the name and argument convention of many of the
  string related functions to be more consistent. Changed the return of
  non-matches in the substr\_-functions.
- 14 March 2023: changed the utility function to the `%m import <-%`
  operator. Fixed some linguistic mistakes in the documentation. Added a
  full documentation pdf. Fixed bugs in all `substr_`-functions. Added
  `codefactor` badge. Fixed some errors in the Description file. Created
  and added the pdf manual. Fixed some minor errors in this Read-Me
  markdown file.
- 17 March 2023: added infix operators for custom row- and column-wise
  sorting of matrices. Slightly optimized the `substr_arrange()`
  function, and added the `opts_collator` argument to it. Re-ordered the
  sections of this Read-Me file. Adjusted the documentation the reflect
  the new changes.
- 19 March 2023: removed the `s_strapply()` function in favor of the now
  newly added `stri_join_mat()` function and its aliases. Renamed the
  matrix re-order operators to `%row~%` and `%col~%`. Added a random
  order option to the `substr_arrange()` function. Adjusted the
  description, documentation, and this Read-Me file to reflect the new
  changes, and also fixed some spelling errors. Hopefully this will be
  one of the last major changes I have to make to this R package.
- 25 March 2023: Now tests using `testthat` added for the
  `R CMD checks`. The `x %n&% y` operator now returns `NA` if either `x`
  or `y` or `NA`. The `%s+%` and `%s*%` operators now use `stringi`’s
  equivalent operators, for more consistency; their in-place modifiers
  are affected as well. Corrected some small spelling- and grammatical
  errors in the documentation and the Read-Me.
- 28 March 2023: Small textual changes to the Read-Me file.
- 4 April 2023: Added the `x % f{}% bnd` and `x % f!{}% bnd` operators.
  Adjusted the `%?=%` operator: now `NA %?=% Inf` and similar equality
  checks will also return `TRUE`. Added more tests with `testthat`.
  Adjusted the Read-Me file and documentations in accordance with these
  changes.
- 8 April 2023: Added the `import_data()` function. Adjusted the
  documentation and this Read-Me file accordingly.
- 11 April 2023: Added the `force_libPaths()` function. Shortened the
  Description text. Adjusted the documentation and this Read-Me file
  accordingly. Fixed some grammatical errors in the documentation.
- 17 & 20 April 2023: Small textual changes to the documentation and
  Read-Me. file.
- 22 April 2023: Added more tests for `stri_locate_ith()`.
- 22 May 2023: Re-arranged the sections of the Read-Me file. Removed the
  CodeFactor badge as it was bugging out for some reason.
- 25 May 2023: The `stri_locate_ith()` function slightly re-written to
  be even faster, and improved its documentation. Edited the
  description. Changed the documentation regarding matrix operators, as
  I suspect the usage of the word “rank” might be confusing. Also edited
  this Read-Me file a bit.
- 27 May 2023: Changed the naming convention of in-place modifiers to
  end with `=%`. Added the `import_lsf()` function. Fixed a mistake in
  the documentation of the `transform_if()`.
- 29 May 2023: The `stri_locate_ith()` now returns a matrix like
  `stri_locate_first/last`. Moreover, I replaced the `mapply` call with
  only vectorized functions; `stri_locate_ith()` is now almost as fast
  as the `stringi` functions it calls.
- 30 May 2023: Fixed some lingering mistakes in the documentations, that
  were left over since the changes from 29 May 2023.
- 4 June 2023: Major changes this time. The `tranform_if()` function now
  allows the user to specify 2 transformation functions, one for if the
  condition is `TRUE`, and the other for `FALSE`. Added the
  `import_inops()` function. Changed the `alias %m import <-% pkgs`
  operator into the `import_as()` function. Added the
  `pkgs %installed in% lib.loc%` operator. Re-arranged this Read-Me file
  a bit.
- 15 June 2023: Rewritten this Read-Me a bit. Added a module import
  system (`alias %source module <-% list(file=...)` operator and
  `source_inops()` function).
- 16 June 2023: RENAMED THIS R-PACKAGE TO `tinyoperators`, to prevent
  confusion as it holds to the `tinyverse` philosophy, rather then the
  “tidy philosophy”. Fixed some minor errors in the documentation.
  Removed `force_libPaths()`, as it may encourage bad coding practices.
- 19 June 2023: Replaced the in-place mathematical modifiers with a
  generalized, function-based in place (mathematical) modification
  operator, `%:=%`, to solve precedence issues. Renamed
  `alias %source module <-% list(file=...)` to
  `alias %source from% list(file=...)`. Edited the documentation a bit.
- 20 June 2023: Extended the import section of this Read-Me more.
- 21 June 2023: Migrated the tests from `testthat` to `tinytest`. Added
  tests for the new `%:=%` operator. Adjusted the documentation, and
  this Read-Me file. Renamed the `%source from%` operator to
  `%@source%`. Removed the `%installed in%` operator.
- 26 June 2023: Another major change. Made `import_as()` much, much
  stricter. Brought back the `pkgs %installed in% lib.loc%` operator.
  Adjusted the documentation accordingly. Asked some people to start
  testing the R package, as it is nearing it’s final form before getting
  out of the experimental life cycle.
- 27 June 2023: Fixed a bug in the `lib.loc` argument of the `import_`
  functions. Made the output of the `import_` functions a little bit
  more compact. Fixed a small bug in an internal function. Added the
  `base` and `recom` arguments to the `pkgs_get_deps()` function.
  Clarified the documentation a bit. Fixed some spelling errors.
  Extended the example in the “import” section of the Read-Me. The
  Read-Me now has section numbers.
- 28 June 2023: renamed the `depends` and `extends` arguments of
  `import_as()` to `dependencies` and `extenions`, to avoid confusion,
  and added the `foreign_exports` argument as well.
- 1 July 2023: Added the `overwrite` argument to `import_as()`, and made
  its alias checks more rigorous. Adjusted the documentation a bit.
  Added some more tests. Fixed some minor bugs in the `source_module`
  functions.
- 2 July 2023: Removed the `overwrite` argument from `import_as()` (that
  was a short-lived argument…), as I’ve found it to be more annoying
  than it’s worth. Added more internal checks to the `import_` and
  `source_module` functions. Error messages now display the used
  exported function instead of internal functions. Adjusted the
  documentation and Read-Me accordingly.

FUTURE PLANS:

I believe `tinyoperators` is slowly getting closer to becoming stable.
There does not appear a need to add/remove many more
functions/operators, although some functions, operators or arguments may
need to be tweaked and/or optimized. Once I am fully satisfied with the
R package (which may take a while, as I am a bit of a perfectionist), I
may attempt to publish this R package to CRAN.

 

## 1.3 Installation

You can install `tinyoperators` from github like so:

``` r
remotes::install_github("https://github.com/tony-aw/tinyoperators")
```

You can attach the package - thus exposing its functions to your
namespace - using:

``` r
library(tinyoperators)
```

and one can open the introduction page to the `tinyoperators` package
using:

``` r
tinyoperators::tinyoperators_help()
```

 

## 1.4 Overview

The `tinyoperators` R-package adds some infix operators, and a few
functions. It primarily focuses on 4 things:

1)  Float truth testing.
2)  Reducing repetitive code.
3)  Extending the string manipulation capabilities of the `stringi` R
    package.
4)  A new package and module import system, that combines the benefits
    of aliasing a package with the benefits of attaching a package.

The `tinyoperators` R-package has only one dependency, namely `stringi`,
though it does allows multi-threading of some of the string-related
functions (when appropriate) via the suggested `stringfish` R-package.
Most functions in this R-package are fully vectorized and have been
optimized for optimal speed and performance.

 

Currently this R package is only available on GitHub.

 

I understand you may not want to go through this entire Read-Me without
knowing if the R package is even worthy of your time. Therefore, allow
me to give you a quick glimpse of what is possible in this R package
before jumping into the details.

Safer float truth testing:

``` r
x <- c(0.3, 0.6, 0.7)
y <- c(0.1*3, 0.1*6, 0.1*7)
print(x); print(y)
#> [1] 0.3 0.6 0.7
#> [1] 0.3 0.6 0.7

x == y # gives FALSE, but should be TRUE
#> [1] FALSE FALSE FALSE

x %f==% y # here it's done correctly
#> [1] TRUE TRUE TRUE
```

Operators and functions to reduce repetitive code:

``` r
# in base R:
very_long_name_1 <- ifelse(# repetitive, and gives unnecessary warning
  very_long_name_1 > 0, log(very_long_name_1), very_long_name_1^2
)
mtcars$mpg[mtcars$cyl>6] <- (mtcars$mpg[mtcars$cyl>6])^2

# with tinyoperators:
very_long_name_1 %<>% transform_if(\(x)x>0, log, \(x)x^2) # compact & no warning
mtcars$mpg[mtcars$cyl>6] %:=% \(x)x^2
```

Locate $i^\textrm{th}$ occurrence of some pattern in a string:

``` r
x <- c("Goodmorning -- GOODafternoon -- GooDevening, and goodnight!",
       paste0(letters[1:13], collapse=""))
print(x)
#> [1] "Goodmorning -- GOODafternoon -- GooDevening, and goodnight!"
#> [2] "abcdefghijklm"
loc <- stri_locate_ith(
  # locate second-last occurrence of "good" (ignore case) of each string in x:
  x, -2, regex="good", case_insensitive=TRUE)
substr(x, loc[,1], loc[,2])
#> [1] "GooD" NA
```

String re-ordering using matrix re-ordering:

``` r
# sorting words:
x <- c("Hello everyone, I'm here", "Goodbye everyone")
print(x)
#> [1] "Hello everyone, I'm here" "Goodbye everyone"
x <- stringi::stri_split_boundaries(
  x, simplify = TRUE, type="word" # vector to matrix
)
mat <- stringi::stri_rank(as.vector(x)) |>
  matrix(ncol=ncol(x)) # matrix of ordering ranks
sorted <- x %row~% mat # matrix re-ordering
stri_c_mat(sorted, margin=1, sep=" ") # row-wise concatenate strings
#> [1] "      , everyone Hello here I'm" "       everyone Goodbye"
```

Using `tinyoperator's` new import system; note that the following code
is run **without attaching a single R package**:

``` r
import_as( # loading "dplyr" + its foreign exports + "powerjoin" under alias "dr." 
  dr., "dplyr", extensions = "powerjoin"
)
#> listing foreign exports from package: dplyr...
#> Importing package: dplyr...
#> 
#> Importing package: powerjoin... no conflicts
#> 
#> Done
#> You can now access the functions using dr.$...
#> (S3)methods will work like normally.

import_inops("magrittr") # exposing operators from `magrrittr` to current env
#> Getting infix operators from package: magrittr...
#> 
#> Placing infix operators in current environment...
#> Done

d <- import_data("starwars", "dplyr") # directly assigning the "starwars" dataset to object "d"

d %>% dr.$filter(species == "Droid") %>%
  dr.$select(name, dr.$ends_with("color"))
#> # A tibble: 6 × 4
#>   name   hair_color skin_color  eye_color
#>   <chr>  <chr>      <chr>       <chr>    
#> 1 C-3PO  <NA>       gold        yellow   
#> 2 R2-D2  <NA>       white, blue red      
#> 3 R5-D4  <NA>       white, red  red      
#> 4 IG-88  none       metal       red      
#> 5 R4-P17 none       silver, red red, blue
#> 6 BB8    none       none        black

myalias. %@source% list(file="sourcetest.R") # source a script under an alias
#> Importing module ...
#> Done
#> You can now access the sourced objects using myalias.$...
myalias.$helloworld() # run a function that was just sourced.
#> [1] "hello world"
```

If you’re still interested, I invite you to read the rest of this
Read-Me, and perhaps try out the package yourself.

 

# 2 Additional logic operators

The tinyoperators package adds a few basic logic operators:

- `%xor%`: Exclusive OR
- `%n&%`: NOT AND (i.e. `(!x) & (!y)`). Note that if either `x` or `y`
  is `NA`, `%n&%` will also give `NA` (unlike `(!x) & (!y)`, which would
  give `FALSE`.)
- `%?=%`: checks if both `x` and `y` are unknown or unreal (NA, NaN,
  Inf, -Inf)
- `%out%`: the opposite of `%in%` (i.e. `!x %in% y`)
- `s %sgrep% p` checks if pattern `p` (defined as either `regex`, or as
  a call from `s_pattern()`) appears in character vector `s` (info on
  the `s_pattern()` function can be found in the string section of this
  read-me)

Here are some examples:

``` r
x <- c(TRUE, FALSE, TRUE, FALSE, NA, NaN, Inf, -Inf, TRUE, FALSE)
y <- c(FALSE, TRUE, TRUE, FALSE, rep(NA, 6))
outcome <- data.frame(
  x=x, y=y,
  "x %xor% y"=x %xor% y, "x %n&% y" = x %n&% y, "x %?=% y" = x %?=% y,
  check.names = FALSE
)
kable(outcome)
```

|    x | y     | x %xor% y | x %n&% y | x %?=% y |
|-----:|:------|:----------|:---------|:---------|
|    1 | FALSE | TRUE      | FALSE    | FALSE    |
|    0 | TRUE  | TRUE      | FALSE    | FALSE    |
|    1 | TRUE  | FALSE     | FALSE    | FALSE    |
|    0 | FALSE | FALSE     | TRUE     | FALSE    |
|   NA | NA    | NA        | NA       | TRUE     |
|  NaN | NA    | NA        | NA       | TRUE     |
|  Inf | NA    | NA        | NA       | TRUE     |
| -Inf | NA    | NA        | NA       | TRUE     |
|    1 | NA    | NA        | NA       | FALSE    |
|    0 | NA    | NA        | NA       | FALSE    |

``` r

1:3 %out% 1:10
#> [1] FALSE FALSE FALSE
1:10 %out% 1:3
#>  [1] FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
```

Numbers can have many different sub-types whilst still being `numeric`.
The `n %=numtype% numtype` operator will check for every value of
numeric vector `n` if it can be considered a number belonging to type
`numtype`. The following values for `numtype` are allowed:

- “~0”: zero, or else a number whose absolute value is smaller than the
  Machine tolerance (`sqrt(.Machine$double.eps)`);
- “B”: binary numbers (0 or 1);
- “prop”: proportions;
- “N”: Natural numbers (non-negative integers including zero);
- “I”: Integers;
- “odd”: odd integers;
- “even”: even integers
- “R”: Real numbers;
- “unreal”: infinity, NA, or NaN;

The string counterpart for `%=numtype%` is `s %=strtype% strtype`, which
checks for every value of character vector `s` if it can seen as a
certain `strtype`. The following values for `strtype` are allowed:

- “empty”: checks if the string only consists of empty spaces.
- “unreal”: checks if the string is NA, or if it has literal string
  “NA”, “NaN” or “Inf”, regardless if it has leading or trailing spaces.
- “numeric”: checks if the string can be converted to a number,
  disregarding leading and trailing spaces. I.e. the string “5.0” can be
  converted to the the actual number 5.0.
- “special”: checks if the string consists of only special characters.

Here are some examples:

``` r

1e-20 %=numtype% "~0"
#> [1] TRUE
n <- c(0:5, 0:-5, 0.1, -0.1, 0, 1, Inf, -Inf, NA, NaN)
n[n %=numtype% "B"]
#> [1] 0 1 0 0 1
n[n %=numtype% "prop"]
#> [1] 0.0 1.0 0.0 0.1 0.0 1.0
n[n %=numtype% "B"]
#> [1] 0 1 0 0 1
n[n %=numtype% "N"]
#> [1] 0 1 2 3 4 5 0 0 1
n[n %=numtype% "I"]
#>  [1]  0  1  2  3  4  5  0 -1 -2 -3 -4 -5  0  1
n[n %=numtype% "odd"]
#> [1]  1  3  5 -1 -3 -5  1
n[n %=numtype% "even"]
#> [1]  0  2  4  0 -2 -4  0
n[n %=numtype% "R"]
#>  [1]  0.0  1.0  2.0  3.0  4.0  5.0  0.0 -1.0 -2.0 -3.0 -4.0 -5.0  0.1 -0.1  0.0
#> [16]  1.0
n[n %=numtype% "unreal"]
#> [1]  Inf -Inf   NA  NaN

s <- c(" AbcZ123 ", " abc ", " 1.3 ", " !#$%^&*() ", "  ", "  NA  ", "  NaN  ", " Inf ")
s[s %=strtype% "empty"]
#> [1] "  "
s[s %=strtype% "unreal"]
#> [1] "  NA  "  "  NaN  " " Inf "
s[s %=strtype% "numeric"]
#> [1] " 1.3 " " Inf "
s[s %=strtype% "special"]
#> [1] " !#$%^&*() "
```

 

# 3 Safer float truth testing operators

This package adds the `%f==%, %f!=% %f<%, %f>%, %f<=%, %f>=%`
(in)equality operators, which perform safer float truth testing. They
are virtually equivalent to the regular (in)equality operators,
`==, !=, <, >, <=, >=`, except for one aspect. The float truth testing
operators assume that if the absolute difference between `x` and `y` is
smaller than the Machine tolerance, `sqrt(.Machine$double.eps)`, then
`x` and `y` ought to be consider to be equal. Thus these provide safer
float (in)equality operators. For example: `(0.1*7) == 0.7` returns
`FALSE`, even though they are equal, due to the way floating numbers are
stored in programming languages like `R`, `Python`, etc. But
`(0.1*7) %f==% 0.7` returns `TRUE`.

Some examples:

``` r
x <- c(0.3, 0.6, 0.7)
y <- c(0.1*3, 0.1*6, 0.1*7)
print(x); print(y)
#> [1] 0.3 0.6 0.7
#> [1] 0.3 0.6 0.7
x == y # gives FALSE, but should be TRUE
#> [1] FALSE FALSE FALSE
x!= y # gives TRUE, should be FALSE
#> [1] TRUE TRUE TRUE
x > y # not wrong
#> [1] FALSE FALSE FALSE
x < y # gives TRUE, should be FALSE
#> [1] TRUE TRUE TRUE
x %f==% y # here it's done correctly
#> [1] TRUE TRUE TRUE
x %f!=% y
#> [1] FALSE FALSE FALSE
x %f<% y # correct
#> [1] FALSE FALSE FALSE
x %f>% y # correct
#> [1] FALSE FALSE FALSE
x %f<=% y # correct
#> [1] TRUE TRUE TRUE
x %f>=% y # correct
#> [1] TRUE TRUE TRUE

x <- c(0.3, 0.6, 0.7)
bnd <- matrix(c(x-0.1, x+0.1), ncol=2)
x %f{}% bnd
#> [1] TRUE TRUE TRUE
x %f!{}% bnd
#> [1] FALSE FALSE FALSE
```

Although designed for objects (vectors, matrices, arrays) of class
`double` (floating numbers), these operators also work correctly for
integers. These operators do not work for non-numeric objects.

 

# 4 Matrix re-ordering operators

The `tinyoperators` R package adds 2 additional matrix operators:

- The `x %row~% mat` operator re-orders the elements within every row of
  matrix `x` by the ordering ranks given in matrix `mat`.
- The `x %col~% mat` operator re-orders the elements within every column
  of matrix `x` by the ordering ranks given in matrix `mat`.

If matrix `x` is a numeric matrix, and one wants to numerically sort the
elements of every row or column, `x %row~% x` or `x %col~% x` would
suffice, respectively.

If matrix `x` is not numeric, sorting using `x %row~% x` and
`x %col~% x`are still possible, but probably not the best option. In the
non-numeric case, providing a matrix of ordering ranks would probably be
faster and give more accurate ordering.

If `mat` is a matrix of non-repeating random integers
(i.e. `sample(1:length(x), replace=FALSE)`), `x %row~% mat` will
randomly shuffle the elements of every row, where the shuffling order of
every row is independent of the other rows. Similarly, `x %col~% mat`
will randomly shuffle the elements of every column, where the shuffling
order of every column is independent of the other columns.

 

Examples with a numeric matrix:

``` r
x <- matrix(sample(1:25), nrow=5)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25   11   16    9    8
#> [2,]    4   14   10   15   13
#> [3,]    7   18    6   12   21
#> [4,]    1   22   19   17    3
#> [5,]    2    5   23   20   24
x %row~% x # sort elements of every row
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    8    9   11   16   25
#> [2,]    4   10   13   14   15
#> [3,]    6    7   12   18   21
#> [4,]    1    3   17   19   22
#> [5,]    2    5   20   23   24
x %row~% -x # reverse-sort elements of every row
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25   16   11    9    8
#> [2,]   15   14   13   10    4
#> [3,]   21   18   12    7    6
#> [4,]   22   19   17    3    1
#> [5,]   24   23   20    5    2
x %col~% x # sort elements of every column
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    5    6    9    3
#> [2,]    2   11   10   12    8
#> [3,]    4   14   16   15   13
#> [4,]    7   18   19   17   21
#> [5,]   25   22   23   20   24
x %col~% -x # reverse-sort elements of every column
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25   22   23   20   24
#> [2,]    7   18   19   17   21
#> [3,]    4   14   16   15   13
#> [4,]    2   11   10   12    8
#> [5,]    1    5    6    9    3

x <- matrix(sample(1:25), nrow=5)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25    3   23    9   11
#> [2,]   12    6    4    7   21
#> [3,]   15   10   24   16    2
#> [4,]    1   18   14   22    8
#> [5,]   20   19   17   13    5
rand <- sample(1:length(x)) |> matrix(ncol=ncol(x)) # matrix of random integers
x %row~% rand # random shuffle every row independent of other rows
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   23   11    9    3   25
#> [2,]   12    4    6    7   21
#> [3,]    2   15   24   16   10
#> [4,]    1   14    8   18   22
#> [5,]   19   20   17    5   13
x %col~% rand # random shuffle every column independent of other columns
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   20   19   23   16   11
#> [2,]   15    3   17    9    2
#> [3,]   12    6   24   13    5
#> [4,]    1   10    4    7    8
#> [5,]   25   18   14   22   21
```

Examples with a character matrix:

``` r
x <- matrix(sample(letters, 25), nrow=5)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "w"  "l"  "s"  "q"  "e" 
#> [2,] "n"  "p"  "i"  "b"  "j" 
#> [3,] "t"  "a"  "z"  "r"  "k" 
#> [4,] "g"  "v"  "y"  "d"  "x" 
#> [5,] "m"  "f"  "c"  "u"  "o"
mat <- stringi::stri_rank(as.vector(x)) |>
  matrix(ncol=ncol(x)) # matrix of ordering ranks
x %row~% mat # sort elements of every row
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "e"  "l"  "q"  "s"  "w" 
#> [2,] "b"  "i"  "j"  "n"  "p" 
#> [3,] "a"  "k"  "r"  "t"  "z" 
#> [4,] "d"  "g"  "v"  "x"  "y" 
#> [5,] "c"  "f"  "m"  "o"  "u"
x %row~% -mat # reverse-sort elements of every row
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "w"  "s"  "q"  "l"  "e" 
#> [2,] "p"  "n"  "j"  "i"  "b" 
#> [3,] "z"  "t"  "r"  "k"  "a" 
#> [4,] "y"  "x"  "v"  "g"  "d" 
#> [5,] "u"  "o"  "m"  "f"  "c"
x %col~% mat # sort elements of every column
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "g"  "a"  "c"  "b"  "e" 
#> [2,] "m"  "f"  "i"  "d"  "j" 
#> [3,] "n"  "l"  "s"  "q"  "k" 
#> [4,] "t"  "p"  "y"  "r"  "o" 
#> [5,] "w"  "v"  "z"  "u"  "x"
x %col~% -mat # reverse-sort elements of every column
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "w"  "v"  "z"  "u"  "x" 
#> [2,] "t"  "p"  "y"  "r"  "o" 
#> [3,] "n"  "l"  "s"  "q"  "k" 
#> [4,] "m"  "f"  "i"  "d"  "j" 
#> [5,] "g"  "a"  "c"  "b"  "e"

x <- matrix(sample(letters, 25), nrow=5)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "z"  "u"  "c"  "f"  "s" 
#> [2,] "o"  "l"  "w"  "m"  "r" 
#> [3,] "x"  "g"  "v"  "d"  "n" 
#> [4,] "j"  "h"  "k"  "t"  "b" 
#> [5,] "p"  "a"  "q"  "e"  "y"
rand <- sample(1:length(x)) |> matrix(ncol=ncol(x)) # matrix of random integers
x %row~% rand # random shuffle every row independent of other rows
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "z"  "f"  "s"  "u"  "c" 
#> [2,] "r"  "l"  "w"  "o"  "m" 
#> [3,] "n"  "x"  "d"  "v"  "g" 
#> [4,] "k"  "b"  "j"  "h"  "t" 
#> [5,] "a"  "p"  "y"  "e"  "q"
x %col~% rand # random shuffle every column independent of other columns
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "x"  "a"  "k"  "d"  "n" 
#> [2,] "z"  "l"  "w"  "f"  "r" 
#> [3,] "p"  "u"  "v"  "e"  "b" 
#> [4,] "o"  "g"  "q"  "m"  "y" 
#> [5,] "j"  "h"  "c"  "t"  "s"
```

These operators internally only use vectorized operations (no loops or
apply-like functions), and are faster than re-ordering matrices using
loops or apply-like functions.

 

# 5 String functions

## 5.1 Matrix joining

The `tinyoperators` package adds a tiny additional function to
`stringi`:

`stri_join_mat` (and their aliases `stri_c_mat` and `stri_paste_mat`).

As the name suggests, these functions perform row-wise (`margin=1`; the
default) or column-wise (`margin=2`) joining of a matrix of strings,
thereby transforming it to a vector of strings. You can do this already
in base R, but it requires converting the matrix to a data.frame or
list, and then calling `stri_join` inside `do.call()`, which to me just
seems too much trouble for something *soooo* abysmally simple.

Here is an example of their usage when re-ordering strings, words, or
sentences :

``` r
# sorting characters in strings:
x <- c("Hello world", "Goodbye world")
print(x)
#> [1] "Hello world"   "Goodbye world"
x <- stringi::stri_split_boundaries(x, simplify = TRUE, type="character")
mat <- stringi::stri_rank(as.vector(x)) |>  matrix(ncol=ncol(x))
sorted <- x %row~% mat # fast sort matrix row-wise
stri_join_mat(sorted, margin=1, sep="") # <- using new function here
#> [1] " deHllloorw"   " bddeGlooorwy"

# sorting words:
x <- c("Hello everyone", "Goodbye everyone")
print(x)
#> [1] "Hello everyone"   "Goodbye everyone"
x <- stringi::stri_split_boundaries(x, simplify = TRUE, type="word")
mat <- stringi::stri_rank(as.vector(x)) |>  matrix(ncol=ncol(x))
sorted <- x %row~% mat # <- fast sort matrix row-wise
stri_c_mat(sorted, margin=1, sep=" ") # <- alias for stri_join_mat
#> [1] "  everyone Hello"   "  everyone Goodbye"

# randomly shuffle sentences:
x <- c("Hello, who are you? Oh, really?! Cool!", "I don't care. But I really don't.")
print(x)
#> [1] "Hello, who are you? Oh, really?! Cool!"
#> [2] "I don't care. But I really don't."
x <- stringi::stri_split_boundaries(x, simplify = TRUE, type="sentence")
mat <- sample(1:length(x)) |> matrix(ncol=ncol(x))
shuffled <- x %row~% mat # <- fast sort matrix row-wise
print(shuffled)
#>      [,1]                   [,2]             [,3]                 
#> [1,] "Hello, who are you? " "Oh, really?! "  "Cool!"              
#> [2,] ""                     "I don't care. " "But I really don't."
stri_paste_mat(shuffled, margin=1, sep=" ") # <- another alias for stri_join_mat
#> [1] "Hello, who are you?  Oh, really?!  Cool!"
#> [2] " I don't care.  But I really don't."
```

 

## 5.2 stri_locate_ith

Suppose one wants to transform the **first** vowels in the strings of a
character vector `str`, such that all upper case vowels become lower
case, and vice-versa. One can do that completely in `stringi` + base R
as follows:

``` r

x <- c("HELLO WORLD", "goodbye world")
loc <- stringi::stri_locate_first(x, regex="a|e|i|o|u", case_insensitive=TRUE)
extr <- stringi::stri_sub(x, from=loc)
repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
stringi::stri_sub_replace(x, loc, replacement=repl)
#> [1] "HeLLO WORLD"   "gOodbye world"
```

But now suppose one wants to transform the **second-last** vowel. How
are you going to do that? It’s not impossible, but also not super
straight-forward. For clear code, `stringi` really needs some kind of
“stri_locate_ith” function. And, of course, the `tinyoperators` package
provides just that.

The `stri_locate_ith(str, i, ...)` function locates for every
element/string in character vector `str`, the $i^\textrm{th}$ occurrence
of some (regex/fixed/etc) pattern. When `i` is positive, the occurrence
is counted from left to right. Negative values for `i` are also allowed,
in which case the occurrence is counted from the right to left. But
`i=0` is not allowed though. Thus, to get the **second** occurrence of
some pattern, use `i=2`, and to get the **second-last** occurrence, use
`i=-2`.

The `stri_locate_ith(str, i, ...)` function uses the exact same argument
and naming convention as `stringi`, to keep your code consistent. And
just like `stringi::stri_locate_first/last`, the
`stri_locate_ith(str, i, ...)` function is a vectorized function: `str`
and `i` as well as the pattern (`regex, fixed, coll, charclass`) can all
be different-valued vectors. It is also vectorized in the sense that no
loops (in R) are used, only vectorized functions.

 

To transform the **second-last** occurrence, one can now use
`stri_locate_ith()` in a very similar way as was done with
`stri_locate_first/last`:

``` r
x <- c("HELLO WORLD", "goodbye world")

loc <- stri_locate_ith( # this part is the key-difference
  x, -2, regex="a|e|i|o|u", case_insensitive=TRUE
)

extr <- stringi::stri_sub(x, from=loc)
repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
stringi::stri_sub_replace(x, loc, replacement=repl)
#> [1] "HELLo WORLD"   "goodbyE world"
```

Notice that the code is virtually equivalent. We *only* need to change
the locate function.

Regarding its speed:

The `stri_locate_ith()` function needs to know the number of matches of
some pattern for every string in a character vector, as well as the
actual matches themselves, before being able to actually give the
$i^\textrm{th}$ occurrence of some pattern. Thus `stri_locate_ith()` (at
least in its current implementation) cannot be faster than the combined
run-time of the `stri_locate_all()` and `stri_count()` functions. As
`stri_locate_ith()` is written only in fully vectorized statements in R
(no loops), the function hardly takes more than twice the time of
`stri_locate_all()` and `stri_count()` combined.

 

## 5.3 Substr - functions

The `tinyoperators` R-package includes the following “substr-”
functions:

- The `substr_repl(x, rp, ...)` function replaces a position (range)
  with string `rp`.
- The `substr_chartr(x, old, new, ...)` function transforms the
  sub-string at a position (range) using `chartr(old, new)`. By default,
  it will translate upper-case characters to lower-case, and vice-versa.
- The `substr_addin(x, addition, side, ...)` function adds the
  additional string `addition` at the side `side` of a position.
- The `substr_extract(x, type, ...)` function extracts the string at,
  before, or after some position (range).
- The `substr_arrange(x, arr, ...)` function re-arranges (sort, reverse,
  randomize) the sub-string at a position (range).

The “position” in the functions above can be specified either by giving
the result of the `stri_locate_ith()` function (see the previous
section) in argument `loc`, or one can give manual positions using the
`start, end` or `at` arguments.

Examples:

``` r
x <- c("Good - good - GOOD", "Good - GOOD - good")
loc <- stri_locate_ith(
  # locate second "good" (ignore case) of each string in x:
  x, 2, regex="good", case_insensitive=TRUE 
)
```

|                                           |                       |                       |
|:------------------------------------------|:----------------------|:----------------------|
| x                                         | Good - good - GOOD    | Good - GOOD - good    |
| substr_extract(x, loc=loc)                | good                  | GOOD                  |
| substr_extract(x, “before”, loc=loc)      | Good -                | Good -                |
| substr_extract(x, “after”, loc=loc)       | \- GOOD               | \- good               |
| substr_repl(x, “??”, loc=loc)             | Good - ?? - GOOD      | Good - ?? - good      |
| substr_chartr(x, loc=loc)                 | Good - GOOD - GOOD    | Good - good - good    |
| substr_addin(x, “\~~”, “after”, loc=loc)  | Good - good\~~ - GOOD | Good - GOOD\~~ - good |
| substr_addin(x, “\~~”, “before”, loc=loc) | Good - \~~good - GOOD | Good - \~~GOOD - good |
| substr_arrange(x, loc=loc)                | Good - dgoo - GOOD    | Good - DGOO - good    |
| substr_arrange(x, “decr”, loc=loc)        | Good - oogd - GOOD    | Good - OOGD - good    |
| substr_arrange(x, “rev”, loc=loc)         | Good - doog - GOOD    | Good - DOOG - good    |
| substr_arrange(x, “rand”, loc=loc)        | Good - ogod - GOOD    | Good - ODGO - good    |

 

# 6 String infix operators

The `tinyoperators` R package implements infix operators for string
arithmetic and sub-setting, as well some of their in-place modifier
equivalents. For consistency, and to avoid masking other common
operators, all string-related operators start with `%s`, where the “s”
stands for “string”.

 

## 6.1 String subsetting operators

As a first sub-setting operator, we have `x %sget% ss`, which returns a
subset of each string in character vector `x`. Here `ss` is a vector of
length 2, or a matrix with `nrow(ss)=length(x)` and 2 columns. The
object `ss` should consist entirely of non-negative integers (thus 0, 1,
2, etc. are valid, but -1, -2, -3 etc are not valid). The first
element/column of ss gives the number of characters counting from the
left side to be extracted from x. The second element/column of ss gives
the number of characters counting from the right side to be extracted
from x.

Here are 2 examples:

``` r
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(2,3)
x %sget% ss
#> [1] "abklm" "noxyz"

x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(1,0)
x %sget% ss
#> [1] "a" "n"
```

Thus `x %sget% ss` “gets” or extracts the given number of characters
from the left and the right, and removes the rest. There is also
`x %strim% ss`, which is the opposite: it trims away the number of
characters from the left and right as defined in the matrix `ss`,
leaving you with whatever is left.

Here are again 2 examples:

``` r
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(2,3)
x %strim% ss
#> [1] "cdefghij" "pqrstuvw"

x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(1,0)
x %strim% ss
#> [1] "bcdefghijklm" "opqrstuvwxyz"
```

 

## 6.2 String arithmetic

The `tinyoperators` package adds 4 string arithmetic operators:

- `x %s+% y` concatenates `x` and `y`;
- `x %s-% p` removes pattern `p` from each string in character vector
  `x`;
- `x %s*% n` repeats each string in character vector `x` for `n` times;
- `x %s/% p` counts how often pattern `p` occurs in string or vector `x`

I.e.:

``` r
"Hello "%s+% " world"
#> [1] "Hello  world"
c("Hello world", "Goodbye world") %s-% " world"
#> [1] "Hello"   "Goodbye"
c("Ha", "Ho", "Hi", "Hu", "He", "Ha") %s*% 2:7
#> [1] "HaHa"           "HoHoHo"         "HiHiHiHi"       "HuHuHuHuHu"    
#> [5] "HeHeHeHeHeHe"   "HaHaHaHaHaHaHa"
c("hello world & goodbye world", "world domination!") %s/% "world"
#> [1] 2 1
```

The right-side arguments `p`, `y`, and `n` can be a single value, or a
vector of the same length as `x`.

 

## 6.3 Specifying Pattern search attributes in string infix operators

The `x %s-% p` and `x %s/% p` operators (and their in-place equivalents,
given later), and the `%sgrep%` operator perform pattern matching for
various purposes. By default the pattern matching is interpreted as
case-sensitive `regex` patterns from `stringi`.

But, of course, sometimes one wants to change this. For example, one may
want it to be case insensitive. Or perhaps one wants to use fixed
expressions, or something else.

The `tinyoperators` package provides options for these cases. To use
more refined pattern definition, simply replace the
argument/right-hand-side expression `p` in the relevant operators with a
call from the `s_pattern()` function.

The `s_pattern()` function uses the exact same argument convention as
`stringi`. For example:

- `s_pattern(regex=p, case_insensitive=FALSE, ...)`
- `s_pattern(fixed=p, ...)`
- `s_pattern(coll=p, ...)`
- `s_pattern(charclass=p, ...)`

Note that the `s_pattern()` function only works for the infix operators
(functions surrounded by `%` signs) related to strings in this R
package.

Examples with Regular expressions:

``` r
x <- c("Hello world", "Goodbye world")
p <- s_pattern(regex=" world")
x %s-% p
#> [1] "Hello"   "Goodbye"

x <- c("Ha", "Ho", "Hi", "Hu", "He", "Ha") %s*% 10
p <- s_pattern(regex="Ha")
x %s/% p
#> [1] 10  0  0  0  0 10
```

Examples with Fixed expressions:

``` r
x <- c("Hello world", "Goodbye world")
p <- s_pattern(fixed=" world")
x %s-% p
#> [1] "Hello"   "Goodbye"

x <- c("Ha", "Ho", "Hi", "Hu", "He", "Ha") %s*% 10
p <- s_pattern(fixed="Ha")
x %s/% p
#> [1] 10  0  0  0  0 10
```

And so on. I’m sure you get the idea.

 

# 7 “DRY” - operators

## 7.1 The transform_if function, and related operators

“Don’t Repeat Yourself”, sometimes abbreviated as “DRY”, is the coding
principle that you should try to reduce repeating patterns in your code
(within reason).

Consider the following code:

`x <- ifelse(cond(x), f(x), g(x))`

Here a conditional subset of the object `x` is transformed where the
condition is using a function referring to `x` itself. Consequently,
reference to `x` is written **four times**! If the object has a short
name like “x, this doesn’t matter too much. But if the object has a
longer name like `very_long_name_1`, doing something like this:

``` r
very_long_name_1 <- ifelse(
  very_long_name_1>0,  log(very_long_name_1), very_long_name_1^2
)
```

becomes cumbersome quickly.

The `tinyoperators` package therefore adds the
`transform_if(x, cond, trans_T, trans_F)` function which will “dry” this
up. The above code can now be re-written as:

``` r
very_long_name_1 %<>% transform_if(\(x)x>0, log, \(x)x^2)
```

Note that `x` must be a vector, matrix, or array. Unlike `ifelse`, the
transformations in `transform_if()` are evaluated as
`trans_T(x[cond(x)])` and `trans_F(x[!cond(x)])`, ensuring no
unnecessary warnings or errors occur.

Besides `transform_if`, the `tinyoperators` package also adds 2
“subset_if” operators:

- The `x %[if]% cond` operator selects elements from vector/matrix/array
  `x`, for which the result of `cond(x)` returns `TRUE`.

- The `x %[!if]% cond` operator selects elements from
  vector/matrix/array `x`, for which the result of `cond(x)` returns
  `FALSE`.

For example:

``` r
object_with_very_long_name <- matrix(-10:9, ncol=2)
print(object_with_very_long_name)
#>       [,1] [,2]
#>  [1,]  -10    0
#>  [2,]   -9    1
#>  [3,]   -8    2
#>  [4,]   -7    3
#>  [5,]   -6    4
#>  [6,]   -5    5
#>  [7,]   -4    6
#>  [8,]   -3    7
#>  [9,]   -2    8
#> [10,]   -1    9
object_with_very_long_name %[if]% \(x)x %in% 1:10
#> [1] 1 2 3 4 5 6 7 8 9
object_with_very_long_name %[!if]% \(x)x %in% 1:10
#>  [1] -10  -9  -8  -7  -6  -5  -4  -3  -2  -1   0
```

Another operator added by `tinyoperators` is `x %unreal =% y`, which
replaces all NA, NaN, Inf and -Inf in `x` with the value given in `y`.

So `x %unreal =% y` is the same as
`x[is.na(x)|is.nan(x)|is.infinite(x)] <- y`.

 

## 7.2 Generalized in-place (mathematical) modifier

This R package includes infix operators for in-place modifying
mathematical arithmetic.

Consider the following line of code:

``` r
mtcars$mpg[mtcars$cyl>6] <- mtcars$mpg[mtcars$cyl>6]^2
```

The same expression, `mtcars$mpg[mtcars$cyl>6]`, is written twice,
making this code rather long and cumbersome, even though we’re just
squaring the expression. The well-known `magrittr` R-package has an
in-place modifier pipe, `%<>%`. This works excellent for actual
functions, like so:

``` r
mtcars$mpg[mtcars$cyl>6] %<>% mean() # same as mtcars$mpg[mtcars$cyl>6] <- mean(mtcars$mpg[mtcars$cyl>6])
```

But for arithmetic, one needs to translate the operators into a
function, and then perform the in-place modifier pipe:

``` r
mtcars$mpg[mtcars$cyl>6] %<>% raise_to_power(2)
```

This is better, but can we do even better?

This R package solves the above laid-out problem by implementing a
generalized in-place (mathematical) modifier, through the `x %:=% f`
operator.

Lets look at the original problem:

``` r
mtcars$mpg[mtcars$cyl>6] <- mtcars$mpg[mtcars$cyl>6]^2
```

With `tinyoperators` one can now make this more compact (more “tiny”, if
you will) as follows:

``` r
mtcars$mpg[mtcars$cyl>6] %:=% \(x)x^2
```

This function-based method is used instead of the more traditional
in-place mathematical modification like `+=` to prevent precedence
issues (functions come before mathematical arithmetic in `R`).
Precedence issues are a common occurrence in R packages that attempt to
implement in-place modifying arithmetic; the `%:=%` does not have this
problem, as the math is specified inside a function.

In case you’re wondering: the primary differences between
`tinyoperator`’s `%:=%` and `magrittr`’s `%<>%`, are as follows:

- `%:=%` allows (and is even designed for) anonymous (mathematical)
  functions, whereas `%<>%` does not allow anonymous functions.
- `%:=%` is not intended to be chained with more piped functions, unlike
  `%<>%`.

 

## 7.3 In-place modifying string arithmetic and sub-setting

With the exception of `%ss%`, all infix operators for string arithmetic
and string sub-setting have their in-place modifying equivalent:

- `x %s+ =% y` is the same as `x <- x %s+% y`
- `x %s- =% p` is the same as `x <- x %s-% p`
- `x %s* =% n` is the same as `x <- x %s*% n`
- `x %s/ =% p` is the same as `x <- x %s/% p`
- `x %sget =% ss` is the same as `x <- x %sget% ss`
- `x %strim =% ss` is the same as `x <- x %strim% ss`

Notice the extra space before the `=` sign.

 

# 8 Import management

## 8.1 Introduction

One can load a package without attaching a package (i.e. using `::` or
using a package alias), or one can attach a package (i.e. using
`library` or `require()`). Both options have their pros and cons.

The benefits of loading a package using `::` or an alias, instead of
attaching the package, are as follows:

1)  prevent overlapping namespaces / masking functions
2)  prevent overriding core R functions
3)  clarify which function came from which package
4)  prevent attaching functions from a package globally
5)  prevent polluting the namespace (the more packages that are
    attached, the higher chance of masking functions, and the more
    difficult to de-bug your code)

The advantages of attaching a package instead of loading without
attaching are as follows:

1)  Less typing: You have to type less to use functions. This is
    particularly relevant for **infix operators** - which
    `tinyoperators` obviously focuses on - as operators use special
    characters, which require them to be surrounded by back-ticks when
    using `::` or `alias$`.
2)  More collective usage: If multiple packages are meant to work
    together, constantly switching between the different package
    name/alias -prefixes may eventually become annoying and even
    syntactically chaotic.

What `tinyoperators` attempts to do with its import system, is to
somewhat find the best of both worlds. Basically, `tinyoperators` has
functions that allow the following import functionality lacking in base
R:

- Allow package + its dependencies + its extensions (reverse
  dependencies) to be loaded under **one alias**. This essentially
  combines the attaching advantage of “collective usage”, whilst keeping
  most advantages of not attaching a package.
- Allow **exposing infix operators** to the **current environment**.
  This gains the advantage of “less typing”, whilst simultaneously
  avoiding the disadvantage of “global assignment”.

Moreover, `tinyoperators` extends this functionality to also work on
**sourced modules**.

Just to clarify: I am not claiming this import system provided in the
`tinyoperators` package is always the best system or whatever. This is
just another option provided, just like the `import` and `box` packages
provide additional options. Please feel free to completely ignore this
section if you prefer using `library()` :-)

This section of the Read-Me is rather lengthy, so I will start with a
super quick example code using `tinyoperator`’s import system:

``` r
import_as( # loading "dplyr" + its foreign exports + "powerjoin" under alias "dr." 
  dr., "dplyr", extensions = "powerjoin"
)
#> listing foreign exports from package: dplyr...
#> Importing package: dplyr...
#> 
#> Importing package: powerjoin... no conflicts
#> 
#> Done
#> You can now access the functions using dr.$...
#> (S3)methods will work like normally.

import_inops("magrittr") # exposing operators from `magrrittr` to current env
#> Getting infix operators from package: magrittr...
#> 
#> Placing infix operators in current environment...
#> Done

d <- import_data("starwars", "dplyr") # directly assigning the "starwars" dataset to object "d"

d %>% dr.$filter(species == "Droid") %>%
  dr.$select(name, dr.$ends_with("color"))
#> # A tibble: 6 × 4
#>   name   hair_color skin_color  eye_color
#>   <chr>  <chr>      <chr>       <chr>    
#> 1 C-3PO  <NA>       gold        yellow   
#> 2 R2-D2  <NA>       white, blue red      
#> 3 R5-D4  <NA>       white, red  red      
#> 4 IG-88  none       metal       red      
#> 5 R4-P17 none       silver, red red, blue
#> 6 BB8    none       none        black

myalias. %@source% list(file="sourcetest.R") # source a script under an alias
#> Importing module ...
#> Done
#> You can now access the sourced objects using myalias.$...
myalias.$helloworld() # run a function that was just sourced.
#> [1] "hello world"
```

The above code is run *without* attaching `dplyr`, `powerjoin`, or its
dependencies. So none of the problems with attaching a package is
present.

What follows are descriptions of the functions that together form this
new, infix-operator friendly **&** multi-package assignment friendly,
import management system.

 

## 8.2 import_as

One can load a package without attaching it, and assign it to an alias,
in base R, using, for example:

``` r
alias <- loadNamespace("packagename", lib.loc = lib.loc)
```

Doing the above, instead of attaching a package using `library()` or
`require()`, can (often) be quite beneficial for several reasons.
i.e. prevent overlapping namespaces, prevent overriding core R
functions, prevent polluting the namespace, clarify which function came
from which package, allowing a package to be loaded locally (like only
within a function environment), etc.

Loading a package alias does have some drawbacks. One is that you cannot
easily import multiple packages under the same alias. While one probably
wouldn’t want to import **multiple** packages under a **single alias**
most of the time, there may be a couple of situations where importing
multiple packages into a single alias may be actually preferable:

- If multiple packages are meant to work together, constantly switching
  between the different package name/alias prefixes may eventually
  become annoying and even syntactically chaotic-looking.
- A package may have one or several dependencies that are supposed to be
  loaded together. For example: the `tidytable` package essentially
  needs the `data.table` package to be loaded also.
- A package may have many extensions you may want to load together. For
  example: `ggplot2` has many extensions (see
  <https://exts.ggplot2.tidyverse.org/gallery/>). If one wishes to alias
  `ggplot2` including some of its many extensions, it would be nice to
  load multiple R packages under the same alias.
- Suppose package `B` is supposed to overwrite a couple of functions
  from package `A` (for example if package `B` extends or improves the
  functionality from a function in package `A`). In that case you may
  want to import package `A`, and then overwrite it with package `B`.

So there are several cases where it is perhaps desirable to load
multiple packages under the same alias.

And that is where `tinyoperator`’s `import_as()` function comes in. It
loads an R package under an alias, and also loads any specified
**dependencies** or **extensions** (=reverse-dependencies) of the
package under the very same alias. It also informs the user which
objects from a package will overwrite which objects from other packages,
so you will never be surprised. The `import_as()` function also only
loads exported functions (unlike `loadNamespace()`, which loads both
internal and external functions). This is, I think, more desirable, as
internal function should remain, you know, internal.

`import_as(alias, ...)` is thus essentially the multi-package equivalent
of `alias <- loadNamespace(...)`.

The main arguments of the `import_as()` function are:

- `alias`: the name (unquoted) of the alias under which to load the main
  package, and any specified (reverse) dependencies. To keep aliases
  easily distinguished from other objects that can also be subset with
  the `$` operator, I recommend ending (not starting!) the names of all
  alias names with a dot (.).
- `foreign_exports`: Some R packages export functions that are not
  defined in their own package, but in their direct dependencies -
  “foreign exports”. If `TRUE` (default), these foreign exports are
  added to the alias, analogous to the behaviour of base R’s `::`
  operator. If `FALSE`, foreign exports are not added.
- `main_package`: the name (string) of the main package to load.
- `dependencies`: a character vector giving the dependencies of the main
  package to load under the alias also. One can also set this to `TRUE`,
  in which case all dependencies of the `main_package` are loaded,
  except base and recommended packages. Default is `FALSE`.
- `enhances`: an optional character vector giving the packages enhanced
  by the `main_package` to be loaded under the alias also.
- `extensions`: an optional character vector giving the
  extensions/reverse-dependencies of the main package to load under the
  same alias also.

Here is one example. Lets load `data.table` and then `tidytable`, under
the same alias, which I will call “tdt.” (for “tidy data.table”):

``` r
import_as(tdt., "tidytable", dependencies="data.table") # this creates the tdt. object
#> Importing package: data.table...
#> 
#> listing foreign exports from package: tidytable...
#> Importing package: tidytable... The following conflicting objects detected:
#> last, fread, first, between, %chin%, %like%, setDTthreads, data.table, getDTthreads, fwrite, %between%
#> tidytable will overwrite conflicting objects from previous imported packages...
#> 
#> Done
#> You can now access the functions using tdt.$...
#> (S3)methods will work like normally.
```

Notice that the above is the same as:

``` r
import_as(tdt., "data.table", extensions = "tidytable") # this creates the tdt. object
#> listing foreign exports from package: data.table...
#> Importing package: data.table...
#> 
#> Importing package: tidytable... The following conflicting objects detected:
#> last, fread, first, between
#> tidytable will overwrite conflicting objects from previous imported packages...
#> 
#> Done
#> You can now access the functions using tdt.$...
#> (S3)methods will work like normally.
```

Now you can of course use those loaded packages as one would normally do
when using a package alias.

The order in which `import_as()` loads the packages under the given
alias is specified by the `loadorder` argument. See the help file for
details.

 

## 8.3 import_inops

When aliasing an R package, infix operators are also loaded in the
alias. However, it may be cumbersome to use them from the alias. For
example this:

``` r
import_as(to., "tinyoperators")
to.$`%row~%`(x, mat)
```

or this:

``` r
tinyoperators::`%row~%`(x, mat)
```

is very cumbersome.

Therefore, `tinyoperators` also adds the `import_inops(pkgs)` function,
which exposes the infix operators from the packages specified in
character vector `pkgs` to the current environment (like the global
environment, or the environment within a function), but does not attach
the functions to the namespace.

For example, the following code exposes the infix operators from the
`tidytable` and `data.table` R packages to the current environment, in
this case the global environment:

``` r
pkgs <- c("data.table", "tidytable")
import_inops(pkgs)
#> Getting infix operators from package: data.table...
#> 
#> Getting infix operators from package: tidytable... no conflicts
#> 
#> Placing infix operators in current environment...
#> Done
```

Just like in `import_as()`, the order of the packages matters whenever
there’s a conflict.

The `import_inops()` functions has the `exclude` and `include.only`
arguments to specify exactly which infix operators to expose to the
current environment. This can be handy to prevent overwriting any (user
defined) infix operators already present in the current environment.

To ensure the user can still verify which operator function came from
which package, a “package” attribute is added to each exposed operator.
Naturally, the namespaces of the operators remain intact.

 

## 8.4 import_data

The `import_as()` function imports everything from the package
namespace. But packages often also have data sets, which are often not
part of the namespace.

The `data()` function in core R can already load data from packages, but
this function loads the data into the global environment, instead of
returning the data directly, making assigning the data to a specific
variable a bit annoying. Therefore, the `tinyoperators` package
introduces the `import_data()` function, which directly returns a data
set from a package.

For example, to import the `chicago` data set from the `gamair` R
package, and assign it directly to a variable (without having to do
re-assignment and so on), one simply runs the following:

``` r
d <- import_data("chicago", "gamair")
head(d)
#>   death pm10median pm25median  o3median  so2median    time tmpd
#> 1   130 -7.4335443         NA -19.59234  1.9280426 -2556.5 31.5
#> 2   150         NA         NA -19.03861 -0.9855631 -2555.5 33.0
#> 3   101 -0.8265306         NA -20.21734 -1.8914161 -2554.5 33.0
#> 4   135  5.5664557         NA -19.67567  6.1393413 -2553.5 29.0
#> 5   126         NA         NA -19.21734  2.2784649 -2552.5 32.0
#> 6   130  6.5664557         NA -17.63400  9.8585839 -2551.5 40.0
```

 

## 8.5 lib.loc

All “import\_” functions in the `tinyoperators` package have a `lib.loc`
argument to explicitly specify from where to get your packages (just
like base R’s `loadNamespace()` and `install.packages()` functions).

 

## 8.6 On pronouns and aliases

The `magrittr` and `rlang` packages add “pronouns” to R: `.`, `. data`,
`.env`. Fret not, for pronouns work regardless if you attached a package
or not. And you don’t need to use something like `rlang::.data` or
`.rlang$.data` for a pronoun to work. They just work.

 

## 8.7 Sourcing modules

Scripts with functions that you source (sometimes referred to as
“modules”) can be kind of seen as mini-packages. So `tinyoperators` adds
the functionality similar to the `import_` functions, to sourcing
modules. To this end, the `tinyoperators` R package adds the
`alias %@source% list(file=...)` operator and the `source_inops()`
function. The `alias %@source% list(file=...)` operator sources a script
and returns all the objects in the script under an `alias`, similar to
`import_as()`. The `source_inops()` function sources a script and places
all infix operators in the script (if any) in the current environment
(like the Global environment, or the environment within a function),
similar to `import_inops()`.

Example:

``` r
myalias. %@source% list(file="sourcetest.R")
#> Importing module ...
#> Done
#> You can now access the sourced objects using myalias.$...
source_inops(file="sourcetest.R")
#> placing operators in current environment...

myalias.$helloworld()
#> [1] "hello world"
```

Another example, this time using an expression:

``` r
exprs <- expression({
  helloworld = function()print("helloworld")
  goodbyeworld <- function() print("goodbye world")
  `%s+test%` <- function(x,y) stringi::`%s+%`(x,y)
  `%s*test%` <- function(x,y) stringi::`%s*%`(x,y)
})

myalias. %@source% list(exprs=exprs)
#> Importing module ...
#> Done
#> You can now access the sourced objects using myalias.$...
myalias.$helloworld()
#> [1] "helloworld"


temp.fun <- function(){
  source_inops(exprs=exprs)
  ls()
}
temp.fun()
#> placing operators in current environment...
#> [1] "%s*test%" "%s+test%"
```

 

## 8.8 Miscellaneous functions and operators

There are some additional miscellaneous functions related to the import
system that should perhaps be mentioned also:

- the `pkgs_get_deps()` function gets the dependencies (or the enhances)
  of a package, regardless if the package is CRAN or non-CRAN.
- the `pkgs %installed in% lib.loc` operator checks if the packages
  specified in character vector `pkgs` are installed in library paths
  `lib.loc`, and does this WITHOUT attaching or even loading a package.
- `alias %::?% fun_name` gets the help file for a function `fun_name`
  from the `alias` object.

 

## 8.9 An example

One R package that could benefit from the import system introduced by
`tinyoperators`, is the `dplyr` R package. The `dplyr` R package
overwrites **core R** functions (including base R) and it overwrites
functions from pre-installed recommended R packages (such as `MASS`).
I.e.:

``` r
library(MASS)
library(dplyr) # <- notice dplyr overwrites base R and recommended R packages
#> Warning: package 'dplyr' was built under R version 4.2.3
#> 
#> Attaching package: 'dplyr'
#> The following object is masked _by_ '.GlobalEnv':
#> 
#>     %>%
#> The following object is masked from 'package:MASS':
#> 
#>     select
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# detaching dplyr again:
detach("package:dplyr")
```

Moreover, `dplyr`’s function names are sometimes generic enough that
there is no obvious way to tell if a function came from `dplyr` or some
other package (for comparison: one can generally recognize `stringi`
functions as they all start with `stri_`). Moreover, `dplyr` was
designed to interact with other R packages such as `tidyselect`, and
constantly switching between package prefixes or aliases is perhaps
undesirable. If you look at the CRAN page for `dplyr`, you’ll notice it
has a lot of reverse dependencies, and perhaps you’d like to use one of
those extensions also.

So here `tinyoperator`’s `import_as()` function might help. Below is an
example where `dplyr` is loaded, and `powerjoin` (which is an
extension), all under one alias which I’ll call “`dr.`”.

``` r
tinyoperators::pkgs_get_deps("dplyr") # a lot of dependencies
#>  [1] "cli"        "generics"   "glue"       "lifecycle"  "magrittr"  
#>  [6] "pillar"     "R6"         "rlang"      "tibble"     "tidyselect"
#> [11] "vctrs"

import_as(
  dr., "dplyr", extensions = "powerjoin", lib.loc=.libPaths()
)
#> listing foreign exports from package: dplyr...
#> Importing package: dplyr...
#> 
#> Importing package: powerjoin... no conflicts
#> 
#> Done
#> You can now access the functions using dr.$...
#> (S3)methods will work like normally.

import_inops("magrittr") # getting the operators from `magrrittr`
#> Getting infix operators from package: magrittr...
#> 
#> Placing infix operators in current environment...
#> Done
```

The functions from `dplyr` can now be used with the `dr.$` prefix. This
way, base R functions are no longer overwritten, and it will be clear
for someone who reads your code whether functions like the `filter()`
function is the base R filter function, or the `dplyr` filter function,
as the latter would be called as `dr.$filter()`.

Let’s first run a simple dplyr example code:

``` r
d <- import_data("starwars", "dplyr")
d %>%
  dr.$filter(.data$species == "Droid") %>% # notice the pronoun can be used without problems
  dr.$select(name, dr.$ends_with("color"))
#> # A tibble: 6 × 4
#>   name   hair_color skin_color  eye_color
#>   <chr>  <chr>      <chr>       <chr>    
#> 1 C-3PO  <NA>       gold        yellow   
#> 2 R2-D2  <NA>       white, blue red      
#> 3 R5-D4  <NA>       white, red  red      
#> 4 IG-88  none       metal       red      
#> 5 R4-P17 none       silver, red red, blue
#> 6 BB8    none       none        black
```

Just add `dr.$` in front of the functions you’d normally use, and
everything works just as expected.

Now lets run an example from the `powerjoin` GitHub page
(<https://github.com/moodymudskipper/powerjoin>), using the above alias:

``` r
male_penguins <- dr.$tribble(
     ~name,    ~species,     ~island, ~flipper_length_mm, ~body_mass_g,
 "Giordan",    "Gentoo",    "Biscoe",               222L,        5250L,
  "Lynden",    "Adelie", "Torgersen",               190L,        3900L,
  "Reiner",    "Adelie",     "Dream",               185L,        3650L
)

female_penguins <- dr.$tribble(
     ~name,    ~species,  ~island, ~flipper_length_mm, ~body_mass_g,
  "Alonda",    "Gentoo", "Biscoe",               211,        4500L,
     "Ola",    "Adelie",  "Dream",               190,        3600L,
"Mishayla",    "Gentoo", "Biscoe",               215,        4750L,
)
dr.$check_specs()
#> # powerjoin check specifications
#> ℹ implicit_keys
#> → column_conflict
#> → duplicate_keys_left
#> → duplicate_keys_right
#> → unmatched_keys_left
#> → unmatched_keys_right
#> → missing_key_combination_left
#> → missing_key_combination_right
#> → inconsistent_factor_levels
#> → inconsistent_type
#> → grouped_input
#> → na_keys

dr.$power_inner_join(
  male_penguins[c("species", "island")],
  female_penguins[c("species", "island")]
)
#> Joining, by = c("species", "island")
#> # A tibble: 3 × 2
#>   species island
#>   <chr>   <chr> 
#> 1 Gentoo  Biscoe
#> 2 Gentoo  Biscoe
#> 3 Adelie  Dream
```

Notice that the only change made, is that all functions start with
`dr.$`, the rest is the same. No need for constantly switching between
`dplyr::...`, `powerjoin::...` and so on - yet it is still clear from
the code that they came from the `dplyr` + `powerjoin` family, and there
is no fear of overwriting functions from other R packages - let alone
core R functions.

 

# 9 Multi-threading

## 9.1 Substr-functions

All the string sub-setting functions have the `fish` argument, which is
`FALSE` by default. If `fish=TRUE`, these functions will use
`stringfish` functions instead of base R and `stringi` functions. The
stringfish functions are usually faster, and also allow native
multi-threading. Note that `stringfish` must be installed in order for
this to work. Multi-threading in `stringfish` can be set-up by running
`setoption(stringfish.nthreads=cl)` somewhere at the start of your code,
where `cl` is the number of threads you want to use.

Don’t use multi-threading unless you need to, as multi-threading has
some overhead, thus its only faster with very large character vectors.

Example:

``` r
x <- c("Goodmorning--Goodevening, and Goodnight",
       paste0(letters[1:13], collapse=""))
print(x)
#> [1] "Goodmorning--Goodevening, and Goodnight"
#> [2] "abcdefghijklm"
loc <- stri_locate_ith(
  # locate second-last occurrence of "good" (ignore case) of each string in x:
  x, -2, regex="good", case_insensitive=TRUE 
)
substr_extract(x, loc=loc, fish = TRUE)
#> [1] "Good" NA
substr_extract(x, "before", loc=loc, fish = TRUE)
#> [1] "Goodmorning--" NA
substr_extract(x, "after", loc=loc, fish = TRUE)
#> [1] "evening, and Goodnight" NA
substr_repl(x, "??", loc=loc, fish = TRUE)
#> [1] "Goodmorning--??evening, and Goodnight"
#> [2] "abcdefghijklm"
substr_chartr(x, loc=loc, fish = TRUE)
#> [1] "Goodmorning--gOODevening, and Goodnight"
#> [2] "abcdefghijklm"
substr_addin(x, " ", "after", loc=loc, fish = TRUE) 
#> [1] "Goodmorning--Good evening, and Goodnight"
#> [2] "abcdefghijklm"
substr_addin(x, " ", "before", loc=loc, fish = TRUE) 
#> [1] "Goodmorning-- Goodevening, and Goodnight"
#> [2] "abcdefghijklm"
substr_arrange(x, loc=loc, fish = TRUE)
#> [1] "Goodmorning--dGooevening, and Goodnight"
#> [2] "abcdefghijklm"
substr_arrange(x, "rev", loc=loc, fish = TRUE)
#> [1] "Goodmorning--dooGevening, and Goodnight"
#> [2] "abcdefghijklm"
substr_arrange(x, "decr", loc=loc, fish = TRUE)
#> [1] "Goodmorning--ooGdevening, and Goodnight"
#> [2] "abcdefghijklm"
substr_arrange(x, "rand", loc=loc, fish = TRUE)
#> [1] "Goodmorning--Gdooevening, and Goodnight"
#> [2] "abcdefghijklm"
```

 

# 10 Tinyverse solutions without external R packages

The `tinyverse` attempts to minimize dependencies, and thus avoid using
external R packages unless necessary. “External R packages” here meaning
not base packages, not pre-installed recommended R packages, and not R
packages that are bundled with Rstudio.

There are some forms of proper coding etiquette that are traditionally
achieved through external R packages - even though they can be solved
quite well without external R packages (especially since R version 4+).
Since this R package adheres to the `tinyverse` philosophy and focuses
on coding etiquette, some of these solutions will briefly be given
without the use of external R packages:

- Give warnings when partial argument matching occurs: one can use
  `options( warnPartialMatchArgs = TRUE, warnPartialMatchAttr = TRUE, warnPartialMatchDollar = TRUE)`.
- Force an error when using a function that appears in multiple attached
  packages: one can use the `options(conflicts.policy = ...)` to specify
  when such an error should be enforced.
- Get path of current script or project: without Rstudio one can use
  `commandArgs()` function; with RStudio one can use
  `rstudioapi::getSourceEditorContext()$path`. Using `setwd("..")` will
  set the working directory one folder up - which would often be your
  project root folder.
- Project isolation: set your library to a folder specific to your
  project (i.e. using `.libPaths()`).
- Date-based version control: As MRAN is no longer available, one may
  use the RStudio package manager website as your repository (see
  <https://packagemanager.posit.com/client/#/repos/2/overview> or
  <https://packagemanager.posit.co/client/#/repos/2/overview>).

 

# 11 Compatibility with other R packages

The `stringi` R package has the `%s+%` and `%s*%` operators. They do
exactly the same things as in `tinyoperators`, and so the masking of
these functions can safely be ignored. I also made sure not to name any
of the operators in `tinyoperators` the same as the operators in
`magrittr` and `zeallot`, so that should be safe also.

 

The `import` R package provides somewhat similar capabilities to the
`tinyoperators` import management system, but it’s still different. The
`tinyoperators` import system focuses more on exposing infix operators
to the current environment, and allowing multiple related packages to be
loaded under the same alias and exposing infix operators, both of which
the `import` package does not really provide (as far as I know).
Strictly speaking there is no incompatibility between `tinyoperators`
and `import`. You can safely use both implementations of the import
system, if you wish.

 

When using `renv` R package, note that it only registers packages loads
using plain `library()` or `require()` calls. Anything different from
that, even things like `for(... in ...)library(...)` or
`if(...)library(...)`, will not be understood by `renv`. Therefore, if
using `renv`, please make sure to set the following:

``` r
renv::settings$snapshot.type("all")
```

This will make sure that all packages installed in your project library,
regardless of how they are loaded, will all be registered by `renv`.
This makes `renv` compatible with calls like `import_as` from
`tinyoperators`, and things like `for(... in ...)library(...)` or
`if(...)library(...)`.

 

# 12 Recommended R packages

`stringi` is of course required for this packages. Besides that, I
highly recommend using this R package alongside the 2 major
operator-related R-packages, namely `magrittr` and `zeallot`.

I also highly recommend the `fastverse` set of R packages
(<https://github.com/fastverse/fastverse>), which are a set of R
packages for (mostly) data wrangling, focused on high speed, better
memory management, and minimal dependencies.

For quick ’n easy back-tracing of errors, I recommend the `rlang` R
package.

 

# 13 Conclusion

I hope this R package will make your life a little bit easier, and your
R environment a little bit tinier.
