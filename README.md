
- <a href="#tidyoperators" id="toc-tidyoperators">tidyoperators</a>
  - <a href="#installation" id="toc-installation">Installation</a>
- <a href="#overview" id="toc-overview">Overview</a>
- <a href="#simple-additional-logic-operators"
  id="toc-simple-additional-logic-operators">Simple additional logic
  operators</a>
- <a href="#safer-float-inequality-operators"
  id="toc-safer-float-inequality-operators">Safer float (in)equality
  operators</a>
- <a href="#in-place-modifying-mathematical-arithmetic"
  id="toc-in-place-modifying-mathematical-arithmetic">In-place modifying
  mathematical arithmetic</a>
- <a href="#unreal-replacement" id="toc-unreal-replacement">Unreal
  replacement</a>
- <a href="#the-transform_if-function-and-the-subset_if-operators"
  id="toc-the-transform_if-function-and-the-subset_if-operators">The
  transform_if function, and the subset_if operators</a>
- <a href="#matrix-rank-based-re-ordering-infix-operators"
  id="toc-matrix-rank-based-re-ordering-infix-operators">Matrix rank-based
  re-ordering infix operators</a>
- <a href="#additional-stringi-functions"
  id="toc-additional-stringi-functions">Additional stringi functions</a>
  - <a href="#matrix-joining" id="toc-matrix-joining">Matrix joining</a>
  - <a href="#locate-ith-pattern-for-stringi"
    id="toc-locate-ith-pattern-for-stringi">Locate <span
    class="math inline"><em>i</em><sup><em>t</em><em>h</em></sup></span>
    pattern for stringi</a>
- <a href="#substr---functions" id="toc-substr---functions">Substr -
  functions</a>
- <a href="#basic-string-infix-operators"
  id="toc-basic-string-infix-operators">Basic string infix operators</a>
  - <a href="#string-subsetting-operators"
    id="toc-string-subsetting-operators">String subsetting operators</a>
  - <a href="#string-arithmetic" id="toc-string-arithmetic">String
    arithmetic</a>
  - <a href="#pattern-attributes-in-strings"
    id="toc-pattern-attributes-in-strings">Pattern attributes in strings</a>
  - <a href="#pattern-attributes-examples"
    id="toc-pattern-attributes-examples">Pattern attributes examples</a>
  - <a href="#in-place-modifying-string-arithmetic-and-sub-setting"
    id="toc-in-place-modifying-string-arithmetic-and-sub-setting">In-place
    modifying string arithmetic and sub-setting</a>
- <a href="#package-import-management-operator-and-function"
  id="toc-package-import-management-operator-and-function">Package import
  management operator and function</a>
  - <a href="#import-operator" id="toc-import-operator">Import operator</a>
  - <a href="#import_data" id="toc-import_data">import_data</a>
- <a href="#speed-and-multi-threading"
  id="toc-speed-and-multi-threading">Speed and multi-threading</a>
- <a href="#recommended-r-packages"
  id="toc-recommended-r-packages">Recommended R packages</a>
- <a href="#compatibility-with-other-operator-related-r-packages"
  id="toc-compatibility-with-other-operator-related-r-packages">Compatibility
  with other operator-related R packages</a>
- <a href="#conclusion" id="toc-conclusion">Conclusion</a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyoperators

<!-- badges: start -->

[![R build
status](https://github.com/tony-aw/tidyoperators/workflows/R-CMD-check/badge.svg)](https://github.com/tony-aw/tidyoperators/actions)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CodeFactor](https://www.codefactor.io/repository/github/tony-aw/tidyoperators/badge)](https://www.codefactor.io/repository/github/tony-aw/tidyoperators)
[![](https://img.shields.io/badge/ORCID-0000--0001--9498--8379-green.svg)](https://orcid.org/0000-0001-9498-8379)
<!-- badges: end -->

![](tidyoperators.svg)

The ‘tidyoperators’ R-package adds some much needed infix operators, and
a few functions, to make your R code much more tidy. It includes infix
operators for the negation of logical operators (exclusive-or, not-and,
not-in), safer float (in)equality operators, in-place modifying
mathematical arithmetic, string arithmetic, string sub-setting, in-place
modifying string arithmetic, in-place modifying string sub-setting,
in-place modifying unreal replacers, and infix operators for custom row-
and column-wise rank-based ordering of matrices. The ‘tidyoperators’
R-package also adds the stringi-like stri_locate_ith and stri_join_mat
functions. It also adds string functions to replace, extract, add-on,
transform, and re-arrange, the ith pattern occurrence or position. Most
stringi pattern expressions options are available for the
string-pattern-related functions, when appropriate. This package adds
the transform_if function. This package also allows integrating
third-party parallel computing packages (like stringfish) for some of
its functions.

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

FUTURE PLANS:

I believe `tidyoperators` is slowly getting closer to becoming stable.
There does not appear a need to add/remove many more
functions/operators, although some functions, operators or arguments may
need to be tweaked and/or optimized. Once I am fully satisfied with the
R package (which may take a while, as I am a bit of a perfectionist), I
may attempt to publish this R package to CRAN.

 

## Installation

You can install `tidyoperators` from github like so:

``` r
remotes::install_github("https://github.com/tony-aw/tidyoperators")
```

then load it using:

``` r
library(tidyoperators)
```

and one can open the introduction page to the `tidyoperators` package
using:

``` r
tidyoperators_help()
```

# Overview

The `tidyoperators` R package adds the following functionality:

- Infix logical operators for exclusive-or, not-and, not-in, `grepl`,
  number-type, and string-type.
- Safer (in)equality operators for floating numbers.
- Infix operators for In-place modifiers for mathematical arithmetic.
- Infix operators for string arithmetic, string sub-setting, and their
  in-place modifying equivalents.
- An in-place modifying unreal replacer operator.
- The tidyoperators package adds additional `stringi` functions, namely
  `stri_locate_ith()` and `stri_join_mat()` (and aliases). These
  functions use the same naming and argument convention as the rest of
  the `stringi` functions, thus keeping your code consistent.
- The fully vectorized sub-string functions, that extract, replace,
  add-in, transform, or re-arrange, the $i^{th}$ pattern occurrence or
  substring.
- The `s_pattern()` helper function for the string infix operators.
- The `transform_if()` function, and some related operators.
- The `%m import <-%` operator for advanced multi-package aliasing.
- The `stringi` pattern expressions options are available for all
  string-pattern-related functions, when appropriate.
- This R package has only one dependency: `stringi`. No other
  dependencies, as to avoid “dependency hell”.
- Although this package has no other dependencies, it allows
  multi-threading of functions (when appropriate) via `stringfish`.

Currently this R package is only available on GitHub.

I understand one may not want to go through this entire Read-Me without
knowing if the R package is worthy of your time. Therefore, allow me to
give you a quick glimpse of what is possible in this R package before
jumping into the details.

 

In-place mathematical arithmetic with `tidyoperators`:

``` r
x <- 1:10
print(x)
#>  [1]  1  2  3  4  5  6  7  8  9 10
x %^ <-% 2 # is the same as x <- x^2
print(x)
#>  [1]   1   4   9  16  25  36  49  64  81 100
```

Simple transformations:

``` r
# in base R:
very_long_name_1[very_long_name_1 > 0] <- log(very_long_name_1[very_long_name_1 > 0])

# with tidyoperators:
very_long_name_1 %<>% transform_if(\(x)x>0, log)
```

Float equality checks:

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

Locate $i^{th}$ occurrence of some pattern in a string:

``` r
x <- c("Goodmorning -- GOODafternoon -- GooDevening, and goodnight!",
       paste0(letters[1:13], collapse=""))
print(x)
#> [1] "Goodmorning -- GOODafternoon -- GooDevening, and goodnight!"
#> [2] "abcdefghijklm"
loc <- stri_locate_ith(
  # locate second-last occurrence of "good" (ignore case) of each string in x:
  x, -2, regex="good", case_insensitive=TRUE, simplify = TRUE
)
substr(x, loc[,1], loc[,2])
#> [1] "GooD" NA
```

String arithmetic:

``` r
"Hello" %s+% " world"
#> [1] "Hello world"
c("Hello world", "Goodbye world") %s-% " world"
#> [1] "Hello"   "Goodbye"
c("Ha", "Ho", "Hi", "Hu", "He", "Ha") %s*% 10
#> [1] "HaHaHaHaHaHaHaHaHaHa" "HoHoHoHoHoHoHoHoHoHo" "HiHiHiHiHiHiHiHiHiHi"
#> [4] "HuHuHuHuHuHuHuHuHuHu" "HeHeHeHeHeHeHeHeHeHe" "HaHaHaHaHaHaHaHaHaHa"
c("HaHa", "Ho", "Hi", "Hu", "He", "Ha") %s/% "Ha"
#> [1] 2 0 0 0 0 1
```

String re-ordering:

``` r
# sorting words:
x <- c("Hello everyone, I'm here", "Goodbye everyone")
print(x)
#> [1] "Hello everyone, I'm here" "Goodbye everyone"
mat <- stringi::stri_split_boundaries(
  x, simplify = TRUE, type="word" # vector to matrix
)
rank <- stringi::stri_rank(as.vector(mat)) |>  matrix(ncol=ncol(mat))
sorted <- mat %row~% rank # rank based matrix re-ordering
stri_c_mat(sorted, margin=1, sep=" ") # row-wise concatenate strings
#> [1] "      , everyone Hello here I'm" "       everyone Goodbye"
```

If you’re still interested, I invite you to read the rest of this
Read-Me and perhaps try out the package yourself.

 

# Simple additional logic operators

The tidyoperators package adds a few basic logic operators:

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

- “\~0”: zero, or else a number whose absolute value is smaller than the
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

 

# Safer float (in)equality operators

This package adds the `%f==%, %f!=% %f<%, %f>%, %f<=%, %f>=%` operators,
which perform “float logic”. They are virtually equivalent to the
regular (in)equality operators, `==, !=, <, >, <=, >=`, except for one
aspect. The float logic operators assume that if the absolute difference
between `x` and `y` is smaller than the Machine tolerance,
`sqrt(.Machine$double.eps)`, then `x` and `y` ought to be consider to be
equal. Thus these provide safer float (in)equality operators. For
example: `(0.1*7) == 0.7` returns `FALSE`, even though they are equal,
due to the way floating numbers are stored in programming languages like
R. But `(0.1*7) %f==% 0.7` returns `TRUE`.

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

 

# In-place modifying mathematical arithmetic

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

This is better, but still not truly tidy.

This R package solves the above laid-out problem by implementing
in-place modifying mathematical arithmetic for all mathematical
operators, excluding matrix operators.

Here is a list of all in-place mathematical modifiers implemented in
this R-package:

- `x %+ <-% y` is the same as`x <- x + y`;
- `x %- <-% y` is the same as`x <- x - y`;
- `x %* <-% y` is the same as`x <- x * y`;
- `x %/ <-% y` is the same as`x <- x / y`;
- `x %^ <-% p` is the same as`x <- x^p`;
- `x %rt <-% p` is the same as`x <- x^(1/p)`;
- `x %logb <-% b` is the same as`x <- log(x, base=b)`;
- `x %alogb <-% b` is the same as`x <- b^x`; if `b=exp(1)`, this is the
  same as`x <- exp(x)`;
- `x %alogb <-% exp(1)` is the same as exp(x)\`.

For consistency, all infix operators that are in-place modifiers in this
R package end with `<-%` (notice the space).

Lets look at the original problem:

``` r
mtcars$mpg[mtcars$cyl>6] <- mtcars$mpg[mtcars$cyl>6]^2
```

With `tidyoperators` one can now make this more tidy with the following:

``` r
mtcars$mpg[mtcars$cyl>6] %^ <-% 2
```

Much tidier, right?

 

# Unreal replacement

Another operator added by `tidyoperators` is `x %unreal <-% y`, which
replaces all NA, NaN, Inf and -Inf in `x` with the value given in `y`.

So `x %unreal <-% y` is the same as
`x[is.na(x)|is.nan(x)|is.infinite(x)] <- y`.

 

# The transform_if function, and the subset_if operators

Then we have the subset_if operators, and the `transform_if()` function.

Consider the following code:

`x[cond(x)] <- f(x[cond(x)])`

Here a conditional subset of the object `x` is transformed with function
`f`, where the condition is using a function referring to `x` itself (in
this case `cond(x)`). Consequently, reference to `x` is written **four
times**! If the object has a short name like “x, this doesn’t matter too
much. But if the object has a longer name like `very_long_name_1`, doing
something like this:

``` r
very_long_name_1[very_long_name_1 > 0] <- log(very_long_name_1[very_long_name_1 > 0])
```

becomes cumbersome, and not so tidy.

The tidyoperators package therefore adds the `transform_if()` function
which will tidy this up. The above code can now be re-written as:

``` r
very_long_name_1 |> transform_if(\(x)x>0, log)
```

Much tidyr, right?

Besides `transform_if`, the tidyoperators package also adds 2
“subset_if” operators:

- The `x %[if]% cond` operator selects elements from vector/matrix/array
  `x`, for which the result of `cond(x)` returns `TRUE`.

- The `x %[!if]% cond` operator selects elements from
  vector/matrix/array `x`, for which the result of `cond(x)` returns
  `FALSE`.

Here are a few examples to see these in action:

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
object_with_very_long_name |> transform_if(\(x)x>0, log)
#>       [,1]      [,2]
#>  [1,]  -10 0.0000000
#>  [2,]   -9 0.0000000
#>  [3,]   -8 0.6931472
#>  [4,]   -7 1.0986123
#>  [5,]   -6 1.3862944
#>  [6,]   -5 1.6094379
#>  [7,]   -4 1.7917595
#>  [8,]   -3 1.9459101
#>  [9,]   -2 2.0794415
#> [10,]   -1 2.1972246

object_with_very_long_name %[if]% \(x)x %in% 1:10
#> [1] 1 2 3 4 5 6 7 8 9
object_with_very_long_name %[!if]% \(x)x %in% 1:10
#>  [1] -10  -9  -8  -7  -6  -5  -4  -3  -2  -1   0
```

Note that this function returns object `x`, to modify `x` directly, one
still has to assign it. To keep your code tidy, consider combining this
function with `magrittr`’s in-place modifying piper-operator (`%<>%`).
I.e.:

`very_long_name_1 %<>% transform_if(cond, trans)`

 

# Matrix rank-based re-ordering infix operators

The `tidyoperators` R package adds 2 additional matrix operators:

- The `x %row~% rank` operator re-orders the elements within every row
  of matrix `x` by the rank given in matrix `rank`.
- The `x %col~% rank` operator re-orders the elements within every
  column of matrix `x` by the rank given in matrix `rank`.

If matrix `x` is a numeric matrix, and one wants to order the elements
of every row or column numerically, `x %row~% x` or `x %col~% x` would
suffice, respectively.

If matrix `x` is not numeric, `x %row~% x` and `x %col~% x`are still
possible, but probably not the best option. In the non-numeric case,
providing a ranking matrix would be better.

If `rank` is a non-repeating sample of random integers
(i.e. `sample(1:length(x), replace=FALSE)`), `x %row~% rank` will
randomly shuffle the elements of every row, where the shuffling order of
every row is independent of the other rows. Similarly, `x %col~% rank`
will randomly shuffle the elements of every column, where the shuffling
order of every column is independent of the other columns

 

Examples with a numeric matrix:

``` r
mat <- matrix(sample(1:25), nrow=5)
print(mat)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25   11   16    9    8
#> [2,]    4   14   10   15   13
#> [3,]    7   18    6   12   21
#> [4,]    1   22   19   17    3
#> [5,]    2    5   23   20   24
mat %row~% mat # sort elements of every row
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    8    9   11   16   25
#> [2,]    4   10   13   14   15
#> [3,]    6    7   12   18   21
#> [4,]    1    3   17   19   22
#> [5,]    2    5   20   23   24
mat %row~% -mat # reverse-sort elements of every row
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25   16   11    9    8
#> [2,]   15   14   13   10    4
#> [3,]   21   18   12    7    6
#> [4,]   22   19   17    3    1
#> [5,]   24   23   20    5    2
mat %col~% mat # sort elements of every column
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    5    6    9    3
#> [2,]    2   11   10   12    8
#> [3,]    4   14   16   15   13
#> [4,]    7   18   19   17   21
#> [5,]   25   22   23   20   24
mat %col~% -mat # reverse-sort elements of every column
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25   22   23   20   24
#> [2,]    7   18   19   17   21
#> [3,]    4   14   16   15   13
#> [4,]    2   11   10   12    8
#> [5,]    1    5    6    9    3

mat <- matrix(sample(1:25), nrow=5)
print(mat)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25    3   23    9   11
#> [2,]   12    6    4    7   21
#> [3,]   15   10   24   16    2
#> [4,]    1   18   14   22    8
#> [5,]   20   19   17   13    5
rank <- sample(1:length(mat)) |> matrix(ncol=ncol(mat)) # randomized rank
mat %row~%  rank# random shuffle every row independent of other rows
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   23   11    9    3   25
#> [2,]   12    4    6    7   21
#> [3,]    2   15   24   16   10
#> [4,]    1   14    8   18   22
#> [5,]   19   20   17    5   13
mat %col~% rank # random shuffle every column independent of other columns
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   20   19   23   16   11
#> [2,]   15    3   17    9    2
#> [3,]   12    6   24   13    5
#> [4,]    1   10    4    7    8
#> [5,]   25   18   14   22   21
```

Examples with a character matrix:

``` r
mat <- matrix(sample(letters, 25), nrow=5)
print(mat)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "w"  "l"  "s"  "q"  "e" 
#> [2,] "n"  "p"  "i"  "b"  "j" 
#> [3,] "t"  "a"  "z"  "r"  "k" 
#> [4,] "g"  "v"  "y"  "d"  "x" 
#> [5,] "m"  "f"  "c"  "u"  "o"
rank <- stringi::stri_rank(as.vector(mat)) |> matrix(ncol=ncol(mat))
mat %row~% rank # sort elements of every row
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "e"  "l"  "q"  "s"  "w" 
#> [2,] "b"  "i"  "j"  "n"  "p" 
#> [3,] "a"  "k"  "r"  "t"  "z" 
#> [4,] "d"  "g"  "v"  "x"  "y" 
#> [5,] "c"  "f"  "m"  "o"  "u"
mat %row~% -rank # reverse-sort elements of every row
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "w"  "s"  "q"  "l"  "e" 
#> [2,] "p"  "n"  "j"  "i"  "b" 
#> [3,] "z"  "t"  "r"  "k"  "a" 
#> [4,] "y"  "x"  "v"  "g"  "d" 
#> [5,] "u"  "o"  "m"  "f"  "c"
mat %col~% rank # sort elements of every column
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "g"  "a"  "c"  "b"  "e" 
#> [2,] "m"  "f"  "i"  "d"  "j" 
#> [3,] "n"  "l"  "s"  "q"  "k" 
#> [4,] "t"  "p"  "y"  "r"  "o" 
#> [5,] "w"  "v"  "z"  "u"  "x"
mat %col~% -rank # reverse-sort elements of every column
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "w"  "v"  "z"  "u"  "x" 
#> [2,] "t"  "p"  "y"  "r"  "o" 
#> [3,] "n"  "l"  "s"  "q"  "k" 
#> [4,] "m"  "f"  "i"  "d"  "j" 
#> [5,] "g"  "a"  "c"  "b"  "e"

mat <- matrix(sample(letters, 25), nrow=5)
print(mat)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "z"  "u"  "c"  "f"  "s" 
#> [2,] "o"  "l"  "w"  "m"  "r" 
#> [3,] "x"  "g"  "v"  "d"  "n" 
#> [4,] "j"  "h"  "k"  "t"  "b" 
#> [5,] "p"  "a"  "q"  "e"  "y"
rank <- sample(1:length(mat)) |> matrix(ncol=ncol(mat)) # randomized rank
mat %row~%  rank# random shuffle every row independent of other rows
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "z"  "f"  "s"  "u"  "c" 
#> [2,] "r"  "l"  "w"  "o"  "m" 
#> [3,] "n"  "x"  "d"  "v"  "g" 
#> [4,] "k"  "b"  "j"  "h"  "t" 
#> [5,] "a"  "p"  "y"  "e"  "q"
mat %col~% rank # random shuffle every column independent of other columns
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

 

# Additional stringi functions

## Matrix joining

The `tidyoperators` package adds a tiny additional function to
`stringi`:

`stri_join_mat` (and their aliases `stri_c_mat` and `stri_paste_mat`).

As the name suggests, these functions perform row-wise (`margin=1`; the
default) or column-wise (`margin=2`) joining of a matrix of strings,
thereby transforming it to a vector of strings. You can do this already
in base R, but it requires converting the matrix to a data.frame or
list, and then calling `stri_join` inside `do.call()`, which to me just
seems too much trouble for something *soooo* abysmally simple.

Here a practical example of their usage when re-ordering strings, words,
or sentences :

``` r
# sorting characters in strings:
x <- c("Hello world", "Goodbye world")
print(x)
#> [1] "Hello world"   "Goodbye world"
mat <- stringi::stri_split_boundaries(x, simplify = TRUE, type="character")
rank <- stringi::stri_rank(as.vector(mat)) |>  matrix(ncol=ncol(mat))
sorted <- mat %row~% rank # <- fast sort matrix row-wise
print(sorted)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#> [1,] ""   ""   " "  "d"  "e"  "H"  "l"  "l"  "l"  "o"   "o"   "r"   "w"  
#> [2,] " "  "b"  "d"  "d"  "e"  "G"  "l"  "o"  "o"  "o"   "r"   "w"   "y"
stri_join_mat(sorted, margin=1, sep="") # <- using new function here
#> [1] " deHllloorw"   " bddeGlooorwy"

# sorting words:
x <- c("Hello everyone", "Goodbye everyone")
print(x)
#> [1] "Hello everyone"   "Goodbye everyone"
mat <- stringi::stri_split_boundaries(x, simplify = TRUE, type="word")
rank <- stringi::stri_rank(as.vector(mat)) |>  matrix(ncol=ncol(mat))
sorted <- mat %row~% rank # <- fast sort matrix row-wise
print(sorted)
#>      [,1] [,2]       [,3]     
#> [1,] " "  "everyone" "Hello"  
#> [2,] " "  "everyone" "Goodbye"
stri_c_mat(sorted, margin=1, sep=" ") # <- alias for stri_join_mat
#> [1] "  everyone Hello"   "  everyone Goodbye"

# randomly shuffle sentences:
x <- c("Hello, who are you? Oh, really?! Cool!", "I don't care. But I really don't.")
print(x)
#> [1] "Hello, who are you? Oh, really?! Cool!"
#> [2] "I don't care. But I really don't."
mat <- stringi::stri_split_boundaries(x, simplify = TRUE, type="sentence")
rank <- sample(1:length(mat)) |> matrix(ncol=ncol(mat))
shuffled <- mat %row~% rank # <- fast sort matrix row-wise
print(shuffled)
#>      [,1]                   [,2]             [,3]                 
#> [1,] "Hello, who are you? " "Oh, really?! "  "Cool!"              
#> [2,] ""                     "I don't care. " "But I really don't."
stri_paste_mat(shuffled, margin=1, sep=" ") # <- another alias for stri_join_mat
#> [1] "Hello, who are you?  Oh, really?!  Cool!"
#> [2] " I don't care.  But I really don't."
```

 

## Locate $i^{th}$ pattern for stringi

Suppose one wants to transform all vowels in the strings of a character
vector `x` such that all upper case vowels become lower case, and
vice-versa. One can do that completely in `stringi` + base R as follows:

``` r

x <- c("HELLO WORLD", "goodbye world")
loc <- stringi::stri_locate_all(x, regex="a|e|i|o|u", case_insensitive=TRUE)
extr <- stringi::stri_sub_all(x, from=loc)
repl <- lapply(extr, \(x)chartr(x=x, old = "a-zA-Z", new = "A-Za-z"))
stringi::stri_sub_all_replace(x, loc, replacement=repl)
#> [1] "HeLLo WoRLD"   "gOOdbyE wOrld"
```

But now suppose one wants to transform **only** the **second-last**
vowel. How are you going to do that? It’s not impossible, but also not
super straight-forward. For a tidy code, `stringi` really needs some
kind of “stri_locate_ith” function. And, of course, the `tidyoperators`
package provides just that.

The `stri_locate_ith(x, i, ...)` function locates for every
element/string in character vector `x`, the $i^{th}$ occurrence of some
(regex/fixed/etc) pattern. When `i` is positive, the occurrence is
counted from left to right. Negative values for `i` are also allowed, in
which case the occurrence is counted from the right to left. But `i=0`
is not allowed though. Thus, to get the **second** occurrence of some
pattern, use `i=2`, and to get the **second-last** occurrence, use
`i=-2`.

What this function returns exactly depends on the `simplify` argument.

If `simplify=FALSE` (the default), it returns a returns a list, one
element for each string. Each list element consists of a matrix with 2
columns. The first column gives the start position of the $i^{th}$
occurrence of a pattern. The second column gives the end position of the
$i^{th}$ occurrence of a pattern. This list can be used in `stringi` for
pattern transformation.

If `simplify=TRUE` it returns a matrix with 3 columns: start position,
end position, and length of position range of the $i^{th}$ occurrence of
a pattern. One row for each string of character vector `x`x.

The `stri_locate_ith(x, i, ...)` function uses the exact same argument
and naming convention as `stringi`, to keep your code consistent. And
just like `stringi::stri_locate_all`, the `stri_locate_ith(x, i, ...)`
function is a vectorized function: `x` and `i` as well as the pattern
(`regex, fixed, coll, charclass`) can all be different-valued vectors.

 

Now back to the original problem.

So we previously transformed all vowels in the strings of a character
vector `x` such that all upper case vowels become lower case, and
vice-versa, like so:

``` r
x <- c("HELLO WORLD", "goodbye world")

loc <- stringi::stri_locate_all(x, regex="a|e|i|o|u", case_insensitive=TRUE)

extr <- stringi::stri_sub_all(x, from=loc)
repl <- lapply(extr, \(x)chartr(x=x, old = "a-zA-Z", new = "A-Za-z"))
stringi::stri_sub_all_replace(x, loc, replacement=repl)
#> [1] "HeLLo WoRLD"   "gOOdbyE wOrld"
```

This transforms all occurrences.

But to transform **only** the **second-last** occurrence, one can now
use `stri_locate_ith()` in a very similar way as was done with
`stri_locate_all`:

``` r
x <- c("HELLO WORLD", "goodbye world")

loc <- stri_locate_ith( # this part is the key-difference
  x, -2, regex="a|e|i|o|u", case_insensitive=TRUE, simplify = FALSE 
)

extr <- stringi::stri_sub_all(x, from=loc)
repl <- lapply(extr, \(x)chartr(x=x, old = "a-zA-Z", new = "A-Za-z"))
stringi::stri_sub_all_replace(x, loc, replacement=repl)
#> [1] "HELLo WORLD"   "goodbyE world"
```

Notice that the code is virtually equivalent. We ONLY need to change the
locate function.

The transformation given in the previous code used `lapply`. I’m sure we
want to stick to vectorized functions as much as possible. Therefore,
the `tidyoperators` package also adds several fully vectorized
`substr_`-functions, which are discussed in the very next section.

# Substr - functions

The `tidyoperators` R-package includes the following fully vectorized
sub-string functions:

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

Example:

``` r

x <- c("Goodmorning -- GOODafternoon -- Goodevening, and goodnight!",
       paste0(letters[1:13], collapse=""))
print(x)
#> [1] "Goodmorning -- GOODafternoon -- Goodevening, and goodnight!"
#> [2] "abcdefghijklm"
loc <- stri_locate_ith(
  # locate second-last occurrence of "good" (ignore case) of each string in x:
  x, -2, regex="good", case_insensitive=TRUE 
)
substr_extract(x, loc=loc) # extract second-last "good"
#> [1] "Good" NA
substr_extract(x, "before", loc=loc) # extract string before second-last "good"
#> [1] "Goodmorning -- GOODafternoon -- " NA
substr_extract(x, "after", loc=loc) # extract string before second-last "good"
#> [1] "evening, and goodnight!" NA
substr_repl(x, "??", loc=loc) # replace second-last "good" with "??"
#> [1] "Goodmorning -- GOODafternoon -- ??evening, and goodnight!"
#> [2] "abcdefghijklm"
substr_chartr(x, loc=loc) # switch upper/lower case of second-last "good"
#> [1] "Goodmorning -- GOODafternoon -- gOODevening, and goodnight!"
#> [2] "abcdefghijklm"
substr_addin(x, " ", "after", loc=loc) # add white space after second-last "good"
#> [1] "Goodmorning -- GOODafternoon -- Good evening, and goodnight!"
#> [2] "abcdefghijklm"
substr_addin(x, " ", "before", loc=loc) # add white space before second-last "good"
#> [1] "Goodmorning -- GOODafternoon --  Goodevening, and goodnight!"
#> [2] "abcdefghijklm"
substr_arrange(x, loc=loc) # sort second-last "good"
#> [1] "Goodmorning -- GOODafternoon -- dGooevening, and goodnight!"
#> [2] "abcdefghijklm"
substr_arrange(x, "decr", loc=loc) # reverse-sort second-last "good"
#> [1] "Goodmorning -- GOODafternoon -- ooGdevening, and goodnight!"
#> [2] "abcdefghijklm"
substr_arrange(x, "rev", loc=loc) # reverse second-last "good"
#> [1] "Goodmorning -- GOODafternoon -- dooGevening, and goodnight!"
#> [2] "abcdefghijklm"
substr_arrange(x, "rand", loc=loc) # randomly shuffle characters of second-last "good"
#> [1] "Goodmorning -- GOODafternoon -- oGodevening, and goodnight!"
#> [2] "abcdefghijklm"
```

Simple, right?

 

# Basic string infix operators

The `tidyoperators` R package implements infix operators for string
arithmetic and sub-setting, as well some of their in-place modifier
equivalents. For consistency, and to avoid masking other common
operators, all string-related operators start with `%s`, where the “s”
stands for “string”.

 

## String subsetting operators

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

 

## String arithmetic

The `tidyoperators` package adds 4 arithmetic operators:

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

It is important to note that the right-side arguments `p`, `y`, and `n`
can be a single value, or a vector of the same length as `x`.

 

## Pattern attributes in strings

The `x %s-% p` and `x %s/% p` operators (and their in-place equivalents,
given later), and the `%sgrep%` operator perform pattern matching for
various purposes. By default the pattern matching is interpreted as
case-sensitive `regex` patterns from `stringi`.

But, of course, sometimes one wants to change this. For example, one may
want it to be case insensitive. Or perhaps one wants to use fixed
expressions, or something else.

The `tidyoperators` package provides options for these cases. To use
more refined pattern definition, simply replace the
argument/right-hand-side expression `p` in the relevant operators with a
call from the `s_pattern()` function.

The `s_pattern()` function uses the exact same argument convention as
`stringi`. For example:

- `s_pattern(regex=p, case_insensitive=FALSE, ...)`
- `s_pattern(fixed=p, ...)`
- `s_pattern(coll=p, ...)`
- `s_pattern(boundary=p, ...)`
- `s_pattern(charclass=p, ...)`

For consistency with base R and with packages such as `stringr`, one can
also fill in `ignore.case=TRUE` or `ignore_case=TRUE` instead of
`case_insensitive=TRUE`, and `s_pattern()` will still understand that.

Note that the `s_pattern()` function only works for the infix operators
(i.e. operator-functions surrounded by `%` signs).

 

## Pattern attributes examples

Regular expressions:

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

Fixed expressions:

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

 

## In-place modifying string arithmetic and sub-setting

With the exception of `%ss%`, all infix operators for string arithmetic
and string sub-setting have their in-place modifying equivalent:

- `x %s+ <-% y` is the same as `x <- x %s+% y`
- `x %s- <-% p` is the same as `x <- x %s-% p`
- `x %s* <-% n` is the same as `x <- x %s*% n`
- `x %s/ <-% p` is the same as `x <- x %s/% p`
- `x %sget <-% ss` is the same as `x <- x %sget% ss`
- `x %strim <-% ss` is the same as `x <- x %strim% ss`

 

# Package import management operator and function

## Import operator

And finally, as the last functionality in the `tidyoperators` package we
have the in-place operator `%m import <-%`. This operator requires a
little bit more advanced knowledge of R.

One can import a package and assign an alias to it in base R using:

``` r
alias <- loadNamespace("packagename")
```

Using a package alias, instead of using `library` or `require` has
obvious benefits (i.e. prevent overlapping namespaces, clarify which
function came from which package).

It does, however, have a drawback: you cannot easily import multiple
packages under the same alias (actually, it is very possible, but it
requires converting environments to lists, and that will make your code
unnecessarily ugly). There are a couple of situations where importing
multiple packages into a single alias might be preferable:

- suppose package `B` is supposed to overwrite a couple of functions
  from package `A` (for example if package `B` fixes the functionality
  from a function in package `A`). In that case you want to import
  package `A`, and then overwrite it with package `B`.
- if multiple packages kind of “belong” together, you may not want to
  give these packages separate aliases.

One example is the core `fastverse` + `tidyverse` combo: `data.table` +
`collapse` + `tidytable`. Considering the large amount of functions
these packages have, some which unfortunately have the same name as base
R functions, one would want to assign them in an alias object. But
giving them separate aliases is perhaps undesirable: `tidytable` is
supposed to overwrite some of the `data.table` functions, and one is
probably always going to use these 3 packages together.

This is where the `%m import <-%` operator comes in. It imports multiple
packages under the same alias, and also informs the user which package
will overwrite which function, so you will never be surprised. The
`%m import <-%` operator also only loads exported functions (unlike
`loadNamespace()`, which loads both internal and external functions).
This is, I think, more desirable, as internal function should remain,
you know, internal.

The `%m import <-%` operator will give a `warning` when the user
attempts to import more than 3 packages under the same alias.

Now, as an example, lets load `data.table`, and then `collapse`, and
then `tidytable`, all under the same alias, which I will call “ftv” (for
“fast-tidy-verse”):

``` r
ftv %m import <-% c("data.table", "collapse", "tidytable")
#> Importing package: data.table...
#> 
#> Importing package: collapse...
#> no conflicts
#> 
#> Importing package: tidytable...
#> The following conflicting objects detected:
#>  
#> %chin%, %like%, last, fread, setDTthreads, data.table, first, getDTthreads, fwrite, %between%, between
#>  
#> tidytable will overwrite conflicting objects from previous imported packages...
#> 
#> Done
#> You can now access the functions using ftv$...
#> methods will work like normally.
```

Notice that the messages explain which package will overwrite what.

Now you can of course use those loaded packages as one would normally do
when using a package alias.

It’s better not to use this method for packages that mainly implement
operators (such as this very R package), as using something like this:

``` r
to %m import <-% "tidyoperators"
to$`%row~%`(mat, rank)
```

is very cumbersome. The `%m import <-%` operator will therefore also
notify the user when attempting to import an R-package where more than
half of the functions are operators.

 

## import_data

The `%m import <-%` imports everything from the package namespace. But
packages often also have data sets, which are often not part of the
namespace.

The `data()` function in core R can already load data from packages, but
this function loads the data into the global environment, instead of
returning the data directly, making assigning the data to a specific
variable a bit annoying. Therefore, the `tidyoperators` package
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

That’s it.

 

# Speed and multi-threading

All the string sub-setting functions have the `fish` argument, which is
`FALSE` by default. If `fish=TRUE`, these functions will use
`stringfish` functions instead of base R and `stringi` functions. The
stringfish functions are usually faster, and also allow native
multi-threading. Note that `stringfish` must be installed in order for
this to work. And `stringfish` needs to be loaded also if you wish to
also use multi-threading. Multi-threading in `stringfish` can be set-up
by running `setoption(stringfish.nthreads=cl)` somewhere at the start of
your code, where `cl` is the number of threads you want to use.

Don’t use multi-threading unless you need to, as multi-threading has
some overhead, thus its only faster with very large character strings.

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
#> [1] "Goodmorning--odGoevening, and Goodnight"
#> [2] "abcdefghijklm"
```

 

# Recommended R packages

`stringi` is of course required for this packages. Besides that, I
highly recommend using this R package alongside the 2 major
operator-related R-packages, namely `magrittr` and `zeallot`.  

 

# Compatibility with other operator-related R packages

The `stringi` R package has the `%s+%` and `%s*%` operators. They do
virtually the same things as in `tidyoperators`, and so the masking of
these functions can safely be ignored. I also made sure not to name any
of the operators in `tidyoperators` the same as the operators in
`magrittr` and `zeallot`, so that should be safe also.

 

# Conclusion

I hope this R package will make your life a little bit tidyr.
