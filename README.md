
- <a href="#tidyoperators" id="toc-tidyoperators">tidyoperators</a>
  - <a href="#installation" id="toc-installation">Installation</a>
- <a href="#overview" id="toc-overview">Overview</a>
- <a href="#simple-additional-logicals"
  id="toc-simple-additional-logicals">Simple additional logicals</a>
- <a href="#safer-float-inequality-operators"
  id="toc-safer-float-inequality-operators">Safer float (in)equality
  operators</a>
- <a href="#in-place-modifying-mathematical-arithmetic"
  id="toc-in-place-modifying-mathematical-arithmetic">In-place modifying
  mathematical arithmetic</a>
  - <a href="#the-problem" id="toc-the-problem">The problem</a>
  - <a href="#the-solution-from-this-r-package"
    id="toc-the-solution-from-this-r-package">The solution from this R
    package</a>
- <a href="#unreal-replacement" id="toc-unreal-replacement">Unreal
  replacement</a>
- <a href="#missing-from-stringi-locate-ith-pattern"
  id="toc-missing-from-stringi-locate-ith-pattern">Missing from stringi:
  Locate <span
  class="math inline"><em>i</em><sup><em>t</em><em>h</em></sup></span>
  pattern</a>
- <a href="#substr---functions" id="toc-substr---functions">Substr -
  functions</a>
- <a href="#basic-string-operations"
  id="toc-basic-string-operations">Basic string operations</a>
  - <a href="#string-subsetting-operators"
    id="toc-string-subsetting-operators">String subsetting operators</a>
  - <a href="#string-arithmetic" id="toc-string-arithmetic">String
    arithmetic</a>
  - <a href="#pattern-attributes-in-strings"
    id="toc-pattern-attributes-in-strings">Pattern attributes in strings</a>
  - <a href="#pattern-attributes-examples"
    id="toc-pattern-attributes-examples">Pattern attributes examples</a>
  - <a href="#in-place-modifying-string-arithmetic-and-subsetting"
    id="toc-in-place-modifying-string-arithmetic-and-subsetting">In-place
    modifying string arithmetic and subsetting</a>
- <a href="#more-advanced-string-operations-with-s_strapply"
  id="toc-more-advanced-string-operations-with-s_strapply">More advanced
  string operations with s_strapply</a>
- <a href="#the-transform_if-function-and-the-subset_if-operators"
  id="toc-the-transform_if-function-and-the-subset_if-operators">The
  transform_if function, and the subset_if operators</a>
- <a href="#speed-efficiency-and-multi-threading"
  id="toc-speed-efficiency-and-multi-threading">Speed, efficiency, and
  multi-threading</a>
  - <a href="#multi-threading-in-string-subsetting-functions"
    id="toc-multi-threading-in-string-subsetting-functions">Multi-threading
    in string subsetting functions</a>
  - <a href="#multi-threading-in-s_strapply"
    id="toc-multi-threading-in-s_strapply">Multi-threading in s_strapply</a>
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
[![](https://img.shields.io/badge/ORCID-0000--0001--9498--8379-green.svg)](https://orcid.org/0000-0001-9498-8379)
<!-- badges: end -->

![](tidyoperators.svg)

The `tidyoperators` R-package adds some much needed infix operators, and
a few functions, to make your R code much more tidy. It includes infix
operators for the negation of logical operators (exclusive-or, not-and,
not-in), safer float (in)equality operators, in-place modifying
mathematical arithmetic, string arithmetic, string sub-setting, in-place
modifying string arithmetic, in-place modifying string sub-setting, and
in-place modifying unreal replacers. The ‘tidyoperators’ R-package also
adds the stri_locate_ith function, which is missing from stringi. And it
includes some helper functions for more complex string arithmetic. Most
stringi pattern expressions options (regex, fixed, coll, charclass) are
available for all string-pattern-related functions, when appropriate.
This package adds the transform_if function. This package also allows
integrating third-party parallel computing packages (like stringfish)
for some of its functions.

WARNING: This package is still very much experimental. Function names,
argument names, and so on may change dramatically. Use it for testing
only, until it’s stable.

CHNAGELOG (EXPERIMENTAL VERSION):

- 8 march 2023: `stringi` is now a dependency. Completely re-written the
  ReadMe file, Description, and documentation.
- 9 march 2023: added the “which”-operators.
- 10 march 2023: `s_strapply()` now uses `stringi`, and uses `apply()`
  instead of `sapply()`. Renamed the which operators `%[sp]%` and
  `%[!sp]%` to `%[grep]%` and `%[!grep]%` to make their meaning more
  obvious. Added this Change log to the ReadMe file.
- 11 march 2023: replaced the “which”-operators with the
  `transform_if()` function and the subset_if operators.
- 13 march 2023: changed the name and argument convention of many of the
  string related functions to be more consistent. Changed the return of
  non-matches in the substr\_-functions.

 

## Installation

You can install `tidyoperators` from github like so:

``` r
library(devtools)
devtools::install_github("https://github.com/tony-aw/tidyoperators")
```

then load it using:

``` r
library(tidyoperators)
```

an one can open the introduction page to the `tidyoperators` package
using:

``` r
tidyoperators_help()
```

# Overview

The `tidyoperators` R package adds the following functionality:

- Infix logical operators for exclusive-or, not-and, not-in,
  number-type, and string-type.
- Safer (in)equality operators for floating numbers.
- Infix operators for In-place modifiers for mathematical arithmetic.
- Infix operators for string arithmetic.
- Infix operators for string sub-setting.
- The fully vectorized sub-string functions.
- Infix operators for In-place modifying string arithmetic.
- Infix operators for In-place modifying string sub-setting.
- The in-place modifying unreal replacer operator.
- `stri_locate_ith`: The stringi R package has a “locate_all” function,
  but no “locate_ith” function. The tidyoperators package adds the
  `stri_locate_ith` function, which uses the same naming and argument
  convention as the rest of the stringi functions, thus keeping your
  code consistent.
- There are also some string helper functions, namely `s_pattern` and
  `s_strapply`.
- The `transform_if` function, and some related operators.
- The utility function `env_c`.
- Most stringi pattern expressions options (regex, fixed, coll,
  charclass) are available for all string-pattern-related functions,
  when appropriate.
- This R package has only one dependency: `stringi`. No other
  dependencies, as to avoid “dependency hell”.
- Although this package has no other dependencies, it allows
  multi-threading of functions (when appropriate) through third-party
  packages to improve efficiency.

I realize there are other R-packages that cover some of the above
functionalities. But I often experience that these R packages (or at
least those I know of) either do not cover all that I required, had some
inconsistencies, or suffered from some other significant drawbacks.
Hence this package was created.

Currently this R package is only available on GitHub.

I understand one may not want to go through this entire Read-Me without
knowing if the R package is worthy of your time. Therefore, allow me to
give you a quick glimpse of what is possible in this R package before
jumping into the details.

First, some in-place mathematical arithmetic with `tidyoperators`:

``` r
x <- 1:10
print(x)
#>  [1]  1  2  3  4  5  6  7  8  9 10
x %^ <-% 2 # is the same as x <- x^2
print(x)
#>  [1]   1   4   9  16  25  36  49  64  81 100
```

Second, some numeric and string sub-types checks:

``` r
n <- c(0:5, 0:-5, 0.1, -0.1, 0, 1, Inf, -Inf, NA, NaN)
n[n %=numtype% "B"]
#> [1] 0 1 0 0 1
n[n %=numtype% "N"]
#> [1] 0 1 2 3 4 5 0 0 1
n[n %=numtype% "I"]
#>  [1]  0  1  2  3  4  5  0 -1 -2 -3 -4 -5  0  1
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

And now some fun string manipulations using `tidyoperators`:

``` r
x <- c("Hello World", "Goodbye World")

# Capitalize ONLY the ODD indices of each string:
x <- c("Hello World", "Goodbye World")
s_strapply(x, fun=\(x){
  replace(x, seq(1, length(x), 2), toupper(x)[seq(1, length(x), 2)])
})
#> [1] "HeLlO WoRlD"   "GoOdByE WoRlD"

# Extract second-last vowel of every word of every string in a vector:
x <- c("Outrageous, egregious, preposterous!", "Pleasant evening everyone")
print(x)
#> [1] "Outrageous, egregious, preposterous!"
#> [2] "Pleasant evening everyone"
s_strapply(x, w=T, fun=\(x)na.omit(substr_extract(x, loc=stri_locate_ith(x, -2, regex="a|e|i|o|u", case_insensitive=TRUE))))
#> [1] "o o o" "a e o"
```

And some string arithmetic:

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

And string sub-setting:

``` r
x <- c("yeay nay or nothing to say", "Goodmorning, goodevening and goodnight",
       paste0(letters[1:13], collapse=""))
print(x)
#> [1] "yeay nay or nothing to say"            
#> [2] "Goodmorning, goodevening and goodnight"
#> [3] "abcdefghijklm"
loc <- stri_locate_ith(x, -2, regex="ay", case_insensitive=TRUE)
substr_extract(x, loc=loc) # extract second-last "ay" in each string
#> [1] "ay" NA   NA
substr_repl(x, "??", loc=loc) # replace second-last "ay" with "??"
#> [1] "yeay n?? or nothing to say"            
#> [2] "Goodmorning, goodevening and goodnight"
#> [3] "abcdefghijklm"
substr_chartr(x, loc=loc) # switch upper/lower case of second-last "ay"
#> [1] "yeay nAY or nothing to say"            
#> [2] "Goodmorning, goodevening and goodnight"
#> [3] "abcdefghijklm"
substr_addin(x, " ", "after", loc=loc) # add white space before second-last "ay"
#> [1] "yeay na  or nothing to say"            
#> [2] "Goodmorning, goodevening and goodnight"
#> [3] "abcdefghijklm"
substr_addin(x, " ", "before", loc=loc) # add white space after second-last "ay"
#> [1] "yeay n y or nothing to say"            
#> [2] "Goodmorning, goodevening and goodnight"
#> [3] "abcdefghijklm"
substr_arrange(x, "rev", loc=loc) # reverse second-last "ay"
#> [1] "yeay nya or nothing to say"            
#> [2] "Goodmorning, goodevening and goodnight"
#> [3] "abcdefghijklm"
```

If you’re still interested, I invite you to read the rest of this
Read-Me and perhaps try out the package yourself.

 

# Simple additional logicals

The tidyoperators package adds a few basic logical operators:

- `%xor%`: Exclusive OR
- `%n&%`: NOT AND (i.e. `(!x) & (!y)`)
- `%out%`: the opposite of `%in%` (i.e. `!x %in% y`)
- `%?=%`: checks if both `x` and `y` are unknown or unreal (NA, NaN,
  Inf, -Inf)
- `s %sgrepl% p` checks if pattern `p` (defined as either `regex`, or as
  a call from `s_pattern()`) appears in character vector `s` (info on
  the `s_pattern()` function can be found in the string section of this
  read-me)

Here are some examples:

``` r
x <- c(TRUE, FALSE, TRUE, FALSE, NA, FALSE, TRUE)
y <- c(FALSE, TRUE, TRUE, FALSE, NA, NA, NA)
cbind(x, y, "x %xor% y"=x %xor% y, "x %n&% y" = x %n&% y, "x %?=% y" = x %?=% y)
#>          x     y x %xor% y x %n&% y x %?=% y
#> [1,]  TRUE FALSE      TRUE    FALSE    FALSE
#> [2,] FALSE  TRUE      TRUE    FALSE    FALSE
#> [3,]  TRUE  TRUE     FALSE    FALSE    FALSE
#> [4,] FALSE FALSE     FALSE     TRUE    FALSE
#> [5,]    NA    NA        NA       NA     TRUE
#> [6,] FALSE    NA        NA       NA    FALSE
#> [7,]  TRUE    NA        NA    FALSE    FALSE

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
```

Although designed for objects (vectors, matrices, arrays) of class
`double` (floating numbers), these operators also work correctly for
integers. These operators do not work for non-numeric objects.

 

# In-place modifying mathematical arithmetic

## The problem

This R package includes infix operators for in-place modifying
mathematical arithmetic. But first: what is an in-place modifier?
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

 

## The solution from this R package

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

I realize there doesn’t really need to be a `%logb%` operator, but since
one was needed for the antilogarithm, I added the “regular” logarithm
also, for consistency. Notice that all in-place modifiers end with `<-%`
(notice the space). All infix operators that are in-place modifiers in
this R package end with `<-%`, so that it is clear what it does.

Lets look at the original problem:

``` r
mtcars$mpg[mtcars$cyl>6] <- mtcars$mpg[mtcars$cyl>6]^2
```

With `tidyoperators` one can now make this more tidy with the following:

``` r
mtcars$mpg[mtcars$cyl>6] %^ <-% 2
```

Much tidier, no?

 

This is not the first or only R package that incorporates in-place
modifiers. Most notably, the `inplace` R package is devoted entirely to
in-place modifying mathematical arithmetic. However, `inplace` has a
nasty side-effect:

If 2 R objects refer to the same values - let’s say `x = 3` and
`y = 3` - using an in-place modifier from the `inplace` package on `x`
will also change `y`. This can be very dangerous.

**The `tidyoperators` R package does not have this problem:** modifying
one object does not affect another object, even if they happen to have
the same value.

 

# Unreal replacement

Another operator added by `tidyoperators` is `x %unreal <-% y`, which
replaces all NA, NaN, Inf and -Inf in `x` with the value given in `y`.

It is the same as `x[is.na(x)|is.nan(x)|is.infinite(x)] <- y`.

 

# Missing from stringi: Locate $i^{th}$ pattern

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

Easy peasy lemon squeezy.

 

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
is not allowed though.

Thus, to get the **second** occurence of some patern, use `i=2`, and to
get the **second-last** occurrence, use `i=-2`.

What this function returns exactly depends on the `simplify` argument.

If `simplify=FALSE` (the default), it returns a returns a list, one
element for each string. Each list element consists of a matrix with 2
columns. The first column gives the start position of the $i^{th}$
occurence of pattern `p`. The second column gives the end position of
the $i^{th}$ occurence of pattern `p`. This list can be used in
`stringi` for pattern transformation.

If `simplify=TRUE` it returns a matrix with 3 columns:

- The first column gives the start position of the $i^{th}$ occurrence
  of pattern `p`.
- The second column gives the end position of the $i^{th}$ occurrence of
  pattern `p`.
- The third column gives the length of the position range of the
  $i^{th}$ occurrence of pattern `p`.

The `stri_locate_ith(x, i, ...)` function uses the exact same argument
and naming convention as `stringi`, to keep your code consistent. And
just like `stringi::stri_locate_all`, the `stri_locate_ith(x, i, ...)`
function is a vectorized function: `x` and `i` as well as the pattern
(`regex, fixed, coll, charclass`) can all be different-valued vectors.

Currently, `stri_locate_ith` internally uses `mapply` for the
vectorization. Once this R package is out of the `experimental` phase, I
plan to replace `mapply` with a `C++` loop. But rest assured:
`stri_locate_ith` is already quite fast.

 

Now back to the original problem.

So we previosuly transformed all vowels in the strings of a character
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

But to transform **only** the **second-last** occurence, one can now use
`stri_locate_ith()` in a very similar way as was done with
`stri_locate_all`:

``` r
x <- c("HELLO WORLD", "goodbye world")

loc <- stri_locate_ith(x, -2, regex="a|e|i|o|u", case_insensitive=TRUE, simplify = FALSE) # <- this line is the key-difference

extr <- stringi::stri_sub_all(x, from=loc)
repl <- lapply(extr, \(x)chartr(x=x, old = "a-zA-Z", new = "A-Za-z"))
stringi::stri_sub_all_replace(x, loc, replacement=repl)
#> [1] "HELLo WORLD"   "goodbyE world"
```

Notice that the code is virtually equivalent. We ONLY need to change the
locate function. Quite tidy, right?

The transformation given in the previous code used `lapply`. I’m sure we
want to stick to vectorized functions as much as possible. Therefore,
the `tidyoperators` package also adds several fully vectorized
`substr_`-functions, which are discussed in the very next section.

# Substr - functions

The `tidyoperators` R-package includes the following sub-string
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
- The `substr_arrange(x, arr, ...)` function sorts or reverse the
  sub-string at a position (range).

The “position” in the functions above can be specified either by giving
the result of the `stri_locate_ith()` function (see the previous
section) in argument `loc`, or by one can give manual numerical
specifications using the `start, stop` or `at` arguments.

For example:

``` r
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"

loc <- stri_locate_ith(x, -2, regex="a|e|i|o|u") # find second-last occurrence of pattern

substr_extract(x, loc=loc) # extract pattern
#> [1] "e" "o"
substr_extract(x, type="before", loc=loc) # extract string before pattern
#> [1] "abcd" "n"
substr_extract(x, type="after", loc=loc) # extract string after pattern
#> [1] "fghijklm"    "pqrstuvwxyz"
substr_repl(x, "??", loc=loc) # replace pattern with double question-mark
#> [1] "abcd??fghijklm" "n??pqrstuvwxyz"
substr_chartr(x, loc=loc) # transform case of pattern match
#> [1] "abcdEfghijklm" "nOpqrstuvwxyz"
substr_addin(x, " ", "after", loc=loc) # add white space before pattern
#> [1] "abcde fghijklm" "no pqrstuvwxyz"
substr_addin(x, " ", "before", loc=loc) # add white space after pattern
#> [1] "abcd efghijklm" "n opqrstuvwxyz"
```

Simple, right?

# Basic string operations

The `tidyoperators` R package implements operators for string arithmetic
and sub-setting, as well some of their in-place modifier equivalents.
For consistency, and to avoid masking other common operators, all
string-related operators start with `%s`, where the “s” stands for
“string”.

 

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

Thus `%sget%` “gets” or extracts the given number of characters from the
left and the right, and removes the rest. There is also `%strim%`, which
is the opposite: it trims away the number of characters from the left
and right as defined in `ss`, leaving you with whatever is left.

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

 

Another fun string operator is `x %ss%  s`. This essentially splits
character vector `x` into a vector containing only individual
characters; then this vector is subset-ted by the number given in `s`.
Honestly, I do not think one would need this often, but it can be handy
sometimes. For example:

``` r
x <- "Tom Marvolo Riddle"
toupper(x) %ss% c(14, 4, 6:5, 12, 10:11, 13, 15, 8:9, 17:16, 18, 3:2, 7, 1) |>
  paste0(collapse = "")
#> [1] "I AM LORDVOLDEMORT"
```

 

## String arithmetic

Certainly, the `tidyoperators` package is not the first R package to
introduce string arithmetic. But I do hope these operators have more
consistent naming and functionality.

The `tidyoperators` package adds 4 arithmetic operators:

- `x %s+% y` is the same as `paste0(x, y)`;
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
x <- c("Ha", "Ho", "Hi", "Hu", "He", "Ha") %s*% 10
c("hello world & goodbye world", "world domination!") %s/% "world"
#> [1] 2 1
```

The tidyoperators R package also includes several string sub-setting
functions.

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
argument/right-hand-side expression `p` in the relevant
functions/operators with a call from the `s_pattern()` function.

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
(i.e. functions surrounded by `%` signs).

 

## Pattern attributes examples

``` r
# VOWELS SUBSETTING ====

x <- c("yeay nay or nothing to say", "Goodmorning, goodevening and goodnight",
       paste0(letters[1:13], collapse=""))
print(x)
#> [1] "yeay nay or nothing to say"            
#> [2] "Goodmorning, goodevening and goodnight"
#> [3] "abcdefghijklm"
p <- s_pattern(regex = "a|e|i|o|u", case_insensitive=TRUE)

# PATTERN ARITHMETIC ====

x <- c("Hello world", "Goodbye world")
p <- s_pattern(regex=" world")
x %s-% p
#> [1] "Hello"   "Goodbye"


x <- c("Ha", "Ho", "Hi", "Hu", "He", "Ha") %s*% 10
p <- s_pattern(regex="Ha")
x %s/% p
#> [1] 10  0  0  0  0 10
```

And then some fixed expressions:

``` r
x <- c("yeay yeay yeay yeay", "nay nay nay nay")
p <- s_pattern(fixed = "ye")

x <- c("Hello world", "Goodbye world")
p <- s_pattern(fixed=" world")
x %s-% p
#> [1] "Hello"   "Goodbye"

x <- c("Ha", "Ho", "Hi", "Hu", "He", "Ha") %s*% 10
p <- s_pattern(coll="Ha")
x %s/% p
#> [1] 10  0  0  0  0 10
```

And so on. I’m sure you get the idea.

 

## In-place modifying string arithmetic and subsetting

With the exception of `%ss%`, all infix operators (notice: operators,
not functions) have their in-place modifying equivalent:

- `x %s+ <-% y` is the same as `x <- x %s+% y`
- `x %s- <-% p` is the same as `x <- x %s-% p`
- `x %s* <-% n` is the same as `x <- x %s*% n`
- `x %s/ <-% p` is the same as `x <- x %s/% p`
- `x %sget <-% ss` is the same as `x <- x %sget% ss`
- `x %strim <-% ss` is the same as `x <- x %strim% ss`

 

# More advanced string operations with s_strapply

The string arithmetic and sub-setting operators and functions given so
far - in combination with the string function already available in base
R and R packages such as `stringi` - can do a lot, but it’s not always
flexible enough. To add extra flexibility, there is also the
`s_strapply(x, fun, w=F, clp=NULL, ...)` function. This function applies
the following steps to every element (every string) of character vector
x:

1)  the string is split into a vector of single characters (`w=F`), or a
    vector of space-delimited words (`w=T`).
2)  the function `fun()` is applied to the vector from step 1.
3)  the result from step 2 is pasted together to form a single string
    element again, using `paste0(..., collapse=clp)`. By default,
    `clp=""` if `w=F`, and `clp=" "` if `w=T`.

In other words, this function turns every string in character vector `x`
into its own little vector, applies a function to this vector, and
pastes the returning vector together into a single string again.

This operator can be used in a very wide variety of ways.

One obvious use of this function is for re-arranging the characters or
words in every string in character vector `x` in some custom way, or
find the occurrence of the character on the alphabet:

``` r
x <- c("Hello World", "Goodbye World")

# randomly shuffle letters:
s_strapply(x, sample)
#> [1] "rlWHeodl lo"   "boyleGWdr doo"

# reverse word order:
s_strapply(x, rev, w=T)
#> [1] "World   Hello"   "World   Goodbye"

# find occurrence of characters on alphabet:
s_strapply(x, fun=\(x)match(tolower(x),letters), clp="; ")
#> [1] "8; 5; 12; 12; 15; NA; 23; 15; 18; 12; 4; NA; NA"
#> [2] "7; 15; 15; 4; 2; 25; 5; NA; 23; 15; 18; 12; 4"

# completely customized sorting of characters (first vowels, then the rest of the letters):
custom_order <- c("a", "e", "i", "o", "u", setdiff(letters, c("a", "e", "i", "o", "u")))
print(paste0(custom_order, collapse = ""))
#> [1] "aeioubcdfghjklmnpqrstvwxyz"
s_strapply(x, fun=\(x){
  rest <- setdiff(x = unique(x), y = custom_order)
  y <- factor(x = x, levels = c(custom_order, rest), ordered = TRUE)
  return(sort(y))
})
#> [1] "eoodlllrH W"   "eooobddlryG W"
```

Much tidier and quicker, right?

Lets try something else: capitalize characters but ONLY at certain
indices:

``` r
x <- c("Hello World", "Goodbye World")
# capitalize odd indices:
s_strapply(x, fun=\(x){
  replace(x, seq(1, length(x), 2), toupper(x)[seq(1, length(x), 2)])
})
#> [1] "HeLlO WoRlD"   "GoOdByE WoRlD"

# capitalize random letters:
s_strapply(x, fun=\(x){
  ind <- sample(1:length(x), size=floor(length(x)/2))
  replace(x, ind, toupper(x[ind]))
})
#> [1] "HElLO WoRLd"   "GoOdbYE WORLd"

# capitalize only first word:
s_strapply(x, fun=\(x){replace(x, 1, toupper(x[1]))}, w=T)
#> [1] "HELLO   World"   "GOODBYE   World"
```

Lets try to take the second-last vowel of every word of every string in
some character vector `x`:

``` r
x <- c("Outrageous, egregious, preposterous!", "Pleasant evening everyone")
print(x)
#> [1] "Outrageous, egregious, preposterous!"
#> [2] "Pleasant evening everyone"
p <- s_pattern(regex="a|e|i|o|u", case_insensitive = TRUE)
s_strapply(x, w=T, fun=\(x) na.omit(substr_extract(x, loc=stri_locate_ith(x, -2, regex=p))))
#> [1] "o o"   "a e o"
```

 

# The transform_if function, and the subset_if operators

The final category of operators in this package are the subset_if
operators, and the `transform_if()` function.

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
which will tidy this up. This function is internally defined in only
vectorized functions, and without loops or apply-like functions, and is
therefore quite fast.

The above code can now be re-written as:

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

 

# Speed, efficiency, and multi-threading

This section discusses speed and optimization in the `tidyoperators`
package.

## Multi-threading in string subsetting functions

All the string sub-setting functions have the `fish` argument, which is
`FALSE` by default. If `fish=TRUE`, these functions will use
`stringfish` functions instead of base R functions, which are faster,
and also allow native multi-threading. Note that `stringfish` must be
installed in order for this to work. And `stringfish` needs to be loaded
also if you wish to also use multi-threading. Multi-threading in
`stringfish` can be set-up by running
`setoption(stringfish.nthreads=cl)` somewhere at the start of your code,
where `cl` is the number of threads you want to use.

Don’t use multi-threading unless you need to, as multi-threading has
some overhead, thus its only faster with very large character strings.

## Multi-threading in s_strapply

The `s_strapply` function uses `apply` internally. Just like in
`stri_locate_ith`, the user can multi-thread this function by replacing
apply:

``` r
x <- rep(c("Hello World", "Goodbye World"), 2e5)

s_strapply(x, sort) # regular way

require(future.apply)
plan(multisession)
s_strapply(x, sort, custom_apply = future_apply) # multi-threaded way
```

Now you have a multi-threaded version of `s_strapply`.

If you combine `s_strapply` with another multi-threaded function, I
advice the user to only make `s_strapply` multi-threaded, and not the
function used in the `fun` argument; making multiple layers of parallel
computing seems like asking for problems.

 

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
