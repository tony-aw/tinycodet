
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tinyoperations

<!-- badges: start -->

[![R build
status](https://github.com/tony-aw/tinyoperations/workflows/R-CMD-check/badge.svg)](https://github.com/tony-aw/tinyoperations/actions)
[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/ORCID-0000--0001--9498--8379-green.svg)](https://orcid.org/0000-0001-9498-8379)
[![](https://img.shields.io/badge/Site%20Pages-tony--aw.github.io/tinyoperations-purple.svg)](https://tony-aw.github.io/tinyoperations)
<!-- badges: end -->

![](man/figures/tinyoperations.svg)  

## Description & Overview

The `tinyoperations` R-package adds some infix operators, and a few
functions. It primarily focuses on 4 things:

1)  Safer decimal number (“double”) truth testing.
2)  Extending the string manipulation capabilities of the `stringi` R
    package.
3)  Reducing repetitive code.
4)  A new package and module import system, that combines the benefits
    of aliasing a package with the benefits of attaching a package.

The `tinyoperations` R-package has only one dependency, namely
`stringi`. Most functions in this R-package are fully vectorized and
have been optimized for optimal speed and performance.

 

WARNING: This package is still very much experimental. Function names,
argument names, and so on may change dramatically. Use it for testing
only, until it’s stable.

Currently this R package is only available on GitHub.

 

Even though this is a relatively small R package, I do understand you
may not want to go through all the articles and help files of
`tinyoperations` without knowing if the R package is even worthy of your
time. Therefore, allow me to give you a quick glimpse of what is
possible in this R package before jumping into the details.

 

Safer decimal number truth testing:

``` r
x <- c(0.3, 0.6, 0.7)
y <- c(0.1*3, 0.1*6, 0.1*7)
print(x); print(y)
#> [1] 0.3 0.6 0.7
#> [1] 0.3 0.6 0.7

x == y # gives FALSE, but should be TRUE
#> [1] FALSE FALSE FALSE

x %d==% y # here it's done correctly
#> [1] TRUE TRUE TRUE
```

 

Operators and functions to reduce repetitive code:

``` r
# in base R:
very_long_name_1 <- ifelse(# repetitive, and gives unnecessary warning
  very_long_name_1 > 0, log(very_long_name_1), very_long_name_1^2
)
mtcars$mpg[mtcars$cyl>6] <- (mtcars$mpg[mtcars$cyl>6])^2

# with tinyoperations:
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

 

String re-ordering via matrix re-ordering:

``` r
# sorting words:
x <- c("Hello everyone, I'm here", "Goodbye everyone")
x <- strcut_brk(x, "word")
mat <- stringi::stri_rank(as.vector(x)) |> matrix(ncol=ncol(x))
sorted <- x %row~% mat
stri_c_mat(sorted, margin=1, sep=" ")
#> [1] "      , everyone Hello here I'm" "       everyone Goodbye"
```

 

Using `tinyoperations's` new import system; note that the following code
is run **without attaching a single R package** (besides
`tinyoperations` itself of course):

``` r
import_as( # loading "tidytable" + its foreign exports + "data.table" under alias "tdt." 
  tdt., "tidytable", dependencies = "data.table"
)
#> Importing packages...
#> Done
#> You can now access the functions using `tdt.$`.
#> (S3)methods will work like normally. 
#> For conflicts report and package order, see `attributes(tdt.)`.
attributes(tdt.)$conflicts |> knitr::kable()
```

| package                     | winning_conflicts                                                                                      |
|:----------------------------|:-------------------------------------------------------------------------------------------------------|
| data.table                  |                                                                                                        |
| tidytable + foreign exports | first, last, between, fread, %chin%, getDTthreads, data.table, %between%, setDTthreads, fwrite, %like% |

``` r

import_inops("magrittr") # exposing operators from `magrrittr` to current env
#> Getting infix operators from package: magrittr...
#> 
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done

d <- import_data("starwars", "dplyr") # directly assigning the "starwars" dataset to object "d"

d %>% tdt.$filter(species == "Droid") %>%
  tdt.$select(name, tdt.$ends_with("color"))
#> # A tidytable: 6 × 4
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
#> You can now access the sourced objects using `myalias.$`.
myalias.$helloworld() # run a function that was just sourced.
#> [1] "hello world"
```

 

If you’re still interested, I invite you to read the articles on the
website (<https://tony-aw.github.io/tinyoperations/>), and perhaps try
out the package yourself.

 

## Installation

You can install `tinyoperations` from github like so:

``` r
remotes::install_github("https://github.com/tony-aw/tinyoperations")
```

You can attach the package - thus exposing its functions to your
namespace - using:

``` r
library(tinyoperations)
```

and one can open the introduction page to the `tinyoperations` package
using:

``` r
tinyoperations::tinyoperations_help()
```

 

# Recommended R packages

`stringi` is of course required for this packages. Besides that, I
highly recommend using this R package alongside the 2 major
operator-related R-packages, namely `magrittr` and `zeallot`.

I also highly recommend the `fastverse` set of R packages
(<https://github.com/fastverse/fastverse>), which are a set of R
packages for (mostly) data wrangling, focused on high speed, better
memory management, and minimal dependencies.

For quick ’n easy back-tracing of errors, I recommend the `rlang` R
package.

 

# Compatibility with other R packages

The `stringi` R package has the `%s+%` and `%s*%` operators. They do
exactly the same things as in `tinyoperations`, and so the masking of
these functions can safely be ignored. I also made sure not to name any
of the operators in `tinyoperations` the same as the operators in
`magrittr` and `zeallot`, so that should be safe also.

 

The `import` R package provides somewhat similar capabilities to the
`tinyoperations` import management system, but they are still quite
different. The `tinyoperations`’ import system focuses more on exposing
infix operators to the current environment, loading a package + its
foreign exports in an alias, and allowing multiple related packages to
be loaded under the same alias. The `import` package does not really
provide these functionality directly (as far as I know). Strictly
speaking there is no incompatibility between `tinyoperations` and
`import`. You can safely use both implementations of the import system,
if you wish.

 

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
`tinyoperations`, and things like `for(... in ...)library(...)` or
`if(...)library(...)`.

 

One may have noticed that some of the functionality in this R packages
has been inspired by other programming languages, in particular by
Python (please, refrain from taking out your torches pitchforks!). This
is not the first R package to take inspiration from Python. Therefore,
one may find some overlapping function and operator names in other
packages that do something similar; so be mindful of that.

 

# Changelog and status

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
- 3 July 2023: a tiny update; just fixed some minor documentation
  issues.
- 9 July 2023: MAJOR UPDATE + RENAMED PACKAGE TO `tinyoperations`.
  Details:… Replaced the `%::?%` operator with the `help.import()`
  function. The `import_as()` function now also adds attributes to the
  alias. Renamed the `import_lsf()` function to `pkg_lsf()`. Renamed the
  `pkgs_get_deps` function to `pkg_get_deps`, as this functions only
  accepts a single package anyway, and also partially re-wrote the
  function to give more consistently ordered character vectors. Added
  the `overwrite` and `inherits` arguments to `import_inops()`. The
  argument `dependencies` in `import_as()` now only accepts an explicit
  character vector as input, as using `TRUE` may result in the ambiguous
  load-order of the dependencies. Each of the `import_` functions now
  have their own separate help page, and also added the
  `tinyoperations_import` page, to keep it more organized. Moreover,
  Added more tests for the `import_` and `source_module` functions.
  Added the `misc` - functions. Replaced all mentions of “float” with
  “decimal”, and replaced all float truth testing (`%f`) to be decimal
  number truth testing as (`%d`). Removed the Natural number option from
  the logical operators. Adjusted the documentation in accordance with
  the aforementioned changes, improved the lay-out of the documentation,
  and clarified the documentation even more.
- 11 July 2023: last update unintentionally created a bug; bug now
  fixed. Added some minor features (see Details). Details: Last update
  unintentionally created a bug; bug now fixed. Also added the `verbose`
  argument to `import_as`, as the function now gives info on conflicts
  and so on in its attributes. Created some fake packages JUST for the
  sake of thorough testing. The tests are not part of R CMD CHECK
  (because it doesn’t like packages defined inside package folder), and
  not uploaded here on GitHub; but I do perform the test with fake
  packages everytime I update this R package
- 22 July 2023: UPDATED TO R4.3.0. Simplified the internal code of the
  `%installed in%` operator. Removed the `substr_` - functions, and
  subsequently also the `stringfish` functionality: They were originally
  created for historical reasons that are no longer relevant
  (`stri_locate_ith` used to output a list; now it outputs a matrix),
  and the stringfish functions had some unwanted behaviour, and working
  around these unwanted behaviours turned out to be more trouble than
  was worth. Added the `strcut_` - functions. The `import_as()` and
  `import_inops()` functions now create LOCKED objects, for additional
  safety (more information can be found on this by running
  `import_lock()`). Added the `delete` argument to the `import_inops()`
  function. The `import_inops()` function now allows almost complete
  freedom in choosing which packages to expose infix operators from.
  Added the safer atomic type casting functions (`as_bool()`,
  `as_int()`, `as_dbl()`, `as_chr()`). The `transform_if()` function now
  also has the `other` and `text` arguments. Some minor optimization
  improvements for some of the functions.Adjusted the documentation in
  accordance with all these changes. Fixed some minor spelling and
  grammar errors in the documentation. Created a GitHub site using
  `pkgdown`, but it’s not published yet.
- 25 July 2023: Small update: Added the `repor_inops()` function,
  clarified the documentation at `import_inops()`, and split the import
  article into 2.

FUTURE PLANS:

I believe `tinyoperations` is slowly getting closer to becoming stable.
There does not appear a need to add/remove many more
functions/operators, although some functions, operators or arguments may
need to be tweaked and/or optimized. Once I am fully satisfied with the
R package (which may take a while, as I am a bit of a perfectionist), I
may attempt to publish this R package to CRAN.

 
