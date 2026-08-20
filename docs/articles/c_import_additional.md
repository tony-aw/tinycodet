# Import system - additional details

``` r
library(tinycodet)
#> Run `?tinycodet::tinycodet` to open the introduction help page of 'tinycodet'.
```

 

## Introduction

The previous article, “Import system - main functions”, discussed the
main functions of the import system. Please read that article first
before reading this article.

 

## Miscellaneous import\_ - functions

Beside the main `import_` functions
([`import_as()`](https://tony-aw.github.io/tinycodet/reference/import_as.md),
[`import_inops()`](https://tony-aw.github.io/tinycodet/reference/import_inops.md),
and
[`import_data()`](https://tony-aw.github.io/tinycodet/reference/import_data.md)),
there are 2 miscellaneous `import_` functions:
[`import_LL()`](https://tony-aw.github.io/tinycodet/reference/import_misc.md)
and
[`import_int()`](https://tony-aw.github.io/tinycodet/reference/import_misc.md).

 

The
[`import_LL()`](https://tony-aw.github.io/tinycodet/reference/import_misc.md)
function places specific functions from a package in the current
environment, and also locks the specified functions to prevent
modification. The primary use-case for this function is for exposing
functions inside a local environment. (“LL” stands for “Local &
Locked”.)

Example:

``` r
rm(list = ls())
import_LL("tidytable", "across")
#> exposing and locking functions to current environment ...
#> Done
ls() # notice `accross` is now exposed in this environment
#> [1] "across"
```

 

The
[`import_int()`](https://tony-aw.github.io/tinycodet/reference/import_misc.md)
function directly returns an internal function from a package. It is
similar to the `:::` operator, but with 2 key differences:

1.  [`import_int()`](https://tony-aw.github.io/tinycodet/reference/import_misc.md)
    includes the `lib.loc` argument.
2.  [`import_int()`](https://tony-aw.github.io/tinycodet/reference/import_misc.md)
    only searches internal functions, not exported ones. This makes it
    clearer in your code that you’re using an internal function, instead
    of making it ambiguous.

The main argument is the `form` argument, which is a 2-sided formula
with one term on each side, the left term giving the package name and
the right term giving the name of the internal function.

Example:

``` r
# Using through re-assignment:
fun <- import_int(tinycodet ~ .internal_paste, .libPaths())
fun("hello", "world")
#> [1] "helloworld"

# Or using directly:
import_int(
  tinycodet ~ .internal_paste, .libPaths()
)("hello", "world")
#> [1] "helloworld"
```

 

## S3 methods: they just work

When importing packages with `tinycodet`’ import system, S3 methods will
work just fine. For example, the S3 method ”
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) ” works with
objects from the `mgcv` R package, even when imported in an alias:

``` r
import_as(~ mgcv., "mgcv") |> suppressMessages()
d <- import_data("gamair", "chicago")

# just a random model for the sake of demonstration:
model <- mgcv.$gam(death ~ s(o3median), data=d)
isS3method(f="plot", class="gam") # this is an S3 method
#> [1] TRUE
plot(model)
```

![](c_import_additional_files/figure-html/unnamed-chunk-4-1.png)

Also, S3 methods defined in the package will automatically be
registered, and thus automatically work. For example, the following code
just works:

``` r
import_as(~ dpr., "dplyr") |> suppressMessages()
import_inops("magrittr") |> suppressMessages()
d <- import_data("dplyr", "starwars")
d <- d %>% dpr.$group_by(species)

isS3method(f="arrange", class="data.frame", envir = dpr.) # this is an S3 method
#> [1] TRUE
isS3method(f="relocate", class="data.frame", envir = dpr.) # this is an S3 method
#> [1] TRUE
# this works:
d %>%
  dpr.$arrange(dpr.$desc(mass)) %>%
  dpr.$relocate(species, mass)
#> # A tibble: 87 × 14
#> # Groups:   species [38]
#>    species    mass name  height hair_color skin_color eye_color birth_year sex  
#>    <chr>     <dbl> <chr>  <int> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Hutt       1358 Jabb…    175 NA         green-tan… orange         600   herm…
#>  2 Kaleesh     159 Grie…    216 none       brown, wh… green, y…       NA   male 
#>  3 Droid       140 IG-88    200 none       metal      red             15   none 
#>  4 Human       136 Dart…    202 none       white      yellow          41.9 male 
#>  5 Wookiee     136 Tarf…    234 brown      brown      blue            NA   male 
#>  6 Human       120 Owen…    178 brown, gr… light      blue            52   male 
#>  7 Trandosh…   113 Bossk    190 none       green      red             53   male 
#>  8 Wookiee     112 Chew…    228 brown      unknown    blue           200   male 
#>  9 NA          110 Jek …    180 brown      fair       blue            NA   NA   
#> 10 Besalisk    102 Dext…    198 none       brown      yellow          NA   male 
#> # ℹ 77 more rows
#> # ℹ 5 more variables: gender <chr>, homeworld <chr>, films <list>,
#> #   vehicles <list>, starships <list>
```

So when importing packages, everything works as expected, including S3
methods.

 

## Alias attributes

One may have noticed in the “Import system - main functions” article,
that aliasing a package like so:

``` r
import_as(~ tdt., "tidytable", re_exports = TRUE, dependencies = "data.table")
#> Importing packages and registering methods...
#> Done
#> You can now access the functions using `tdt.$`
#> For conflicts report, packages order, and other attributes, run `attr.import(tdt.)`
```

… produces several messages, including the message “For conflicts
report, packages order, and other attributes, run
[`attr.import()`](https://tony-aw.github.io/tinycodet/reference/import_helper.md)”.

The
[`attr.import()`](https://tony-aw.github.io/tinycodet/reference/import_helper.md)
function allows the user to access the special attributes stored and
locked inside the alias object. These attributes show which imported
package overwrites which imported functions, in what order the packages
are imported and so on.

Here are some examples.

Show the packages imported under the alias, and in which order the
packages are imported, and from which packages the re-exported functions
came from:

``` r
attr.import(tdt., "pkgs")
#> $packages_order
#> [1] "data.table" "tidytable" 
#> 
#> $main_package
#> [1] "tidytable"
#> 
#> $re_exports.pkgs
#> [1] "data.table" "rlang"      "tidyselect" "magrittr"   "pillar"
```

Show which functions from which packages “win” conflicts:

``` r
attr.import(tdt., "conflicts")|> knitr::kable()
```

| package | winning_conflicts |
|:---|:---|
| data.table |  |
| tidytable + re-exports | last, first, between, %notin%, fread, setDTthreads, fwrite, getDTthreads, data.table, %chin%, %between%, %like% |

Show the arguments used in the
[`import_as()`](https://tony-aw.github.io/tinycodet/reference/import_as.md)
call that produced the alias object in question:

``` r
attr.import(tdt., "args")
#> $main_package
#> [1] "tidytable"
#> 
#> $re_exports
#> [1] TRUE
#> 
#> $dependencies
#> [1] "data.table"
#> 
#> $extensions
#> NULL
#> 
#> $lib.loc
#> [1] "C:/Users/Tony/AppData/Local/Temp/Rtmp04kXaJ/temp_libpath628440ee60d1"
#> [2] "D:/Programs/R/R-4.6.1/library"                                       
#> 
#> $import_order
#> [1] "dependencies" "main_package" "extensions"
```

The help file on
[`attr.import()`](https://tony-aw.github.io/tinycodet/reference/import_helper.md)
provides more details on each of these options.

 

## Check for package versions mismatch

The
[`pversion_check4mismatch()`](https://tony-aw.github.io/tinycodet/reference/pversion.md)
function checks if there is any mismatch between the currently loaded
packages and the packages in the specified library path.

The
[`pversion_report()`](https://tony-aw.github.io/tinycodet/reference/pversion.md)
function gives a table of all specified packages, with their loaded and
installed versions, regardless if there is a mismatch or not.

 

## help.import

The
[`help.import()`](https://tony-aw.github.io/tinycodet/reference/import_helper.md)
function gets the help file for a function `i` (or topic string `i`),
even if the function is inside an alias object, or if the function is an
unattached function (like exposed infix operators).

Example:

``` r
import_as(~ mr., "magrittr")
import_inops(mr.)

help.import(i=mr.$add)
help.import(i=`%>%`)
help.import(i="add", alias=mr.)
```

 

## Miscellaneous comments on package imports

The [magrittr](https://github.com/tidyverse/magrittr) and
[rlang](https://github.com/r-lib/rlang) packages add “pronouns” to R:
`.`, `. data`, `.env`. Fret not, for pronouns work regardless if you
attached a package or not. And you don’t need to use something like
[`rlang::.data`](https://rlang.r-lib.org/reference/dot-data.html) or
`rlang.$.data` for a pronoun to work. They just work.

 

There are some additional miscellaneous functions related to the package
import system that should perhaps be mentioned also:

- the
  [`pkg_get_deps()`](https://tony-aw.github.io/tinycodet/reference/pkgs.md)
  function gets the dependencies (or the enhances) of a package,
  regardless if the package is CRAN or non-CRAN. See the help file for
  details.
- the `pkgs %installed in% lib.loc` operator checks if the packages
  specified in character vector `pkgs` are installed in library paths
  `lib.loc`, and does this without attaching or even loading the
  packages.

 
