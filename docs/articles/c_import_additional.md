# Import system - additional details

``` r
library(tinycodet)
#> Run `?tinycodet::tinycodet` to open the introduction help page of 'tinycodet'.
```

 

## Introduction

The previous article, “Import system - main functions”, discussed the
main functions of the import system. Please read that article first
before reading this article.

 

 

## S3/S4 methods: they just work

When importing packages with `tinycodet`’ import system, S3 and S4
methods will work just fine.

For example, the following code with S3 just works:

``` r
import_as(.dpr ~ dplyr)
#> Import & method registration complete
import_from("magrittr", import_ls("magrittr", "infix"))
#> c("%!>%", "%$%", "%<>%", "%>%", "%T>%")
#> Import & method registration complete
d <- import_data("dplyr", "starwars")
d <- d %>% .dpr$group_by(species)

isS3method(f="arrange", class="data.frame", envir = .dpr) # this is an S3 method
#> [1] TRUE
isS3method(f="relocate", class="data.frame", envir = .dpr) # this is an S3 method
#> [1] TRUE
# this works:
d %>%
  .dpr$arrange(.dpr$desc(mass)) %>%
  .dpr$relocate(species, mass)
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

and so do these S4 methods:

``` r
import_from("broadcast", "bc.d")
#> Import & method registration complete

bc.d(1:10, array(1:10, c(1,10)), "+") # fast broadcasted addition
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#>  [1,]    2    3    4    5    6    7    8    9   10    11
#>  [2,]    3    4    5    6    7    8    9   10   11    12
#>  [3,]    4    5    6    7    8    9   10   11   12    13
#>  [4,]    5    6    7    8    9   10   11   12   13    14
#>  [5,]    6    7    8    9   10   11   12   13   14    15
#>  [6,]    7    8    9   10   11   12   13   14   15    16
#>  [7,]    8    9   10   11   12   13   14   15   16    17
#>  [8,]    9   10   11   12   13   14   15   16   17    18
#>  [9,]   10   11   12   13   14   15   16   17   18    19
#> [10,]   11   12   13   14   15   16   17   18   19    20
```

So when importing packages, everything works as expected, including S3
and S4 methods.

 

## Alias attributes

The
[`attr.import()`](https://tony-aw.github.io/tinycodet/reference/tinyimport_alias.md)
function allows the user to access the special attributes stored and
locked inside the alias object. These attributes show which imported
package overwrites which imported functions, in what order the packages
are imported and so on.

Here are some examples.

Show the packages imported under the alias, and in which order the
packages are imported, and from which packages the re-exported functions
came from:

``` r
import_as(.tdt ~ tidytable, deps = "data.table")
#> Import & method registration complete

attr.import(.tdt, "pkgs")
#> $packages_order
#> [1] "data.table" "tidytable" 
#> 
#> $main_package
#> [1] "tidytable"
#> 
#> $re_exports.pkgs
#> NULL
```

Show which functions from which packages “win” conflicts:

``` r
attr.import(.tdt, "conflicts")|> knitr::kable()
```

| package | winning_conflicts |
|:---|:---|
| data.table |  |
| tidytable + re-exports | last, first, between, %notin%, fread, setDTthreads, fwrite, getDTthreads, data.table, %chin%, %between%, %like% |

The help file on
[`attr.import()`](https://tony-aw.github.io/tinycodet/reference/tinyimport_alias.md)
provides more details on each of these options.

 

## help.import

The
[`help.import()`](https://tony-aw.github.io/tinycodet/reference/import_helper.md)
function gets the help file for a function `i` (or topic string `i`),
even if the function is inside an alias object, or if the function is an
unattached function (like exposed infix operators).

Example:

``` r
import_as(.dpr ~ "dplyr")
import_from("magrittr", import_ls("magrittr", "infix"))

help.import(i = .mr$add)
help.import(i = `%>%`)
help.import(i = "add", alias = .mr)
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

 
