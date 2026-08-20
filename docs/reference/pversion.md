# Check for Package Versions Mismatch

The `pversion_check4mismatch()` function checks if there is any mismatch
between the currently loaded packages and the packages in the specified
library path.  
  
The `pversion_report()` function gives a table of all specified
packages, with their loaded and installed versions, regardless if there
is a mismatch or not.

## Usage

``` r
pversion_check4mismatch(pkgs = NULL, lib.loc = .libPaths())

pversion_report(pkgs = NULL, lib.loc = .libPaths())
```

## Arguments

- pkgs:

  a character vector with the package name(s).  
  Packages that are not actually loaded will be ignored.  
  Base/core R will also be ignored.  
  If `NULL`, all loaded packages (see
  [loadedNamespaces](https://rdrr.io/r/base/ns-load.html)) excluding
  core/base R will be checked.

- lib.loc:

  character vector specifying library search path (the location of R
  library trees to search through).  
  The `lib.loc` argument would usually be
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).  
  See also [loadNamespace](https://rdrr.io/r/base/ns-load.html).

## Value

For `pversion_check4mismatch()`:  
If no mismatch between loaded versions and those in `lib.loc` were
found, returns `NULL`.  
Otherwise it returns a `data.frame`, with the loaded version and library
version of the specified packages.  
  
For `pversion_report()`:  
Returns a `data.frame`, with the loaded version and library version of
the specified packages, as well as a logical column indicating whether
the two versions are equal (`TRUE`), or not equal (`FALSE`).  
  

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)

## Examples

``` r
"dplyr" %installed in%  .libPaths()
#> dplyr 
#>  TRUE 

import_as(~dpr., "dplyr")
#> Importing packages and registering methods...
#> Done
#> You can now access the functions using `dpr.$`
#> For conflicts report, packages order, and other attributes, run `attr.import(dpr.)` 
pversion_check4mismatch()
#> NULL
pversion_report()
#>         package version_loaded version_lib.loc versions_equal
#> 1     powerjoin          0.1.0           0.1.0           TRUE
#> 2      rappdirs          0.3.3           0.3.3           TRUE
#> 3          sass          0.4.9           0.4.9           TRUE
#> 4          utf8          1.2.4           1.2.4           TRUE
#> 5      generics          0.1.3           0.1.3           TRUE
#> 6         tidyr          1.3.2           1.3.2           TRUE
#> 7          xml2          1.3.6           1.3.6           TRUE
#> 8       stringi          1.8.4           1.8.4           TRUE
#> 9        digest         0.6.37          0.6.37           TRUE
#> 10     magrittr          2.0.3           2.0.3           TRUE
#> 11 RColorBrewer          1.1-3           1.1.3           TRUE
#> 12     evaluate          1.0.5           1.0.5           TRUE
#> 13      fastmap          1.2.0           1.2.0           TRUE
#> 14     jsonlite          2.0.0           2.0.0           TRUE
#> 15      whisker          0.4.1           0.4.1           TRUE
#> 16        purrr          1.2.2           1.2.2           TRUE
#> 17        fansi          1.0.7           1.0.7           TRUE
#> 18       scales          1.4.0           1.4.0           TRUE
#> 19        httr2          1.1.0           1.1.0           TRUE
#> 20  textshaping          1.0.5           1.0.5           TRUE
#> 21    jquerylib          0.1.4           0.1.4           TRUE
#> 22          cli          3.6.6           3.6.6           TRUE
#> 23        rlang          1.3.0           1.3.0           TRUE
#> 24      remotes          2.5.0           2.5.0           TRUE
#> 25        withr          3.0.3           3.0.3           TRUE
#> 26       cachem          1.1.0           1.1.0           TRUE
#> 27         yaml         2.3.12          2.3.12           TRUE
#> 28      memoise          2.0.1           2.0.1           TRUE
#> 29        dplyr          1.2.1           1.2.1           TRUE
#> 30      ggplot2          4.0.2           4.0.2           TRUE
#> 31        vctrs          0.7.3           0.7.3           TRUE
#> 32           R6          2.6.1           2.6.1           TRUE
#> 33    lifecycle          1.0.5           1.0.5           TRUE
#> 34           fs          2.1.0           2.1.0           TRUE
#> 35  htmlwidgets          1.6.4           1.6.4           TRUE
#> 36         ragg          1.5.2           1.5.2           TRUE
#> 37  fontawesome          0.5.3           0.5.3           TRUE
#> 38    pkgconfig          2.0.3           2.0.3           TRUE
#> 39         desc          1.4.3           1.4.3           TRUE
#> 40    tinycodet          0.7.1           0.7.1           TRUE
#> 41       gtable          0.3.6           0.3.6           TRUE
#> 42      pkgdown          2.2.1           2.2.1           TRUE
#> 43       pillar         1.11.1          1.11.1           TRUE
#> 44        bslib          0.9.0           0.9.0           TRUE
#> 45         glue          1.8.0           1.8.0           TRUE
#> 46   data.table         1.18.4          1.18.4           TRUE
#> 47         Rcpp          1.1.2           1.1.2           TRUE
#> 48  systemfonts          1.3.2           1.3.2           TRUE
#> 49     collapse          2.1.7           2.1.7           TRUE
#> 50         xfun           0.51            0.51           TRUE
#> 51       tibble          3.2.1           3.2.1           TRUE
#> 52   tidyselect          1.2.1           1.2.1           TRUE
#> 53   rstudioapi         0.17.1          0.17.1           TRUE
#> 54        knitr           1.49            1.49           TRUE
#> 55       farver          2.1.1           2.1.1           TRUE
#> 56    htmltools          0.5.9           0.5.9           TRUE
#> 57     labeling          0.4.3           0.4.3           TRUE
#> 58    rmarkdown           2.29            2.29           TRUE
#> 59           S7          0.2.1           0.2.1           TRUE
#> 60      downlit          0.4.4           0.4.4           TRUE



```
