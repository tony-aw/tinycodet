# Miscellaneous Package Related Functions

The `pkgs %installed in% lib.loc` operator checks if one or more given
packages (`pkgs`) exist in the given library paths (`lib.loc`), without
loading the packages at all.  
The syntax of this operator forces the user to make it syntactically
explicit where to look for installed R-packages.  
As `pkgs %installed in% lib.loc` does not even load a package, the user
can safely use it without fearing any unwanted side-effects.  
  
The `pkg_get_deps()` function gets the **direct** dependencies of a
package from the Description file. It works on non-CRAN packages also.  
  
The `pkg_get_deps_minimal()` function is the same as `pkg_get_deps()`,
except with `base, recom, semi, shared_tidy` all set to `FALSE`, and the
default value for `deps_type` is c("Depends", "Imports").  
  

## Usage

``` r
pkgs %installed in% lib.loc

pkg_get_deps(
  package,
  lib.loc = .libPaths(),
  deps_type = c("LinkingTo", "Depends", "Imports"),
  base = FALSE,
  recom = TRUE,
  semi = TRUE,
  shared_tidy = TRUE
)

pkg_get_deps_minimal(
  package,
  lib.loc = .libPaths(),
  deps_type = c("Depends", "Imports")
)
```

## Arguments

- pkgs:

  a character vector with the package name(s).

- lib.loc:

  character vector specifying library search path (the location of R
  library trees to search through).  
  The `lib.loc` argument would usually be
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).  
  See also [loadNamespace](https://rdrr.io/r/base/ns-load.html).  
  **For `pkg_get_deps()` and `pkg_get_deps_minimal()`**:  
  `lib.loc` can also be set to `NULL`.  
  In that case the loaded namespace of the package will be checked
  instead of the installed packages at `lib.loc`.  
  If `lib.loc` is `NULL`, but the package is not loaded, an error is
  returned.

- package:

  a single string giving the package name.

- deps_type:

  a character vector, giving the dependency types to be used.  
  The order of the character vector given in `deps_type` affects the
  order of the returned character vector; see Details sections.

- base:

  `TRUE` or `FALSE`, indicating whether base/core R should be included
  (`TRUE`), or not included (`FALSE`).

- recom:

  `TRUE` or `FALSE`, indicating whether the pre-installed 'recommended'
  R-packages should be included (`TRUE`), or not included (`FALSE`).

- semi:

  `TRUE` or `FALSE`, indicating whether semi-ingrained R-packages ('S7',
  'rstudioapi') should be included (`TRUE`) or not include (`FALSE`).

- shared_tidy:

  `TRUE` or `FALSE`, indicating whether the following packages should be
  included (`TRUE`) or not included (`FALSE`):  
  'rlang', 'lifecycle', 'cli', 'glue', and 'withr'.  
    

## Value

For `pkgs %installed in% lib.loc`:  
Returns a named logical vector.  
The names give the package names.  
The value `TRUE` indicates a package is installed in `lib.loc`.  
The value `FALSE` indicates a package is not installed in `lib.loc`.  
The value `NA` indicates a package is not actually a separate package,
but base/core 'R' (i.e. 'base', 'stats', etc.).  
  
For `pkg_get_deps()` and `pkg_get_deps_minimal()`:  
A character vector of direct dependencies, without duplicates.  
  

## Details

For `pkg_get_deps()`:  
For each string in argument `deps_type`, the package names in the
corresponding field of the Description file are extracted, in the order
as they appear in that field.  
The order given in argument `deps_type` also affects the order of the
returned character vector:  
For example, `c("LinkingTo", "Depends", "Imports")`,  
means the package names are extracted from the fields in the following
order:

1.  "LinkingTo";

2.  "Depends";

3.  "Imports".

The unique (thus non-repeating) package names are then returned to the
user.  
  

## References

O'Brien J., elegantly extract R-package dependencies of a package not
listed on CRAN. *Stack Overflow*. (1 September 2023).
<https://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran>

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)

## Examples

``` r
"dplyr" %installed in% .libPaths()
#> dplyr 
#>  TRUE 

pkg_get_deps_minimal("dplyr")
#> [1] "generics"   "magrittr"   "pillar"     "R6"         "tibble"    
#> [6] "tidyselect" "vctrs"     
pkgs <- pkg_get_deps("dplyr")
pkgs %installed in% .libPaths()
#>        cli   generics       glue  lifecycle   magrittr     pillar         R6 
#>       TRUE       TRUE       TRUE       TRUE       TRUE       TRUE       TRUE 
#>      rlang     tibble tidyselect      vctrs 
#>       TRUE       TRUE       TRUE       TRUE 


```
