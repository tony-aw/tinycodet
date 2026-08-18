# Report Infix Operators

The `report_inops()` function returns a data.frame listing the infix
operators defined in the current environment, or a user specified
environment. It also reports from which packages the infix operators
came from.

## Usage

``` r
report_inops(env)
```

## Arguments

- env:

  an optional environment to give, where the function should look for
  infix operators.  
  When not specified, the current environment is used.  

## Value

A data.frame. The first column gives the infix operator names. The
second column gives the package the operator came from, or NA if it did
not come from a package.

## See also

[`tinycodet_misc()`](https://tony-aw.github.io/tinycodet/reference/aaa5_tinycodet_misc.md)

## Examples

``` r
report_inops()
#> NULL

`%paste%` <- function(x,y)paste0(x,y)

report_inops()
#>   infix_operator package
#> 1        %paste%    <NA>

import_inops("stringi")
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done

report_inops()
#>    infix_operator package
#> 1         %paste%    <NA>
#> 2           %s!=% stringi
#> 3          %s!==% stringi
#> 4            %s$% stringi
#> 5            %s*% stringi
#> 6            %s+% stringi
#> 7            %s<% stringi
#> 8           %s<=% stringi
#> 9           %s==% stringi
#> 10         %s===% stringi
#> 11           %s>% stringi
#> 12          %s>=% stringi
#> 13       %stri!=% stringi
#> 14      %stri!==% stringi
#> 15        %stri$% stringi
#> 16        %stri*% stringi
#> 17        %stri+% stringi
#> 18        %stri<% stringi
#> 19       %stri<=% stringi
#> 20       %stri==% stringi
#> 21      %stri===% stringi
#> 22        %stri>% stringi
#> 23       %stri>=% stringi



```
