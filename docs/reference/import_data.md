# Directly Return a Data-set From a Package

The `import_data()` function gets a specified data set from a package.  
Unlike [`utils::data()`](https://rdrr.io/r/utils/data.html), the
`import_data()` function returns the data set directly, and allows
assigning the data set like so:  
`mydata <- import_data(...)`.  

## Usage

``` r
import_data(package, dataname, lib.loc = .libPaths())
```

## Arguments

- package:

  a single string, giving the name of the R-package.

- dataname:

  a single string, giving the name of the data set.

- lib.loc:

  character vector specifying library search path (the location of R
  library trees to search through).  
  The `lib.loc` argument would usually be
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).  
  See also [loadNamespace](https://rdrr.io/r/base/ns-load.html).

## Value

Returns the data directly. Thus, one can assign the data like so:
`mydata <- import_data(...)`.

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)

## Examples

``` r
d <- import_data("datasets", "cars")
head(d)
#>   speed dist
#> 1     4    2
#> 2     4   10
#> 3     7    4
#> 4     7   22
#> 5     8   16
#> 6     9   10


```
