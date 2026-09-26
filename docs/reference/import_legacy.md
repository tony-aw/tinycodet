# Legacy import\_ functions

These functions are deprecated, and will be removed in a future
update.  
  

## Usage

``` r
import_inops(expose, lib.loc = .libPaths(), ...)

import_LL(package, selection, lib.loc = .libPaths())
```

## Arguments

- expose, package:

  a single string, giving the name of the R-package.

- lib.loc:

  a character vector describing the location of R library trees to
  search through.

- ...:

  further arguments passed to
  [import_from](https://tony-aw.github.io/tinycodet/reference/import_from.md)
  or
  [import_ls](https://tony-aw.github.io/tinycodet/reference/import_ls.md)

- selection:

  a character vector of function names (both regular functions and infix
  operators).  
  Internal functions or re-exported functions are not supported.

## Value

See
[import_from](https://tony-aw.github.io/tinycodet/reference/import_from.md).

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md),
[`import_from()`](https://tony-aw.github.io/tinycodet/reference/import_from.md),
[`import_ls()`](https://tony-aw.github.io/tinycodet/reference/import_ls.md)

## Examples

``` r
import_inops("stringi")
#> Warning: `import_inops()` is deprecated and will be removed;
#>           please use `import_from(..., ls = import_ls(..., type = "infix"))` instead
#> calling:
#> `import_from(
#>  "stringi"
#>  ls = import_ls("stringi","infix", FALSE, lib.loc, FALSE), 
#>  FALSE, FALSE, lib.loc, parent.frame(), ...
#> )`
#> Import & method registration complete
import_LL("stringi", "stri_c")
#> Warning: `import_LL()` is deprecated and will be removed; please use `import_from()` instead
#> calling `import_from(..., env = parent.frame())`
#> Import & method registration complete


```
