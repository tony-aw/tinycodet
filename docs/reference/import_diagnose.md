# Check for Mismatches between Loaded and Installed Packages

The `import_diagnose()` function compares the loaded Namespaces with
those installed in the specified `lib.loc`, and checks for version and
library path mismatches.  
Any differences found will be reported in the form of a simple
`data.frame`.  
  

## Usage

``` r
import_diagnose(lib.loc = .libPaths())
```

## Arguments

- lib.loc:

  a character vector describing the location of R library trees to
  search through.  
    

## Value

If no issues are found, returns `NULL`.  
Otherwise, a data.frame giving the packages where issues have been
found.  
This data.frame will have the following columns:

- "package": Character vector of package names

- "installed_in_lib.loc": logical vector indicating if the package is
  actually installed in the given `lib.loc` paths (`TRUE`) or not
  (`FALSE`).

- "version_loaded": character vector giving the version of the packages
  as loaded in [loadedNamespaces](https://rdrr.io/r/base/ns-load.html).

- "version_installed": character vector giving the version of the
  packages as installed in `lib.loc`;  
  It will be `NA` if the package is not installed in `lib.loc`.

- "versions_equal": logical vector indicating if the loaded and
  installed versions match.  
  Gives `NA` if the package is not installed in `lib.loc`.  
    

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)

## Examples

``` r
import_diagnose()
#> NULL


```
