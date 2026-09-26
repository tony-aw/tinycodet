# Expose Exported Objects From Package Namespace in an Environment

`import_from()` exposes exported objects from a package to the specified
environment.  
  

## Usage

``` r
import_from(
  package,
  ls,
  re_exports = TRUE,
  lock = FALSE,
  prefix = NULL,
  lib.loc = .libPaths(),
  env = NULL
)
```

## Arguments

- package:

  a single string, giving the package name.

- ls:

  a character vector giving the names of exported objects to expose.  
  To expose all objects of a certain type, like all (infix/replacement)
  operators or all non-functions, the
  [import_ls](https://tony-aw.github.io/tinycodet/reference/import_ls.md)
  function can be used to get those object names.

- re_exports:

  `TRUE` or `FALSE`, indicating if re-exports of `package` should be
  included.  
  Default is `TRUE`, as that is analogous to the behaviour of base R's
  [::](https://rdrr.io/r/base/ns-dblcolon.html) operator.

- lock:

  `TRUE` or `FALSE`, indicating if the exported objects should be locked
  (see [lockBinding](https://rdrr.io/r/base/bindenv.html)).

- prefix:

  OPTIONAL: either `NULL` or a single string, giving the prefix to add
  to the functions, to avoid possible conflicts.  
  If `NULL` no prefix is added; this is the default.  
  Otherwise, a prefix is added to the exported object names.  
  Note that **no** prefix is added to infix operators, primitive
  functions, and non-functions (like constants).  
  The prefix cannot end with a single dot, as that might interfere with
  S3 methods.

- lib.loc:

  a character vector describing the location of R library trees to
  search through.

- env:

  see
  [import_env](https://tony-aw.github.io/tinycodet/reference/import_env.md).  
    

## Value

The objects specified in the given package will be placed & locked in
the specified environment.  
  

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md),
[`import_ls()`](https://tony-aw.github.io/tinycodet/reference/import_ls.md)

## Examples

``` r
import_from("stringi", import_ls("stringi", "infix"))
#> c("%s!=%", "%s!==%", "%s$%", "%s*%", "%s+%", "%s<%", "%s<=%", 
#> "%s==%", "%s===%", "%s>%", "%s>=%", "%stri!=%", "%stri!==%", 
#> "%stri$%", "%stri*%", "%stri+%", "%stri<%", "%stri<=%", "%stri==%", 
#> "%stri===%", "%stri>%", "%stri>=%")
#> Import & method registration complete
import_from("stringi", import_ls("stringi", "rp"))
#> c("stri_datetime_add", "stri_datetime_add<-", "stri_sub", "stri_sub<-", 
#> "stri_sub_all", "stri_sub_all<-", "stri_subset", "stri_subset<-", 
#> "stri_subset_charclass", "stri_subset_charclass<-", "stri_subset_coll", 
#> "stri_subset_coll<-", "stri_subset_fixed", "stri_subset_fixed<-", 
#> "stri_subset_regex", "stri_subset_regex<-")
#> Import & method registration complete


```
