# List Exported Objects from Package Namespace

Lists exported objects defined (or re-exported) in a package
namespace.  
Note that `import_ls()` necessary loads the package, but does not attach
the package.  
  
The return value of `import_ls()` is for programmatically dynamic code
(see section `Value`).  
The side-effect of `import_ls()` is for syntactically readable code (see
section `Side Effect`).  
  

## Usage

``` r
import_ls(
  package,
  types = c("reg", "infix", "rp", "nonfun"),
  re_exports = TRUE,
  lib.loc = .libPaths(),
  print = TRUE
)
```

## Arguments

- package:

  a single string, giving the package name.

- types:

  a character vector, specifying the type.  
  The following types are supported:

  - "reg": regular functions.

  - "infix": infix operators.

  - "rp": replacement operators, including their base functions.

  - "nonfun": non-functions.

- re_exports:

  `TRUE` or `FALSE`, indicating if re-exports of `package` should be
  included.  
  Default is `TRUE`, as that is analogous to the behaviour of base R's
  [::](https://rdrr.io/r/base/ns-dblcolon.html) operator.

- lib.loc:

  a character vector describing the location of R library trees to
  search through.

- print:

  `TRUE` (default) or `FALSE`, indicating if the exported objects should
  be printed to the console as literal code.  
  Note that this is a **side-effect**, and does not impact the storable
  return value of `import_ls()`.  
    

## Value

A character vector of exported object names defined in the package.  
Can be used programmatically in
[library](https://rdrr.io/r/base/library.html) or
[import_from](https://tony-aw.github.io/tinycodet/reference/import_from.md)
functions.  
I.e.:

    # Like so:

    ls <- import_ls("packagename", "infix")
    library(packagename, include.only = ls)

    # Or like so:

    ls <- import_ls("packagename", "infix")
    import_from(packagename, ls = ls)

## Side Effect

**(if `print = TRUE`)**  
The returned character vector is printed to your console as literal
code.  
I.e. `'c("obj1", "obj2")'`.  
One can then copy-paste the printed literal code, for syntactical
clarity.  
I.e.:

    # Like so:

    import_ls("packagename", "infix") # prints literal code to your console
    library(packagename, include.only = ...paste printed literal code here...)

    # Or like so:

    import_ls("packagename", "infix") # prints literal code to your console
    import_from("packagename", ls = ...paste printed literal code here...)

## Why Listing Functions by Type is Useful

One can import a package under an alias using
[import_as](https://tony-aw.github.io/tinycodet/reference/import_as.md).  
But using infix operators or replacement operators from an alias
requires convoluted code like so:  

    .alias$`%op%`(x, y)
    .alias$`fun<-`(x, ..., value)

Instead, it would be easier to just attach (via
[library](https://rdrr.io/r/base/library.html)) or expose (via
[import_from](https://tony-aw.github.io/tinycodet/reference/import_from.md))
such operators so that they can be used on their own.  
The `import_ls()` function allows the user to get a list of all
functions from a certain type, like "infix operators" or "replacement
operators".  
The listed functions can then be passed to
[library](https://rdrr.io/r/base/library.html) (to attach them) or
[import_from](https://tony-aw.github.io/tinycodet/reference/import_from.md)
(to expose them).  
  

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md),
[import_from](https://tony-aw.github.io/tinycodet/reference/import_from.md),
[library](https://rdrr.io/r/base/library.html)

## Examples

``` r
# Programmatically Dynamic Code ====

ls <- import_ls("stringi", "infix")
#> c("%s!=%", "%s!==%", "%s$%", "%s*%", "%s+%", "%s<%", "%s<=%", 
#> "%s==%", "%s===%", "%s>%", "%s>=%", "%stri!=%", "%stri!==%", 
#> "%stri$%", "%stri*%", "%stri+%", "%stri<%", "%stri<=%", "%stri==%", 
#> "%stri===%", "%stri>%", "%stri>=%")
import_from("stringi", ls)
#> Import & method registration complete

if("bit64" %installed in% .libPaths()) {
  ls <- import_ls("bit64", "nonfun")
  import_from("bit64", ls)
}
#> Registered S3 method overwritten by 'bit64':
#>   method          from 
#>   print.bitstring tools
#> "NA_integer64_"
#> Import & method registration complete


# Syntactically Readable Code ====

import_ls("stringi", "rp") # copy-pasted the printed literal code
#> c("stri_datetime_add", "stri_datetime_add<-", "stri_sub", "stri_sub<-", 
#> "stri_sub_all", "stri_sub_all<-", "stri_subset", "stri_subset<-", 
#> "stri_subset_charclass", "stri_subset_charclass<-", "stri_subset_coll", 
#> "stri_subset_coll<-", "stri_subset_fixed", "stri_subset_fixed<-", 
#> "stri_subset_regex", "stri_subset_regex<-")
import_from(
  "stringi",
  ls = c("stri_datetime_add", "stri_datetime_add<-", "stri_sub", "stri_sub_all", 
         "stri_sub_all<-", "stri_sub<-", "stri_subset", "stri_subset_charclass", 
         "stri_subset_charclass<-", "stri_subset_coll", "stri_subset_coll<-", 
         "stri_subset_fixed", "stri_subset_fixed<-", "stri_subset_regex", 
         "stri_subset_regex<-", "stri_subset<-")
)
#> Import & method registration complete

```
