# import_inops.control

Additional arguments to control exposing infix operators in the
[import_inops](https://tony-aw.github.io/tinycodet/reference/import_inops.md)
function.

## Usage

``` r
import_inops.control(
  exclude = NULL,
  include.only = NULL,
  overwrite = TRUE,
  inherits = FALSE
)
```

## Arguments

- exclude:

  a character vector, giving the infix operators NOT to expose to the
  current environment.  
  This can be handy to prevent overwriting any (user defined) infix
  operators already present in the current environment.  

- include.only:

  a character vector, giving the infix operators to expose to the
  current environment, and the rest of the operators will not be
  exposed.  
  This can be handy to prevent overwriting any (user defined) infix
  operators already present in the current environment.  

- overwrite:

  logical, indicating if it is allowed to overwrite existing infix
  operators.

  - If `TRUE` (default), a warning is given when operators existing in
    the current environment are being overwritten, but the function
    continuous nonetheless.

  - If `FALSE`, an error is produced when the to be exposed operators
    already exist in the current environment, and the function is
    halted.

- inherits:

  logical.  
  When exposing infix operators,
  [import_inops](https://tony-aw.github.io/tinycodet/reference/import_inops.md)
  checks if infix operators with the same names are already present in
  the current environment.  
  If `inherits = FALSE`, only the current environment is checked for
  existing operators.  
  If `inherits = TRUE`, enclosed environments, most notably package
  namespaces, are also checked for existing operators.  
  Defaults to `FALSE`.  
  See also [exists](https://rdrr.io/r/base/exists.html).  

## Value

This function is used internally in the
[import_inops](https://tony-aw.github.io/tinycodet/reference/import_inops.md)
function.  
  

## Details

You cannot specify both the `exclude` and `include.only` arguments. Only
one or the other, or neither.  
  

## See also

[`import_inops()`](https://tony-aw.github.io/tinycodet/reference/import_inops.md),
[`tinycodet_import()`](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)

## Examples

``` r
# additional arguments (only used when exposing, not unexposing):
import_as(~ stri., "stringi")
#> Importing packages and registering methods...
#> Done
#> You can now access the functions using `stri.$`
#> For conflicts report, packages order, and other attributes, run `attr.import(stri.)` 
import_inops(expose = stri., include.only = "%s==%")
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
import_inops(unexpose = stri.)
#> Removing the following infix operators:
#> %s==%
#> Done
import_inops(expose = "stringi", exclude = "%s==%")
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
import_inops(unexpose = "stringi")
#> Removing the following infix operators:
#> %s<%, %s>=%, %stri!=%, %s>%, %s!=%, %stri<=%, %s$%, %stri<%, %stri*%, %stri!==%, %stri+%, %s!==%, %stri===%, %s*%, %s===%, %stri$%, %stri==%, %s+%, %s<=%, %stri>%, %stri>=%
#> Done
import_inops(expose = stri., overwrite = FALSE)
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
import_inops(unexpose = stri.)
#> Removing the following infix operators:
#> %s>=%, %stri<%, %s===%, %s==%, %stri*%, %stri===%, %stri>%, %s<=%, %stri+%, %s$%, %stri!=%, %stri>=%, %stri<=%, %s!==%, %stri$%, %s<%, %stri==%, %s!=%, %s*%, %stri!==%, %s>%, %s+%
#> Done
import_inops(expose = "stringi", overwrite = FALSE)
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
import_inops(unexpose = "stringi")
#> Removing the following infix operators:
#> %s==%, %s<%, %s>=%, %stri!=%, %s>%, %s!=%, %stri<=%, %s$%, %stri<%, %stri*%, %stri!==%, %stri+%, %s!==%, %stri===%, %s*%, %s===%, %stri$%, %stri==%, %s+%, %s<=%, %stri>%, %stri>=%
#> Done

```
