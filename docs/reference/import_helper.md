# Helper Functions for the 'tinycodet' Package Import System

The `help.import()` function finds the help file for functions or
topics, including exposed functions/operators as well as functions in a
package alias object.  
  
The `is.tinyimport()` function checks if an alias object or an exposed
function is of class `tinyimport`; i.e. if it is an object produced by
the
[import_as](https://tony-aw.github.io/tinycodet/reference/import_as.md),
[import_inops](https://tony-aw.github.io/tinycodet/reference/import_inops.md),
or
[import_LL](https://tony-aw.github.io/tinycodet/reference/import_misc.md)
function.  
  
The `attr.import()` function gets one or all special attribute(s) from
an alias object returned by
[import_as](https://tony-aw.github.io/tinycodet/reference/import_as.md).  
  

## Usage

``` r
help.import(..., i, alias)

is.tinyimport(x)

attr.import(alias, which = NULL)
```

## Arguments

- ...:

  further arguments to be passed to
  [help](https://rdrr.io/r/utils/help.html).

- i:

  either one of the following:

  - a function (use back-ticks when the function is an infix operator).
    Examples: `myfun` , `` `%operator%` `` , `myalias.$some_function` .
    If a function, the `alias` argument is ignored.

  - a string giving the function name or topic (i.e. `"myfun"`,
    `"thistopic"`). If a string, argument `alias` must be specified
    also.

- alias:

  the alias object as created by the
  [import_as](https://tony-aw.github.io/tinycodet/reference/import_as.md)
  function.  

- x:

  an existing object (i.e. an assigned variable or a locked constant) to
  be tested.

- which:

  The attributes to list. If `NULL`, all attributes will be returned.  
  Possibilities: "pkgs", "conflicts", "args", and
  "ordered_object_names".

## Value

For `help.import()`:  
Opens the appropriate help page.  
  
For `is.tinyimport()`:  
Returns `TRUE` if the function is produced by
[import_as](https://tony-aw.github.io/tinycodet/reference/import_as.md),
[import_inops](https://tony-aw.github.io/tinycodet/reference/import_inops.md),
or
[import_LL](https://tony-aw.github.io/tinycodet/reference/import_misc.md),
and returns `FALSE` if it is not.  
  
For `attr.import(alias, which = NULL)`:  
All special attributes of the given alias object are returned as a
list.  
  
For `attr.import(alias, which = "pkgs")`:  
Returns a list with 3 elements:

- packages_order: a character vector of package names, giving the
  packages in the order they were imported in the alias object.

- main_package: a string giving the name of the main package.
  Re-exported functions, if present, are taken together with the main
  package.

- re_exports.pkgs: a character vector of package names, giving the
  packages from which the re-exported functions in the main package were
  taken.  
    

For `attr.import(alias, which = "conflicts")`:  
The order in which packages are imported in the alias object (see
attribute `pkgs$packages_order`) matters: Functions from later named
packages overwrite those from earlier named packages, in case of
conflicts.  
The "conflicts" attribute returns a data.frame showing exactly which
functions overwrite functions from earlier named packages, and as such
"win" the conflicts.  
  
For `attr.import(alias, which = "args")`:  
Returns a list of input arguments. These were the arguments supplied to
[import_as](https://tony-aw.github.io/tinycodet/reference/import_as.md)
when the alias object in question was created.  
  
For `attr.import(alias, which = "ordered_object_names")`:  
Gives the names of the objects in the alias, in the order as they were
imported.  
For conflicting objects, the last imported ones are used for the
ordering.  
Note that if argument `re_exports` is `TRUE`, re-exported functions are
imported when the main package is imported, thus changing this order
slightly.  
  

## Details

For `help.import(...)`:  
Do not use the `topic` / `package` and `i` / `alias` argument sets
together. It's either one set or the other.  
For example:

    import_as(~ mr., "magrittr")
    import_inops(mr.)
    help.import(i = mr.$add)
    help.import(i = `%>%`)
    help.import(i = "add", alias = mr.)
    help.import(topic = "%>%", package = "magrittr")
    help.import("%>%", package = "magrittr") # same as previous line

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)

## Examples

``` r
import_as(~ to., "tinycodet")
#> Importing packages and registering methods...
#> Done
#> You can now access the functions using `to.$`
#> For conflicts report, packages order, and other attributes, run `attr.import(to.)` 
import_inops(to.)
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
`%s==%` <- stringi::`%s==%`

is.tinyimport(to.) # returns TRUE
#> [1] TRUE
is.tinyimport(`%s==%`) # returns FALSE: not imported by tinycodet import system
#> [1] FALSE

attr.import(to., "conflicts")
#>                  package winning_conflicts
#> 1 tinycodet + re-exports                  



```
