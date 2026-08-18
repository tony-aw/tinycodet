# (Un)Expose Infix Operators From Package Namespace in the Current Environment

`import_inops(expose = ...)` exposes infix operators specified in a
package or an alias object to the current environment.  
  
`import_inops(unexpose = ...)` "unexposes" (i.e. removes) the infix
operators specified in a package or an alias object from the current
environment.  
Note that in this case only infix operators exposed by the 'tinycodet'
import system will be removed from the current environment; "regular"
(i.e. user-defined) infix operators will not be touched.  
  
To attach all infix operators from a package to the global namespace,
one can use the
[pkg_lsf](https://tony-aw.github.io/tinycodet/reference/pkgs.md)
function like so:

    y <- pkg_lsf("packagename", type = "inops")
    library(packagename, include.only = y)

## Usage

``` r
import_inops(expose = NULL, unexpose = NULL, lib.loc = .libPaths(), ...)
```

## Arguments

- expose, unexpose:

  either one of the following:

  - an alias object as produced by the
    [import_as](https://tony-aw.github.io/tinycodet/reference/import_as.md)
    function.

  - a string giving the package name. Core R (i.e. "base", "stats",
    etc.) is not allowed.

- lib.loc:

  character vector specifying library search path (the location of R
  library trees to search through).  
  Only used when supplying a string to `expose` / `unexpose`, and
  ignored when supplying an alias object to `expose` / `unexpose` (the
  library is path already stored inside the alias object).  
  The `lib.loc` argument would usually be
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).  
  See also [loadNamespace](https://rdrr.io/r/base/ns-load.html).

- ...:

  additional arguments, only relevant if the `expose` argument is
  used.  
  See
  [import_inops.control](https://tony-aw.github.io/tinycodet/reference/import_inops.control.md).  
    

## Value

If using argument `expose`:  
The infix operators specified in the given package or alias will be
placed in the current environment.  
  
If using argument `unexpose`:  
The infix operators specified in the given package or alias, exposed by
`import_inops()`, will be removed from the current environment.  
If such infix operators could not be found, this function simply returns
`NULL`.  
  

## Details

**Why Exposing Infix Operators Is Useful**  
To use a function from an R-package, while avoiding the disadvantages of
attaching a package (see
[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)),
one would traditionally use the
[::](https://rdrr.io/r/base/ns-dblcolon.html) operator like so:

    packagename::function_name()

This is, however, cumbersome with infix operators, as it forces one to
code like this:

    packagename::`%op%`(x,y)

Exposing infix operators to the current environment, using the
`import_inops()` function, allows one to use infix operators without
using cumbersome code, and without having to attach the infix operators
globally.  
  
  
**Other Details**  
The `import_inops()` function does not support overloading base/core R
operators.  
  
When using `import_inops()` to remove infix operators from the current
environment, it will use the attributes of those operators to determine
if the infix operator came from the 'tinycodet' import system or not.
Only infix operators exposed by the 'tinycodet' import system will be
removed.  
  
Primitive operators (see
[is.primitive](https://rdrr.io/r/base/is.function.html)) are not
exposed.  
  

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md),
[`import_inops.control()`](https://tony-aw.github.io/tinycodet/reference/import_inops.control.md),
[`report_inops()`](https://tony-aw.github.io/tinycodet/reference/report_inops.md)

## Examples

``` r
import_inops(expose = "stringi") # expose infix operators from package
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
import_inops(unexpose = "stringi") # remove the exposed infix operators from environment
#> Removing the following infix operators:
#> %s==%, %s<%, %s>=%, %stri!=%, %s>%, %s!=%, %stri<=%, %s$%, %stri<%, %stri*%, %stri!==%, %stri+%, %s!==%, %stri===%, %s*%, %s===%, %stri$%, %stri==%, %s+%, %s<=%, %stri>%, %stri>=%
#> Done

import_as(~ stri., "stringi")
#> Importing packages and registering methods...
#> Done
#> You can now access the functions using `stri.$`
#> For conflicts report, packages order, and other attributes, run `attr.import(stri.)` 
import_inops(expose = stri.) # expose infix operators from alias
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
import_inops(unexpose = stri.) # unexposed infix operators from current environment
#> Removing the following infix operators:
#> %s>=%, %stri<%, %s===%, %s==%, %stri*%, %stri===%, %stri>%, %s<=%, %stri+%, %s$%, %stri!=%, %stri>=%, %stri<=%, %s!==%, %stri$%, %s<%, %stri==%, %s!=%, %s*%, %stri!==%, %s>%, %s+%
#> Done


# additional arguments (only used when exposing, not unexposing):
import_inops(expose = "stringi", exclude = "%s==%")
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
import_inops(unexpose = "stringi")
#> Removing the following infix operators:
#> %s<%, %s>=%, %stri!=%, %s>%, %s!=%, %stri<=%, %s$%, %stri<%, %stri*%, %stri!==%, %stri+%, %s!==%, %stri===%, %s*%, %s===%, %stri$%, %stri==%, %s+%, %s<=%, %stri>%, %stri>=%
#> Done
import_inops(expose = "stringi", overwrite = FALSE)
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
import_inops(unexpose = "stringi")
#> Removing the following infix operators:
#> %s==%, %s<%, %s>=%, %stri!=%, %s>%, %s!=%, %stri<=%, %s$%, %stri<%, %stri*%, %stri!==%, %stri+%, %s!==%, %stri===%, %s*%, %s===%, %stri$%, %stri==%, %s+%, %s<=%, %stri>%, %stri>=%
#> Done

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
import_inops(expose = stri., overwrite = FALSE)
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
import_inops(unexpose = stri.)
#> Removing the following infix operators:
#> %s>=%, %stri<%, %s===%, %s==%, %stri*%, %stri===%, %stri>%, %s<=%, %stri+%, %s$%, %stri!=%, %stri>=%, %stri<=%, %s!==%, %stri$%, %s<%, %stri==%, %s!=%, %s*%, %stri!==%, %s>%, %s+%
#> Done



```
