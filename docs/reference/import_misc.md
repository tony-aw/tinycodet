# Miscellaneous import\_ - Functions

The `import_LL()` function places specific functions from a package in
the current environment, and also locks (see
[lockBinding](https://rdrr.io/r/base/bindenv.html)) the specified
functions to prevent modification.  
The primary use-case for this function is for exposing functions inside
a local environment.  
  
The `import_int()` function directly returns an internal function from a
package.  
It is similar to the [:::](https://rdrr.io/r/base/ns-dblcolon.html)
operator, but with 2 key differences:  

1.  `import_int()` includes the `lib.loc` argument.

2.  `import_int()` only searches internal functions, not exported ones.
    This makes it clearer in your code that you're using an internal
    function, instead of making it ambiguous.  
      

## Usage

``` r
import_LL(package, selection, lib.loc = .libPaths())

import_int(form, lib.loc = .libPaths())
```

## Arguments

- package:

  a single string, giving the name of the package to take functions
  from.  
  Core R (i.e. "base", "stats", etc.) is not allowed.

- selection:

  a character vector of function names (both regular functions and infix
  operators).  
  Internal functions or re-exported functions are not supported.

- lib.loc:

  character vector specifying library search path (the location of R
  library trees to search through).  
  The `lib.loc` argument would usually be
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).  
  See also [loadNamespace](https://rdrr.io/r/base/ns-load.html).

- form:

  a two-sided formula, with one term on each side.  
  The term on the left hand side should give a single package name.  
  The term on the right hand side should give a single internal
  function.  
  Example: `package_name ~ function_name`  
  Core R (i.e. "base", "stats", etc.) is not allowed.

## Value

For `import_LL()`:  
The specified functions will be placed in the current environment, and
locked.  
To unexpose or overwrite the functions, simply remove them; i.e.:  
`rm(list=c("some_function1", "some_function2")`).  
  
For `import_int()`:  
The function itself is returned directly.  
So one can assign the function directly to some variable, like so:  
`myfun <- import_int(...)`  
or use it directly without re-assignment like so:  
`import_int(...)(...)`

## Details

**Regarding the Locks in `import_LL()`**  
The
[import_as](https://tony-aw.github.io/tinycodet/reference/import_as.md)
function returns a locked environment, just like
[loadNamespace](https://rdrr.io/r/base/ns-load.html), thus protecting
the functions from accidental modification or re-assignment.  
The
[import_inops](https://tony-aw.github.io/tinycodet/reference/import_inops.md)
function returns infix operators, and though these are not locked, one
needs to surround infix operators by back ticks to re-assign or modify
them, which is unlikely to happen on accident.  
The `import_LL()` function, however, returns "loose" functions. And
these functions (unless they are infix operators) do not have the
protection due to a locked environment or due to the syntax.  
Therefore, to ensure safety from (accidental) modification or
re-assignment, the `import_LL()` function locks these functions (see
[lockBinding](https://rdrr.io/r/base/bindenv.html)). For consistency,
infix operators exposed by `import_LL()` are also locked.  
  
**Other Details**  
The `import_LL()` and `import_int()` functions do not support importing
functions from base/core R.  
  

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)

## Examples

``` r
# Using import_LL ====
import_LL(
  "stringi", "stri_sub"
)
#> exposing and locking functions to current environment ...
#> Done
# the stri_sub() function now cannot be modified, only used or removed, because it's locked:
bindingIsLocked("stri_sub", environment()) # TRUE
#> [1] TRUE

mypaste <- function(x, y) {
  import_LL("stringi", selection = "stri_c")
  stri_c(x, y)
  }
mypaste("hello ", "world")
#> exposing and locking functions to current environment ...
#> Done
#> [1] "hello world"



# Using internal function ====
# Through re-assignment:
fun <- import_int(tinycodet ~ .internal_paste, .libPaths())
fun("hello", "world")
#> [1] "helloworld"

# Or using directly:
import_int(
  tinycodet ~ .internal_paste, .libPaths()
)("hello", "world")
#> [1] "helloworld"


```
