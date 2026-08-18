# Import R-package, its Re-exports, Dependencies, and/or Extensions, Under a Single Alias

The `import_as()` function imports the namespace of an R-package, and
optionally also its re-exports, dependencies, and extensions, all under
the same alias. The specified alias, containing the exported functions
from the specified packages, will be placed in the current
environment.  

## Usage

``` r
import_as(
  alias,
  main_package,
  re_exports = TRUE,
  dependencies = NULL,
  extensions = NULL,
  lib.loc = .libPaths(),
  import_order = c("dependencies", "main_package", "extensions")
)
```

## Arguments

- alias:

  a syntactically valid name giving the alias object where the
  package(s) are to be imported into.  
  This name can be given either as a single string (i.e. `"alias."`), or
  as a one-sided formula with a single term (i.e. `~ alias.`).

- main_package:

  a single string, giving the name of the main package to import under
  the given alias.  
  Core R (i.e. "base", "stats", etc.) is not allowed.

- re_exports:

  `TRUE` or `FALSE`.

  - If `re_exports = TRUE` the re-exports from the `main_package`
    (including those exported from Core R) are added to the alias
    together with the main package.  
    This is the default, as it is analogous to the behaviour of base R's
    [::](https://rdrr.io/r/base/ns-dblcolon.html) operator.  

  - If `re_exports = FALSE`, these re-exports are not added together
    with the main package.  
    The user can still import the packages under the alias from which
    the re-exported functions came from, by specifying them in the
    `dependencies` argument.

- dependencies:

  an optional character vector, giving the names of the dependencies of
  the `main_package` to be imported also under the alias.  
  Defaults to `NULL`, which means no dependencies are imported under the
  alias.  
  See
  [pkg_get_deps](https://tony-aw.github.io/tinycodet/reference/pkgs.md)
  to quickly get dependencies from a package.  
  Core R (i.e. "base", "stats", etc.) is not allowed.

- extensions:

  an optional character vector, giving the names of the extensions of
  the `main_package` to be imported also under the alias.  
  Defaults to `NULL`, which means no extensions are imported under the
  alias.  
  Core R (i.e. "base", "stats", etc.) is not allowed.

- lib.loc:

  character vector specifying library search path (the location of R
  library trees to search through).  
  The `lib.loc` argument would usually be
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).  
  See also [loadNamespace](https://rdrr.io/r/base/ns-load.html).  
    

- import_order:

  the character vector  
  `c("dependencies", "main_package", "extensions")`,  
  or some re-ordering of this character vector, giving the relative
  import order of the groups of packages.  
  See Details section for more information.  

## Value

A locked environment object, similar to the output of
[loadNamespace](https://rdrr.io/r/base/ns-load.html), with the name as
specified in the `alias` argument, will be created.  
This object, referred to as the "(package) alias object", will contain
the exported functions from the specified package(s).  
The alias object will be placed in the current environment.  
  
To use, for example, function "some_function()" from alias "alias.",
use:  
`alias.$some_function()`  
To see the special attributes of this alias object, use
[attr.import](https://tony-aw.github.io/tinycodet/reference/import_helper.md).  
To "unimport" the package alias object, simply remove it (i.e.
`rm(list = "alias.")`).  

## Details

**Expanded Definitions of Some Arguments**  

- "re-exports" are functions that are defined in the dependencies of the
  `main_package`, but are re-exported in the namespace of the
  `main_package`.  
  Unlike the `dependencies` argument, functions from core 'R' are
  included in re-exports.

- "dependencies" are here defined as any R-package appearing in the
  "Depends", "Imports", or "LinkingTo" fields of the Description file of
  the `main_package`. So no recursive dependencies.

- "extensions" are reverse-dependencies that actually extend the
  functionality of the `main_package`.  
  Programmatically, some package "E" is considered an extension of some
  "main_package", if the following is `TRUE`:  
  `"main_package" %in% `
  [pkg_get_deps_minimal](https://tony-aw.github.io/tinycodet/reference/pkgs.md)`("E")`  
    

**Why Aliasing Multiple Packages is Useful**  
To use an R-package with its extension packages or dependencies, whilst
avoiding the disadvantages of attaching a package (see
[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)),
one would traditionally use the
[::](https://rdrr.io/r/base/ns-dblcolon.html) operator like so:  

    main_package::some_function1()
    dependency1::some_function2()
    extension1::some_function3()

This becomes cumbersome as more packages are needed and/or as the
package name(s) become longer.  
The `import_as()` function avoids this issue by allowing multiple
**related** packages to be imported under a single alias, allowing one
to code like this:

    import_as(
       ~ alias., "main_package",
       dependencies = "dependency1", extensions = "extension1",
       lib.loc = .libPaths()
    )
    alias.$some_function1()
    alias.$some_function2()
    alias.$some_function3()

Thus importing a package, or multiple directly related packages, under a
single alias, which `import_as()` provides, avoids the above issues.
Importing a package under an alias is referred to as "aliasing" a
package.  
  
  
**Alias Naming Recommendation**  
To keep package alias object names easily distinguishable from other
objects that can also be subset with the
[\$](https://rdrr.io/r/base/Extract.html) operator, I recommend ending
(or starting, if you want to hide it from
[`ls()`](https://rdrr.io/r/base/ls.html)) all alias names with a dot
(`.`), or ending alias names with an underscore (`_`).  
  
  
**Regarding `import_order`**  
The order of the character vector given in the `dependencies` and
`extensions` arguments matters. If multiple packages share objects with
the same name, the objects of the package named last will overwrite
those of the earlier named packages.  
  
The `import_order` argument defaults to the character vector  
`c("dependencies", "main_package", "extensions")`,  
which is the recommended setting.  
This setting results in the following importing order:  

1.  The dependencies, **in the order specified by the `dependencies`
    argument**.

2.  The main_package (see argument `main_package`), including re-exports
    (if `re_exports = TRUE`).

3.  The extensions, **in the order specified by the `extensions`
    argument**.  
      

**Other Details**  

- Packages that appear in the "Suggests" or "Enhances" fields of
  packages are not considered dependencies or extensions.

- No more than 10 packages (ignoring re-exports) are allowed to be
  imported under a single alias.

- Primitive functions (see
  [is.primitive](https://rdrr.io/r/base/is.function.html)) are not
  imported.  
    

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)

## Examples

``` r
"data.table" %installed in% .libPaths()
#> data.table 
#>       TRUE 

import_as( # this creates the 'dt.' object
  ~ dt., "data.table"
)
#> Importing packages and registering methods...
#> Done
#> You can now access the functions using `dt.$`
#> For conflicts report, packages order, and other attributes, run `attr.import(dt.)` 

```
