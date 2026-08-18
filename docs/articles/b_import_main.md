# Import system - main functions

``` r
library(tinycodet)
#> Run `?tinycodet::tinycodet` to open the introduction help page of 'tinycodet'.
```

 

## Introduction

One can use a package without attaching (for example using `::`), or one
can attach a package (for example using
[`library()`](https://rdrr.io/r/base/library.html) or
[`require()`](https://rdrr.io/r/base/library.html)).

The advantages and disadvantages of using without attaching a package
versus attaching a package - at least those relevant for this article -
can be compactly presented in the following table:

[TABLE]

What `tinycodet` attempts to do with its import system, is to somewhat
find the best of both worlds. It does this by introducing the following
functions:

- [`import_as()`](https://tony-aw.github.io/tinycodet/reference/import_as.md):
  Import a main package, and optionally its re-exports + its
  dependencies + its extensions, under a single alias. This essentially
  combines the attaching advantage of using multiple related packages
  (row 7 on the table above), whilst keeping most advantages of using
  without attaching a package.
- [`import_inops()`](https://tony-aw.github.io/tinycodet/reference/import_inops.md):
  Expose infix operators from a package or an alias object to the
  current environment. This gains the attaching advantage of less typing
  (row 6 in table above), whilst simultaneously avoiding the
  disadvantage of attaching functions from a package globally (row 4).
- [`import_data()`](https://tony-aw.github.io/tinycodet/reference/import_data.md):
  Directly return a data set from a package, to allow straight-forward
  assignment.

The import package system presented here is just another option
provided, just like the [import](https://github.com/rticulate/import)
and [box](https://github.com/klmr/box) packages provide their own
alternative import systems. Please feel free to completely ignore this
article if you’re really adamant on attaching packages using
[`library()`](https://rdrr.io/r/base/library.html)/[`require()`](https://rdrr.io/r/base/library.html)
:-).

This article is rather lengthy, so I will start with a quick example
code using `tinycodet`’ import system:

``` r
# importing "tidytable" + its re-exports + "data.table" under alias "tdt.":
import_as( 
  ~ tdt., "tidytable", dependencies = "data.table"
)
#> Importing packages and registering methods...
#> Done
#> You can now access the functions using `tdt.$`
#> For conflicts report, packages order, and other attributes, run `attr.import(tdt.)`

# exposing operators from `magrrittr` to current environment:
import_inops("magrittr")
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done

# directly assigning the "starwars" dataset to object "d":
d <- import_data("dplyr", "starwars") 

# see it in action:
d %>% tdt.$filter(species == "Droid") %>%
  tdt.$select(name, tdt.$ends_with("color"))
#> # A tidytable: 6 × 4
#>   name   hair_color skin_color  eye_color
#>   <chr>  <chr>      <chr>       <chr>    
#> 1 C-3PO  NA         gold        yellow   
#> 2 R2-D2  NA         white, blue red      
#> 3 R5-D4  NA         white, red  red      
#> 4 IG-88  none       metal       red      
#> 5 R4-P17 none       silver, red red, blue
#> 6 BB8    none       none        black

rm(list=ls()) # clearing everything
```

The above code is run *without attaching* any of the packages or its
dependencies. So none of the problems with attaching a package is
present.

Despite the length of this article, which is mostly due to me being
overly detailed, the import system is made to be *very simple for the
user*.

What follows are descriptions of the main functions that together form
this new, infix-operator friendly **&** multi-package assignment
friendly, import management system.

 

## import_as

The
[`import_as()`](https://tony-aw.github.io/tinycodet/reference/import_as.md)
function imports an R package + its re-exports under an alias, and also
imports any specified direct **dependencies** and/or direct
**extensions** of the package under the very same alias. It also informs
the user which objects from a package will overwrite which objects from
other packages, so you will never be surprised.

The main arguments of the
[`import_as()`](https://tony-aw.github.io/tinycodet/reference/import_as.md)
function are:

- `alias`: the name of the alias object under which to import the
  package(s). Can be given as a single string or as a formula with a
  single term. To keep aliases easily distinguishable from other objects
  that can also be subset with the `$` operator, I recommend ending all
  alias names with a dot (.).
- `main_package`: the name (string) of the main package to import.
- `re_exports`: Some R packages export functions that are not defined in
  their own package, but in their direct dependencies - “re-exports”. If
  `TRUE` (default), the re-exports of the `main_package` are added to
  the alias, analogous to the behaviour of base R’s `::` operator. If
  `FALSE`, re-exports are not added.
- `dependencies`: an optional character vector giving the dependencies
  of the `main_package` to import under the alias also.
- `extensions`: an optional character vector giving the extensions of
  the `main_package` to import under the same alias also.
- `lib.loc`: the library paths to look for the packages; defaults to
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html). This argument
  is present in all `import_` - functions.

Here is one example. Lets import
[data.table](https://github.com/Rdatatable/data.table) and its
extensions [tidytable](https://github.com/markfairbanks/tidytable),
under the same alias, which I will call “tdt.” (for “tidy data.table”):

``` r
import_as(~ tdt., "data.table", extensions = "tidytable") # this creates the tdt. object
#> Importing packages and registering methods...
#> Done
#> You can now access the functions using `tdt.$`
#> For conflicts report, packages order, and other attributes, run `attr.import(tdt.)`
```

Now one can use the imported functions using: `tdt.$some_function()`.

 

## import_inops

When aliasing an R package, infix operators are also imported in the
alias. However, it may be cumbersome to use them from the alias. For
example this:

``` r
import_as(~ to., "tinycodet")
to.$`%row~%`(x, mat)
```

or this:

``` r
tinycodet::`%row~%`(x, mat)
```

is very cumbersome.

Therefore, `tinycodet` also adds the
[`import_inops()`](https://tony-aw.github.io/tinycodet/reference/import_inops.md)
function, which exposes the infix operators. The infix operators are
exposed to the current environment, but does not attach the functions to
the namespace.

For example, to expose the infix operators in the alias object from
before to the current environment, one can do the following:

``` r
import_inops(expose = tdt.)
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
```

One can give the `unexpose` argument instead of the `expose` argument,
which will delete the infix operators from those packages/package alias
exposed in the current environment by
[`import_inops()`](https://tony-aw.github.io/tinycodet/reference/import_inops.md).
Infix operators defined by the user will not be touched. For example:

``` r
import_inops(unexpose = tdt.)
#> Removing the following infix operators:
#> %plike%, %chin%, :=, %inrange%, %in%, %between%, %ilike%, %flike%, %notin%, %like%
#> Done
```

One can also expose and unexpose the infix operators directly from a
package, instead of via an alias object. In that case the package name
must be given as a string.

For example, the following code exposes the infix operators from the
[data.table](https://github.com/Rdatatable/data.table) R package:

``` r
import_inops(expose ="data.table")
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
```

And similarly one can remove the exposed infix operators again from the
current environment as follows:

``` r
import_inops(unexpose = "data.table")
#> Removing the following infix operators:
#> %ilike%, %notin%, %between%, :=, %plike%, %flike%, %like%, %chin%, %inrange%
#> Done
```

 

The
[`import_inops()`](https://tony-aw.github.io/tinycodet/reference/import_inops.md)
function has the `exclude` and `include.only` arguments to specify
exactly which infix operators to expose to the current environment, as
well as the `overwrite` and `inherits` arguments to specify what to do
when the infix operators you are about to expose already exist in the
current environment (and loaded namespaces). This can be handy to
prevent overwriting any (user defined) infix operators already present
in the current environment or loaded namespaces.

Examples:

``` r
import_inops(expose = tdt., include.only = ":=")
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
import_inops(unexpose = tdt.)
#> Removing the following infix operators:
#> :=
#> Done
import_inops(expose = "data.table", , include.only = ":=")
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
import_inops(unexpose = "data.table")
#> Removing the following infix operators:
#> :=
#> Done
```

 

If the user would rather attach the infix operators to the (global)
namespace, `tinycodet` provides the
[`pkg_lsf()`](https://tony-aw.github.io/tinycodet/reference/pkgs.md)
function, which returns a character vector listing all functions or
infix operators from a package. This vector can then be used in the
`include.only` argument of the
[`library()`](https://rdrr.io/r/base/library.html) function. Like so:

``` r
library(magrittr, include.only = pkg_lsf("magrittr", type = "inops"))
```

 

## import_data

The
[`import_as()`](https://tony-aw.github.io/tinycodet/reference/import_as.md)
and
[`import_inops()`](https://tony-aw.github.io/tinycodet/reference/import_inops.md)
functions get all functions from the package namespace. But packages
often also have data sets, which are often not part of the namespace.

The [`data()`](https://rdrr.io/r/utils/data.html) function in core R can
already load data from packages, but this function loads the data into
the global environment, instead of returning the data directly, making
assigning the data to a specific variable a bit annoying. Therefore, the
`tinycodet` package introduces the
[`import_data()`](https://tony-aw.github.io/tinycodet/reference/import_data.md)
function, which directly returns a data set from a package.

For example, to import the `chicago` data set from the
[gamair](https://github.com/cran/gamair) R package, and assign it
directly to a variable (without having to do re-assignment and so on),
one simply runs the following:

``` r
d <- import_data("gamair", "chicago")
head(d)
#>   death pm10median pm25median  o3median  so2median    time tmpd
#> 1   130 -7.4335443         NA -19.59234  1.9280426 -2556.5 31.5
#> 2   150         NA         NA -19.03861 -0.9855631 -2555.5 33.0
#> 3   101 -0.8265306         NA -20.21734 -1.8914161 -2554.5 33.0
#> 4   135  5.5664557         NA -19.67567  6.1393413 -2553.5 29.0
#> 5   126         NA         NA -19.21734  2.2784649 -2552.5 32.0
#> 6   130  6.5664557         NA -17.63400  9.8585839 -2551.5 40.0
```

 

## When to use or not to use the new import system

The ‘tinycodet’ import system is helpful particularly for packages that
have at least one of the following properties:

- The namespace of the package(s) conflicts with other packages.

- The namespace of the package(s) conflicts with core R, or with those
  of recommended R packages.

- The package(s) have function names that are generic enough, such that
  it is not obvious which function came from which package.

There is no necessity for using the ‘tinycodet’ import system with every
single package. One can safely attach the ‘stringi’ package, for
example, as ‘stringi’ uses a unique and immediately recognisable naming
scheme (virtually all ‘stringi’ functions start with “stri\_”), and this
naming scheme does not conflict with core R, nor with most other
packages.

Of course, if one wishes to use ‘stringi’ only within a specific
environment, it becomes advantageous to import ‘stringi’ using the
‘tinycodet’ import system. In that case the
[`import_LL()`](https://tony-aw.github.io/tinycodet/reference/import_misc.md)
function would be most applicable.

 

## Function attributes

All the functions imported by
[`import_as()`](https://tony-aw.github.io/tinycodet/reference/import_as.md),
[`import_inops()`](https://tony-aw.github.io/tinycodet/reference/import_inops.md),
and
[`import_LL()`](https://tony-aw.github.io/tinycodet/reference/import_misc.md)
functions will have a “package” attribute, so you will always know which
function came from which package.

For example:

``` r
import_inops("magrittr")
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
attributes(`%>%`)
#> $package
#> [1] "magrittr"
#> 
#> $function_name
#> [1] "%>%"
#> 
#> $tinyimport
#> [1] "tinyimport"
#> 
#> $class
#> [1] "function"   "tinyimport"
```

 

## Note for R-Package Developers

It goes without saying, just like one should **NEVER** use
[`library()`](https://rdrr.io/r/base/library.html) or
[`require()`](https://rdrr.io/r/base/library.html) inside an R-package,
similarly, one should **NOT** use tinycodet’s import functions inside an
R-Package.

The import functions can still be used inside functions defined in a
file to be sourced, though. Just not in functions inside an R-package.

 

## Example

One R package that could benefit from the import system introduced by
`tinycodet`, is the [dplyr](https://github.com/tidyverse/dplyr) R
package. The [dplyr](https://github.com/tidyverse/dplyr) R package
overwrites **core R** functions (including base R) and it overwrites
functions from pre-installed recommended R packages (such as `MASS`).
I.e.:

``` r
rm(list=ls()) # clearing environment again
library(MASS)
library(dplyr) # <- notice dplyr overwrites base R and recommended R packages
#> 
#> Attaching package: 'dplyr'
#> The following object is masked from 'package:MASS':
#> 
#>     select
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# detaching dplyr again:
detach("package:dplyr")
```

Moreover, [dplyr](https://github.com/tidyverse/dplyr)’s function names
are sometimes generic enough that there is no obvious way to tell if a
function came from [dplyr](https://github.com/tidyverse/dplyr) or some
other package (for comparison: one can generally recognize `stringi`
functions as they all start with `stri_`). If you look at the CRAN page
for [dplyr](https://github.com/tidyverse/dplyr), you’ll notice it has
some interesting extensions you might want to use, such as
[powerjoin](https://github.com/moodymudskipper/powerjoin).

To prevent masking base R functions, and to prevent obscurity regarding
which functions come from [dplyr](https://github.com/tidyverse/dplyr)
and [powerjoin](https://github.com/moodymudskipper/powerjoin), and which
functions come from core R, one could constantly use `dplyr::` and
`powerjoin::`. But constantly switching between package prefixes or
aliases is perhaps undesirable.

So here `tinycodet`’
[`import_as()`](https://tony-aw.github.io/tinycodet/reference/import_as.md)
function might help. Below is an example where
[dplyr](https://github.com/tidyverse/dplyr) is imported (including its
re-exports), along with
[powerjoin](https://github.com/moodymudskipper/powerjoin) (which is an
extension), all under one alias which I’ll call “`dpr.`”. Moreover, all
infix operators from `magrittr` are exposed to the current environment.

``` r
import_as(
  ~ dpr., "dplyr", extensions = "powerjoin", lib.loc = .libPaths()
)
#> Importing packages and registering methods...
#> Done
#> You can now access the functions using `dpr.$`
#> For conflicts report, packages order, and other attributes, run `attr.import(dpr.)`

import_inops("magrittr") # getting the infix operators from `magrittr`
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done
```

The functions from [dplyr](https://github.com/tidyverse/dplyr) can now
be used with the `dpr.$` prefix. This way, base R functions are no
longer overwritten, and it will be clear for someone who reads your code
whether functions like the
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) function
is the base R filter function, or the
[dplyr](https://github.com/tidyverse/dplyr) filter function, as the
latter would be called as `dpr.$filter()`.

Let’s first run a simple example code with the imported functions:

``` r
d <- import_data("dplyr", "starwars")
d %>%
  dpr.$filter(.data$species == "Droid") %>% # notice the ".data" pronoun can be used without problems
  dpr.$select(name, dpr.$ends_with("color"))
#> # A tibble: 6 × 4
#>   name   hair_color skin_color  eye_color
#>   <chr>  <chr>      <chr>       <chr>    
#> 1 C-3PO  NA         gold        yellow   
#> 2 R2-D2  NA         white, blue red      
#> 3 R5-D4  NA         white, red  red      
#> 4 IG-88  none       metal       red      
#> 5 R4-P17 none       silver, red red, blue
#> 6 BB8    none       none        black
```

Just add `dpr.$` in front of the functions you’d normally use, and
everything works just as expected.

Now lets run an example from the
[powerjoin](https://github.com/moodymudskipper/powerjoin) GitHub page
(<https://github.com/moodymudskipper/powerjoin>), using the above alias:

``` r
male_penguins <- dpr.$tribble(
     ~name,    ~species,     ~island, ~flipper_length_mm, ~body_mass_g,
 "Giordan",    "Gentoo",    "Biscoe",               222L,        5250L,
  "Lynden",    "Adelie", "Torgersen",               190L,        3900L,
  "Reiner",    "Adelie",     "Dream",               185L,        3650L
)

female_penguins <- dpr.$tribble(
     ~name,    ~species,  ~island, ~flipper_length_mm, ~body_mass_g,
  "Alonda",    "Gentoo", "Biscoe",               211,        4500L,
     "Ola",    "Adelie",  "Dream",               190,        3600L,
"Mishayla",    "Gentoo", "Biscoe",               215,        4750L,
)
dpr.$check_specs()
#> # powerjoin check specifications
#> ℹ implicit_keys
#> → column_conflict
#> → duplicate_keys_left
#> → duplicate_keys_right
#> → unmatched_keys_left
#> → unmatched_keys_right
#> → missing_key_combination_left
#> → missing_key_combination_right
#> → inconsistent_factor_levels
#> → inconsistent_type
#> → grouped_input
#> → na_keys

dpr.$power_inner_join(
  male_penguins[c("species", "island")],
  female_penguins[c("species", "island")]
)
#> Joining, by = c("species", "island")
#> # A tibble: 3 × 2
#>   species island
#>   <chr>   <chr> 
#> 1 Gentoo  Biscoe
#> 2 Gentoo  Biscoe
#> 3 Adelie  Dream
```

Notice that the only change made, is that all functions start with
`dpr.$`, the rest is the same. No need for constantly switching between
`dplyr::...`, `powerjoin::...` and so on - yet it is still clear from
the code that the functions came from the
[dplyr](https://github.com/tidyverse/dplyr) +
[powerjoin](https://github.com/moodymudskipper/powerjoin) family, and
there is no fear of overwriting functions from other R packages - let
alone core R functions.

 
