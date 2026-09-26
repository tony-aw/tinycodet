# Import system - main functionality

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

- [`import_from()`](https://tony-aw.github.io/tinycodet/reference/import_from.md):
  Import specific objects from a package into the current or specific
  environment.

- [`import_as()`](https://tony-aw.github.io/tinycodet/reference/import_as.md):
  Import a main package, and optionally its re-exports + its minimal
  dependencies, under a single alias. This essentially combines the
  attaching advantage of using multiple related packages (row 7 on the
  table above), whilst keeping most advantages of using without
  attaching a package.

- [`import_ls()`](https://tony-aw.github.io/tinycodet/reference/import_ls.md):
  List names of exported objects by category (like “infix operators”, or
  “replacement operators”, etc.). Can be used in combination with, for
  example, [`library()`](https://rdrr.io/r/base/library.html) or
  [`import_from()`](https://tony-aw.github.io/tinycodet/reference/import_from.md).
  This gives the advantage of less typing (row 6 on the above table).

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

 

## import_from

The easiest to understand and most straight-forward import function is
the
[`import_from()`](https://tony-aw.github.io/tinycodet/reference/import_from.md)
function. It takes a package, and exposes the specified functions to the
current (or a user-specified) environment:

``` r
import_from("magrittr", "%>%")
#> Import & method registration complete
lsf.str() # function exists in current environment
#> %>% : function (lhs, rhs)
rm(list = lsf.str()) # remove function from current environment
```

We can add a prefix to the functions to avoid conflicts:

``` r
import_from("dplyr", "select", prefix = "dpr_")
#> Import & method registration complete
ls() # dpr_select is now available in the current environment
#> [1] "dpr_select"
```

Prefixes will not be added to infix operators or non-functions.

If you want to add the functions to your search path, rather than the
current environment, you can do so as follows:

``` r
searchenv_add("my_ops") # add new search path environment called "my_ops"
"my_ops" %in% search() # "my_ops" is now part of your search paths
#> [1] TRUE
import_from("dplyr", "select", prefix = "dpr_", env = "my_ops")
#> Import & method registration complete

# list what is present in search path "my_ops":
ls(searchenv_get("my_ops")) 
#> [1] "dpr_select"

# remove search path "my_ops":
searchenv_rm("my_ops")
```

The differences between
[`import_from()`](https://tony-aw.github.io/tinycodet/reference/import_from.md)
and using
[`library(package, include.only = ...)`](https://rdrr.io/r/base/library.html),
are as follows:

- [`import_from()`](https://tony-aw.github.io/tinycodet/reference/import_from.md)
  does not touch your search path unless **you** want it to.
- [`import_from()`](https://tony-aw.github.io/tinycodet/reference/import_from.md)
  does not trigger the `.onAttach()` function in the package, since
  you’re not attaching the package globally.
- [`import_from()`](https://tony-aw.github.io/tinycodet/reference/import_from.md)
  allows adding a prefix to the names of regular functions.

 

## import_as

The
[`import_as()`](https://tony-aw.github.io/tinycodet/reference/import_as.md)
function imports an R package + its re-exports under an alias, and also
imports any specified direct dependencies of the package under the very
same alias. It also informs the user which objects from a package will
overwrite which objects from other packages, so you will never be
surprised.

Here is one example. Lets
import[tidytable](https://github.com/markfairbanks/tidytable) and its
main dependency [data.table](https://github.com/Rdatatable/data.table),
under the same alias, which I will call “.tdt” (for “tidy data.table”):

``` r
import_as(
  .tdt ~ tidytable, deps = "data.table"
) # this creates the .tdt object
#> Import & method registration complete
```

Functions can now be accessed using the `$` operator.  
Like `.tdt$some_function()`.

 

## import_ls

When aliasing an R package, infix and replacement operators are also
imported in the alias. However, it may be cumbersome to use them from
the alias:

``` r
import_as(.to ~ tinycodet)
.to$`%row~%`(x, mat)
.to$`strfind<-`(x, ..., value)
```

It may be more convenient to attach or expose these objects separately.

This is where
[`import_ls()`](https://tony-aw.github.io/tinycodet/reference/import_ls.md)
comes in.  
[`import_ls()`](https://tony-aw.github.io/tinycodet/reference/import_ls.md)
lists all exported objects in a package of a specific **type**.  
These types are supported:

- “infix”: infix operators
- “rp”: replacement operators, including their base functions
- “nonfun”: objects that are not functions
- “reg”: regular functions.

So we can just attach only the infix operators from, for example, the
‘magrittr’ package like so:

``` r
magrittr_infix <- import_ls("magrittr", "infix")
#> c("%!>%", "%$%", "%<>%", "%>%", "%T>%")
library(magrittr, include.only = magrittr_infix)
```

Notice that the
[`import_ls()`](https://tony-aw.github.io/tinycodet/reference/import_ls.md)
function returns a character vector that can be stored in an object for
programmatic (in this case the object “magrittr_infix”), *and*
**prints** a literal piece of code.

This literal piece of code is printed for syntactical clarity.  
You see, the code
[`library(magrittr, include.only = magrittr_infix)`](https://rdrr.io/r/base/library.html)
does not make it obvious - merely from looking at your code - which
infix operators exactly are now attached. We can make it explicit by
simply copy-pasting the printed code from
[`import_ls()`](https://tony-aw.github.io/tinycodet/reference/import_ls.md)
like so:

``` r
import_ls("magrittr", "infix")
#> c("%!>%", "%$%", "%<>%", "%>%", "%T>%")
my_copypaste <- c("%!>%", "%$%", "%<>%", "%>%", "%T>%")
library(magrittr, include.only = my_copypaste)
```

Now, anyone who merely glances at your code immediately sees which
operators came from which package, and with very little effort on your
part.

For the sake of demonstration, let’s try out each type here:

``` r
import_ls("magrittr", "infix")
#> c("%!>%", "%$%", "%<>%", "%>%", "%T>%")
import_ls("stringi", "rp")
#> c("stri_datetime_add", "stri_datetime_add<-", "stri_sub", "stri_sub_all", 
#> "stri_sub_all<-", "stri_sub<-", "stri_subset", "stri_subset_charclass", 
#> "stri_subset_charclass<-", "stri_subset_coll", "stri_subset_coll<-", 
#> "stri_subset_fixed", "stri_subset_fixed<-", "stri_subset_regex", 
#> "stri_subset_regex<-", "stri_subset<-")
import_ls("bit64", "nonfun")
#> Registered S3 method overwritten by 'bit64':
#>   method          from 
#>   print.bitstring tools
#> "NA_integer64_"
import_ls("tibble", "reg")
#> c("add_case", "add_column", "add_row", "as.tibble", "as_data_frame", 
#> "as_tibble", "as_tibble_col", "as_tibble_row", "char", "column_to_rownames", 
#> "data_frame", "data_frame_", "deframe", "enframe", "frame_data", 
#> "frame_matrix", "glimpse", "has_name", "has_rownames", "is.tibble", 
#> "is_tibble", "lst", "lst_", "new_tibble", "num", "obj_sum", "remove_rownames", 
#> "repair_names", "rowid_to_column", "rownames_to_column", "set_char_opts", 
#> "set_num_opts", "set_tidy_names", "size_sum", "tbl_sum", "tibble", 
#> "tibble_", "tibble_row", "tidy_names", "tribble", "trunc_mat", 
#> "type_sum", "validate_tibble", "view")
```

 

## import_data

The
[`import_as()`](https://tony-aw.github.io/tinycodet/reference/import_as.md)
and
[`import_from()`](https://tony-aw.github.io/tinycodet/reference/import_from.md)
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

Moreover, [dplyr](https://github.com/tidyverse/dplyr)‘s function names
are sometimes generic enough that there is no obvious way to tell if a
function came from [dplyr](https://github.com/tidyverse/dplyr) or some
other package (for comparison: one can generally recognize `stringi`
functions as they all start with `stri_`). ’dplyr’ also has quite a
large set of dependencies, like ‘tibble’ which ‘dplyr’ was designed to
work along with it.

To prevent masking base R functions, and to prevent obscurity regarding
which functions come from [dplyr](https://github.com/tidyverse/dplyr) &
[tibble](https://github.com/tidyverse/tibble), and which functions come
from core R, one could constantly use `dplyr::` and `tibble::`. But
constantly switching between package prefixes or aliases is perhaps
undesirable.

So here `tinycodet`’
[`import_as()`](https://tony-aw.github.io/tinycodet/reference/import_as.md)
function might help. Below is an example where
[dplyr](https://github.com/tidyverse/dplyr) is imported (including its
re-exports), along with [tibble](https://github.com/tidyverse/tibble)
(which is a direct dependency), all under one alias which I’ll call
“`.dpr`”. Moreover, all infix operators from `magrittr` are attached:

``` r
import_as(
  .dpr ~ dplyr, deps = "tibble", lib.loc = .libPaths()
)
#> Import & method registration complete

library(magrittr, include.only = import_ls("magrittr", "infix"))
```

The functions from [dplyr](https://github.com/tidyverse/dplyr) can now
be used with the `.dpr$` prefix. This way, base R functions are no
longer overwritten, and it will be clear for someone who reads your code
whether functions like the
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) function
is the base R filter function, or the
[dplyr](https://github.com/tidyverse/dplyr) filter function, as the
latter would be called as `.dpr$filter()`.

Let’s first run a simple example code with the imported functions:

``` r
d <- import_data("dplyr", "starwars")
d %>%
  .dpr$filter(.data$species == "Droid") %>% # notice the ".data" pronoun can be used without problems
  .dpr$select(name, .dpr$ends_with("color"))
#> # A tibble: 6 × 4
#>   name   hair_color skin_color  eye_color
#>   <chr>  <chr>      <chr>       <chr>    
#> 1 C-3PO  NA         gold        yellow   
#> 2 R2-D2  NA         white, blue red      
#> 3 R5-D4  NA         white, red  red      
#> 4 IG-88  none       metal       red      
#> 5 R4-P17 none       silver, red red, blue
#> 6 BB8    none       none        black


male_penguins <- .dpr$tribble(
     ~name,    ~species,     ~island, ~flipper_length_mm, ~body_mass_g,
 "Giordan",    "Gentoo",    "Biscoe",               222L,        5250L,
  "Lynden",    "Adelie", "Torgersen",               190L,        3900L,
  "Reiner",    "Adelie",     "Dream",               185L,        3650L
)

female_penguins <- .dpr$tribble(
     ~name,    ~species,  ~island, ~flipper_length_mm, ~body_mass_g,
  "Alonda",    "Gentoo", "Biscoe",               211,        4500L,
     "Ola",    "Adelie",  "Dream",               190,        3600L,
"Mishayla",    "Gentoo", "Biscoe",               215,        4750L,
)
```

Notice that the only change made, is that all functions start with
`.dpr$`, the rest is the same. No need for constantly switching between
`dplyr::...`, `tibble::...` and so on - yet it is still clear from the
code that the functions came from the
[dplyr](https://github.com/tidyverse/dplyr) +
[tibble](https://github.com/tidyverse/tibble) family, and there is no
fear of overwriting functions from other R packages - let alone core R
functions.

 

## import_diagnose

The `import_` functions always check the specified library paths
(argument `lib.loc` in the `import_` functions) before importing a
package, and give an error if the package is not present in the
specified library paths.

However, there are many ways to load an R package: via `::`, via
[`loadNamespace()`](https://rdrr.io/r/base/ns-load.html), or via
[`library()`](https://rdrr.io/r/base/library.html)/[`require()`](https://rdrr.io/r/base/library.html)
(the latter loads *AND* attaches a package). For these other functions,
if the package is already loaded, no error will be given if the package
is not present in the specified library path. A package that is already
loaded can not always be safely unloaded.

This may, indirectly, lead to a situation where a package or one of its
recursive dependencies have been loaded from a different library path.
Worse still, it may be of the wrong version. The `::`,
[`loadNamespace()`](https://rdrr.io/r/base/ns-load.html) and
`librar()`/[`require()`](https://rdrr.io/r/base/library.html) functions
will *not* inform you about this. If your project relies on strict
library path or package version management, this is a problem.

Luckily for the user, the `import_` functions **DO** check if **any**
reverse dependencies has been loaded different library paths or has been
loaded from a different version than the version available in the
specified library paths, and warns the user if any mismatches are
found..

You can run
[`import_diagnose()`](https://tony-aw.github.io/tinycodet/reference/import_diagnose.md)
to see exactly which loaded packages are not aligned with those present
in the library path. This allows you to properly examine any problems to
your version control endeavour.

Like so:

``` r

import_diagnose()
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

 
