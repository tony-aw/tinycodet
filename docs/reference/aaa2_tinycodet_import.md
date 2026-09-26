# Overview of the 'tinycodet' Import System

The 'tinycodet' R-package introduces a new package import system.  
  
One can **use** a package **without attaching** the package - for
example by using the [::](https://rdrr.io/r/base/ns-dblcolon.html)
operator.  
Or, one can explicitly **attach** a package - for example by using the
[library](https://rdrr.io/r/base/library.html) function.  
The advantages and disadvantages of **using without attaching** a
package versus **attaching** a package, at least those relevant here,
are compactly presented in the following list:  
  
(1) Prevent masking functions from other packages:  
![\[YES(ADVANTAGE)\]](figures/usewithoutattach-Yes(advantage)-darkgreen.svg)` `
![\[NO(DISADVANTAGE)\]](figures/attaching-No(disadvantage)-red.svg)  
  
(2) Prevent masking core R functions:  
![\[YES(ADVANTAGE)\]](figures/usewithoutattach-Yes(advantage)-darkgreen.svg)` `
![\[NO(DISADVANTAGE)\]](figures/attaching-No(disadvantage)-red.svg)  
  
(3) Clarify which function came from which package:  
![\[YES(ADVANTAGE)\]](figures/usewithoutattach-Yes(advantage)-darkgreen.svg)` `
![\[NO(DISADVANTAGE)\]](figures/attaching-No(disadvantage)-red.svg)  
  
(4) Enable functions only in current/local environment instead of
globally:  
![\[YES(ADVANTAGE)\]](figures/usewithoutattach-Yes(advantage)-darkgreen.svg)` `
![\[NO(DISADVANTAGE)\]](figures/attaching-No(disadvantage)-red.svg)  
  
(5) Prevent namespace pollution:  
![\[YES(ADVANTAGE)\]](figures/usewithoutattach-Yes(advantage)-darkgreen.svg)` `
![\[NO(DISADVANTAGE)\]](figures/attaching-No(disadvantage)-red.svg)  
  
(6) Minimise typing - especially for replacement or infix operators  
(i.e. typing `` package::`%op%`(x, y) `` instead of `x %op% y` is
cumbersome):  
![\[NO(DISADVANTAGE)\]](figures/usewithoutattach-No(disadvantage)-red.svg)` `
![\[YES(ADVANTAGE)\]](figures/attaching-Yes(advantage)-darkgreen.svg)  
  
(7) Use multiple related packages, without constantly switching between
package prefixes  
(i.e. doing `packagename1::some_function1()`;  
`packagename2::some_function2()`;  
`packagename3::some_function3()` is chaotic and cumbersome):  
![\[NO(DISADVANTAGE)\]](figures/usewithoutattach-No(disadvantage)-red.svg)` `
![\[YES(ADVANTAGE)\]](figures/attaching-Yes(advantage)-darkgreen.svg)  
  

What 'tinycodet' attempts to do with its import system, is to somewhat
find the best of both worlds. It does this by introducing the following
functions:  

- [import_from](https://tony-aw.github.io/tinycodet/reference/import_from.md):  
  Import specific objects from a package into the current or specific
  environment.

- [import_as](https://tony-aw.github.io/tinycodet/reference/import_as.md):  
  Import a main package, and optionally its re-exports + its direct
  minimal dependencies, under a single alias.  
  This essentially combines the attaching advantage of using multiple
  related packages (item 7 on the list), whilst keeping most advantages
  of using without attaching a package.

- [import_ls](https://tony-aw.github.io/tinycodet/reference/import_ls.md):  
  List names of exported objects by category (like "infix operators", or
  "replacement operators", etc.).  
  Can be used in combination with, for example,
  [library](https://rdrr.io/r/base/library.html) or
  [import_from](https://tony-aw.github.io/tinycodet/reference/import_from.md),
  to attach or expose all infix- and replacement operators at once.  
  This gives the advantage of less typing (item 6 on the above list).

- [import_data](https://tony-aw.github.io/tinycodet/reference/import_data.md):  
  Directly return a data set from a package, to allow straight-forward
  assignment.

- [import_diagnose](https://tony-aw.github.io/tinycodet/reference/import_diagnose.md):  
  Check for mismatch issues (i.e. version or `lib.loc` mismatch) between
  loaded packages and installed packages.  
    

The import system also includes general helper functions:

- The
  [x.import](https://tony-aw.github.io/tinycodet/reference/import_helper.md)
  functions:  
  Helper functions specifically for the 'tinycodet' import system.

- The [pkg](https://tony-aw.github.io/tinycodet/reference/pkgs.md) -
  functions:  
  General helper functions regarding packages.

- The
  [searchenv](https://tony-aw.github.io/tinycodet/reference/searchenv.md) -
  functions:  
  safe functions for safely adding, removing, and accessing custom
  search path environments.  
    

See the examples section below to get an idea of how the 'tinycodet'
import system works in practice. More examples can be found on the
website (<https://tony-aw.github.io/tinycodet/>)

## Details

**When to Use or Not to Use the 'tinycodet' Import System**  
The 'tinycodet' import system is helpful particularly for packages that
have at least one of the following properties:

- The namespace of the package(s) conflicts with other packages.

- The namespace of the package(s) conflicts with core R, or with those
  of recommended R packages.

- The package(s) have function names that are generic enough, such that
  it is not obvious which function came from which package.

See examples below.  
  
There is no necessity for using the 'tinycodet' import system with every
single package. One can safely attach the 'stringi' package, for
example, as 'stringi' uses a unique and immediately recognisable naming
scheme (virtually all 'stringi' functions start with "`stri_`"), and
this naming scheme does not conflict with core R, nor with most other
packages.  
  
Of course, if one wishes to use a package (like 'stringi') **only**
within a specific environment, it becomes advantageous to still import
the package using the 'tinycodet' import system.  
  

**Some Additional Comments on the 'tinycodet' Import System**  

- Methods (like S3, S4) will automatically be registered.

- Pronouns, such as the `.data` and `.env` pronouns from the 'rlang'
  package, will work without any prefixes required.

- 'tinycodet' avoids the [exists](https://rdrr.io/r/base/exists.html)
  function, to prevent memory leakage.  
    

## For R Package Developers

It goes without saying, just like one should NEVER use
[`library()`](https://rdrr.io/r/base/library.html) or
[`require()`](https://rdrr.io/r/base/library.html) inside an R-package,
similarly, one should NOT use tinycodet’s import functions inside an
R-Package.  
The import functions can still be used inside functions defined in a
file to be sourced, though.  
Just not in functions inside an R-package.  
  

## See also

[tinycodet_help](https://tony-aw.github.io/tinycodet/reference/aaa0_tinycodet_help.md)

## Examples

``` r
all(c("dplyr", "powerjoin", "magrittr") %installed in% .libPaths())
#> [1] TRUE

# \donttest{

# import dplyr, tibble, and powerjoin, under aliases:
import_as(.dpr ~ dplyr, re_exports = TRUE, deps = "tibble")
#> Import & method registration complete
import_as(.pj ~ powerjoin)
#> Import & method registration complete

# attaching only the infix operators from 'magrrittr':
library(magrittr, import_ls("magrittr", "infix") )

# directly assigning dplyr's "starwars" dataset to object "d":
d <- import_data("dplyr", "starwars")

# See it in Action:
d %>% .dpr$filter(species == "Droid") %>%
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
.pj$check_specs()
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

.pj$power_inner_join(
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

mypaste <- function(x, y) {
  import_from("stringi", "stri_c")
  stri_c(x, y)
}
mypaste("hello ", "world")
#> Import & method registration complete
#> [1] "hello world"

# }
```
