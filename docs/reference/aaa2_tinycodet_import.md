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
  
(6) Minimise typing - especially for infix operators  
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

- [import_as](https://tony-aw.github.io/tinycodet/reference/import_as.md):
  Import a main package, and optionally its re-exports + its direct
  dependencies + its direct extensions, under a single alias. This
  essentially combines the attaching advantage of using multiple related
  packages (item 7 on the list), whilst keeping most advantages of using
  without attaching a package.

- [import_inops](https://tony-aw.github.io/tinycodet/reference/import_inops.md):
  Expose infix operators from a package or an alias object to the
  current environment. This gains the attaching advantage of less typing
  (item 6 on the list), whilst simultaneously avoiding the disadvantage
  of attaching functions from a package globally (item 4 on the list).

- [import_data](https://tony-aw.github.io/tinycodet/reference/import_data.md):
  Directly return a data set from a package, to allow straight-forward
  assignment.

Furthermore, there are two miscellaneous `import_` - functions:
[import_LL](https://tony-aw.github.io/tinycodet/reference/import_misc.md)
and
[import_int](https://tony-aw.github.io/tinycodet/reference/import_misc.md).  
  
The import system also includes general helper functions:

- The
  [x.import](https://tony-aw.github.io/tinycodet/reference/import_helper.md)
  functions: helper functions specifically for the 'tinycodet' import
  system.

- The
  [pversion](https://tony-aw.github.io/tinycodet/reference/pversion.md)\_
  functions: check mismatch between loaded package version and package
  version in library path.

- The [pkgs](https://tony-aw.github.io/tinycodet/reference/pkgs.md) -
  functions: general helper functions regarding packages.

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
the package using the 'tinycodet' import system. In that case the
[import_LL](https://tony-aw.github.io/tinycodet/reference/import_misc.md)
function would be most applicable.  
  
  
**Some Additional Comments on the 'tinycodet' Import System**  

- (S3) Methods will automatically be registered.

- Pronouns, such as the `.data` and `.env` pronouns from the 'rlang'
  package, will work without any prefixes required.

- All functions imported by the
  [import_as](https://tony-aw.github.io/tinycodet/reference/import_as.md),
  [import_inops](https://tony-aw.github.io/tinycodet/reference/import_inops.md),
  or
  [import_LL](https://tony-aw.github.io/tinycodet/reference/import_misc.md)
  functions have a "package" attribute, so you will always know which
  function came from which package.  
    

**For R Package Developers**  
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

# NO packages are being attached in any of the following code

# import 'dplyr' + its re-exports + extension 'powerjoin', under alias "dpr.":
import_as(
  ~ dpr., "dplyr", re_exports = TRUE, extensions = "powerjoin"
)
#> Importing packages and registering methods...
#> Done
#> You can now access the functions using `dpr.$`
#> For conflicts report, packages order, and other attributes, run `attr.import(dpr.)` 

# exposing infix operators from 'magrrittr' to current environment:
import_inops("magrittr")
#> Checking for conflicting infix operators in the current environment...
#> Placing infix operators in current environment...
#> Done

# directly assigning dplyr's "starwars" dataset to object "d":
d <- import_data("dplyr", "starwars")

# See it in Action:
d %>% dpr.$filter(species == "Droid") %>%
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

mypaste <- function(x, y) {
  import_LL("stringi", selection = "stri_c")
  stringi::stri_c(x, y)
}
mypaste("hello ", "world")
#> exposing and locking functions to current environment ...
#> Done
#> [1] "hello world"

# }
```
