# 

## Introduction

The `tinycodet` R-package adds some functions to help in your coding
etiquette. It primarily focuses on 4 aspects:

1.  Safer decimal (in)equality testing, standard-evaluated alternatives
    to [`with()`](https://rdrr.io/r/base/with.html) and `aes()`, and
    other functions for safer coding.
2.  A new package import system, that attempts to combine the benefits
    of using a package without attaching, with the benefits of attaching
    a package.
3.  Extending the string manipulation capabilities of the `stringi` R
    package.
4.  Reducing repetitive code.

The `tinycodet` R-package has only one dependency, namely `stringi`.
Most functions in this R-package are fully vectorized and optimized, and
have been well documented.

Here I’ll give a quick glimpse of what is possible in this R package.

 

## Safer functionality

‘tinycodet’ adds some functions to help in coding more safely:

- [`with_pro()`](https://tony-aw.github.io/tinycodet/reference/pro.md)
  and
  [`aes_pro()`](https://tony-aw.github.io/tinycodet/reference/pro.md)
  are standard-evaluated alternatives to
  [`base::with()`](https://rdrr.io/r/base/with.html) and
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  These use formulas as input.
- In base R, `(0.1*3) == 0.3` gives `FALSE`, due to the way decimal
  numbers are stored in programming languages like R and Python.
  `tinycodet` adds safer truth testing operators, that give correct
  results.
- One can re-assign the values `T` and `F`. One can even run
  `T <- FALSE` and `F <- TRUE`!. `tinycodet` adds the
  [`lock_TF()`](https://tony-aw.github.io/tinycodet/reference/lock.md)
  function that forces `T` to stay `TRUE` and `F` to stay `FALSE`.

One example of
[`aes_pro()`](https://tony-aw.github.io/tinycodet/reference/pro.md):

``` r
requireNamespace("ggplot2")
d <- import_data("ggplot2", "mpg")

x <- ~ cty
y <- ~ sqrt(hwy)
color <- ~ drv

ggplot2::ggplot(d, aes_pro(x, y, color = color)) +
  ggplot2::geom_point()
```

![](tinycodet_files/figure-html/unnamed-chunk-2-1.png)

 

## New import system

One can use a package without attaching the package (for example using
`::`), or one can attach a package (for example using
[`library()`](https://rdrr.io/r/base/library.html) or
[`require()`](https://rdrr.io/r/base/library.html)). The advantages and
disadvantages of using without attaching a package versus attaching a
package - at least those relevant for now - can be compactly presented
in the following table:

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

Here is an example using `tinycodet's` new import system; note that the
following code is run without attaching a single R package (besides
`tinycodet` itself of course):

``` r
# importing "tidytable" + "data.table" under alias "tdt.":
import_as( 
  ~ tdt., "tidytable", dependencies = "data.table"
)
```

    ## Importing packages and registering methods...

    ## Done
    ## You can now access the functions using `tdt.$`
    ## For conflicts report, packages order, and other attributes, run `attr.import(tdt.)`

``` r
# exposing operators from `magrrittr` to current environment:
import_inops("magrittr")
```

    ## Checking for conflicting infix operators in the current environment...

    ## Placing infix operators in current environment...

    ## Done

``` r
# directly assigning the "starwars" dataset to object "d":
d <- import_data("dplyr", "starwars") 

# see it in action:
d %>% tdt.$filter(species == "Droid") %>%
  tdt.$select(name, tdt.$ends_with("color"))
```

    ## # A tidytable: 6 × 4
    ##   name   hair_color skin_color  eye_color
    ##   <chr>  <chr>      <chr>       <chr>    
    ## 1 C-3PO  NA         gold        yellow   
    ## 2 R2-D2  NA         white, blue red      
    ## 3 R5-D4  NA         white, red  red      
    ## 4 IG-88  none       metal       red      
    ## 5 R4-P17 none       silver, red red, blue
    ## 6 BB8    none       none        black

 

## Extending the string manipulation capabilities of stringi

‘tinycodet’ adds some additional functionality to ‘stringi’ (the primary
package for string manipulation):

- [`stri_locate_ith()`](https://tony-aw.github.io/tinycodet/reference/stri_locate_ith.md):
  ‘stringi’ has functions to locate the first and last pattern
  occurrences. ‘tinycodet’ adds
  [`stri_locate_ith()`](https://tony-aw.github.io/tinycodet/reference/stri_locate_ith.md),
  which can locate the $`i^\textrm{th}`$ pattern occurrence.
- ‘stringi’ has some limited string arithmetic operators: `%s+%` and
  `%s*%`. ‘tinycodet’ enlarges this set with additional string
  arithmetic operators.
- ‘stringi’ has several string search and relation operators (`%s==%`,
  `s%!=%`). ‘tinycodet’ also enlarges this set with pattern searching
  operators (`%s{}%`, `%s!{}%`, `strfind()<-`).
- cutting strings into pieces, without removing the delimiter, lies at
  the core of (almost) all boundaries-operations in ‘stringi’. For the
  user’s convenience, ‘tinycodet’ adds the `strcut_` functions to cut
  strings in a more concise way (with less keystrokes).

 

## Reduce repetitive code

``` r
# in base R:
ifelse( # repetitive, and gives unnecessary warning
  is.na(object>0), -Inf,
  ifelse(
    object>0,  log(object), object^2
  )
)
mtcars$mpg[mtcars$cyl>6] <- (mtcars$mpg[mtcars$cyl>6])^2 # long

# with tinycodet:
object |> transform_if(\(x)x>0, log, \(x)x^2, \(x) -Inf) # compact & no warning
mtcars$mpg[mtcars$cyl>6] %:=% \(x)x^2 # short
```

 

## The Articles

If you’re still interested, I invite you to read the articles on the
website (<https://tony-aw.github.io/tinycodet/>), and perhaps try out
the package yourself.

The following articles are currently present:

- [Safer
  functionality](https://tony-aw.github.io/tinycodet/articles/a_safer.html):
  Describes the functions for safer/stricter coding.
- [Import system - main
  functions](https://tony-aw.github.io/tinycodet/articles/b_import_main.html):
  Description of the main functions of the package import system
  introduced by `tinycodet`.
- [Import system - additional
  details](https://tony-aw.github.io/tinycodet/articles/c_import_additional.html):
  Additional important details on the tinycodet import system.
- [String related
  functions](https://tony-aw.github.io/tinycodet/articles/d_strings_functions.html):
  Describes the `tinycodet` functions that extend the string
  manipulation capabilities of `stringi`.
- [String related infix
  operators](https://tony-aw.github.io/tinycodet/articles/e_strings_inops.html):
  Describes the `tinycodet` infix operators that extend the string
  manipulation capabilities of `stringi`.
- [Don’t Repeat
  Yourself](https://tony-aw.github.io/tinycodet/articles/f_DRY.html):
  Describes the `tinycodet` functions that help reduce repetitions in
  your code.
- [Miscellaneous
  functionality](https://tony-aw.github.io/tinycodet/articles/g_misc.html):
  Various other functions and operators that `tinycodet` introduces.
- [Relations to other R
  packages](https://tony-aw.github.io/tinycodet/articles/h_otherpkgs.html):
  Describes how `tinycodet` relates to other R packages, mostly
  regarding compatibility.

For a complete list of functions introduced by `tinycodet`, please see
the [References
page](https://tony-aw.github.io/tinycodet/reference/index.html).

 
