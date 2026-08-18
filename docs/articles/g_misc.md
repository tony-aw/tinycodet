# Miscellaneous functionality

``` r
library(tinycodet)
#> Run `?tinycodet::tinycodet` to open the introduction help page of 'tinycodet'.
```

 

## source_selection

The
[`source_selection()`](https://tony-aw.github.io/tinycodet/reference/source_selection.md)
function is the same as base R’s
[`source()`](https://rdrr.io/r/base/source.html) function, except that
it allows only placing the selected objects and functions into the
current environment, instead of all objects.

The objects to be selected can be specified using any combination of the
following:

- by supplying a character vector of exact object names to the `select`
  argument.
- by supplying a character vector of `regex` patterns to the `regex`
  argument.
- by supplying a character vector of `fixed` patterns to the `fixed`
  argument.

The `regex` and `fixed` arguments are especially handy when sourcing S3
methods. For example, to expose the following methods to the current
environment, `mymethod.numeric()` and `mymethod.character()` from
generic `mymethod()`, one could specify `regex = "^mymethod"`.

Example:

``` r
exprs <- expression({
  helloworld = function()print("helloworld")
  goodbyeworld <- function() print("goodbye world")
  `%s+test%` <- function(x,y) stringi::`%s+%`(x,y)
  `%s*test%` <- function(x,y) stringi::`%s*%`(x,y)
  mymethod <- function(x) UseMethod("mymethod", x)
  mymethod.numeric <- function(x)x * 2
  mymethod.character <- function(x)chartr(x, old = "a-zA-Z", new = "A-Za-z")
})

temp.fun <- function(){
  source_selection(list(exprs=exprs), regex= "^mymethod", fixed = c("%", ":="))
  ls() # list all objects residing within the function definition
}
temp.fun()
#> Sourcing script ...
#> Done
#> [1] "%s*test%"           "%s+test%"           "mymethod"          
#> [4] "mymethod.character" "mymethod.numeric"

temp.fun <- function(){
  source_selection(list(exprs=exprs), select = c("helloworld", "goodbyeworld"))
  ls() # list all objects residing within the function definition
}
temp.fun()
#> Sourcing script ... 
#> 
#> Done
#> [1] "goodbyeworld" "helloworld"
```

 
