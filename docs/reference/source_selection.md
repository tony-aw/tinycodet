# Source Specific Objects from Script

The `source_selection()` function is the same as base R's
[source](https://rdrr.io/r/base/source.html) function, except that it
allows only placing the selected objects and functions into the current
environment, instead of all objects.  
  
The objects to be selected can be specified using any combination of the
following:

- by supplying a character vector of exact object names to the `select`
  argument.

- by supplying a character vector of `regex` patterns to the `regex`
  argument.

- by supplying a character vector of `fixed` patterns to the `fixed`
  argument.

Note that the `source_selection()` function does not suppress output
(i.e. plots, prints, messages) from the sourced script file.  

## Usage

``` r
source_selection(lst, select = NULL, regex = NULL, fixed = NULL)
```

## Arguments

- lst:

  a named list, giving the arguments to be passed to the
  [source](https://rdrr.io/r/base/source.html) function.  
  The `local` argument should not be included in the list.

- select:

  a character vector, giving the exact names of the functions or objects
  appearing in the script, to expose to the current environment.

- regex:

  a character vector of `regex` patterns (see
  [about_search_regex](https://rdrr.io/pkg/stringi/man/about_search_regex.html)).  
  These should give regular expressions that match to the names of the
  functions or objects appearing in the script, to expose to the current
  environment.  
  For example, to expose the following methods to the current
  environment,  
  `mymethod.numeric()` and `mymethod.character()` from generic
  `mymethod()`,  
  one could specify `regex = "^mymethod"`.  
  [![\[REGEX\]](figures/aboutsearch-regex-darkred.svg)](https://stringi.gagolewski.com/rapi/about_search_regex.html)  

- fixed:

  a character vector of `fixed` patterns (see
  [about_search_fixed](https://rdrr.io/pkg/stringi/man/about_search_fixed.html)).  
  These should give fixed expressions that match to the names of the
  functions or objects appearing in the script, to expose to the current
  environment.  
  For example, to expose the following methods to the current
  environment,  
  `mymethod.numeric()` and `mymethod.character()` from generic
  `mymethod()`,  
  one could specify `fixed = "mymethod"`.  
  [![\[FIXED\]](figures/aboutsearch-fixed-darkgreen.svg)](https://stringi.gagolewski.com/rapi/about_search_fixed.html)  

## Value

Any specified objects will be placed in the current environment.  
  

## Details

One can specify which objects to expose using arguments `select`,
`regex`, or `fixed`.  
The user can specify all 3 of them, but at least one of the 3 must be
specified.  
It is not a problem if the specifications overlap.

## See also

[tinycodet_misc](https://tony-aw.github.io/tinycodet/reference/aaa5_tinycodet_misc.md),
[`base::source()`](https://rdrr.io/r/base/source.html)

## Examples

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

source_selection(list(exprs=exprs), regex = "^mymethod")
#> Sourcing script ... 
#> Done
mymethod(1)
#> [1] 2
mymethod("a")
#> [1] "A"


temp.fun <- function(){
  source_selection(list(exprs=exprs), regex = "^mymethod", fixed = c("%", ":="))
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
#> Done
#> [1] "goodbyeworld" "helloworld"  

```
