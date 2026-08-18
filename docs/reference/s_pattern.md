# Pattern Specifications for String Related Operators

The [%s-%, %s/%,
%ss%](https://tony-aw.github.io/tinycodet/reference/str_arithmetic.md)
operators, as well as the string search operators
([str_search](https://tony-aw.github.io/tinycodet/reference/str_search.md)),
perform pattern matching for some purpose, where the pattern is given in
the second argument (`p`).  
When a character vector or string is given as the second argument (`p`),
this is interpreted as case-sensitive `regex` patterns from 'stringi'.  
  
Instead of giving a string or character vector of regex patterns, one
can also supply a list to specify exactly how the pattern should be
interpreted. The list should use the exact same argument convention as
'stringi'.  
  
For example:

- `list(regex = p, case_insensitive = FALSE, ...)`

- `list(fixed = p, ...)`

- `list(coll = p, ...)`

- `list(charclass = p, ...)`

All arguments in the list are simply passed to the appropriate functions
in 'stringi'.  
For example:

    x %s/% p

counts how often regular expression specified in character vector `p`
occurs in `x`, whereas the following,

    x %s/% list(fixed = p, case_insensitive = TRUE)

will do the same, except it uses fixed (i.e. literal) expression, and it
does not distinguish between upper case and lower case characters.  
  
'tinycodet' adds some convenience functions based on the `stri_opts_` -
functions in 'stringi':

- `s_regex(p, ...)` is equivalent to `list(regex = p, ...)`

- `s_fixed(p, ...)` is equivalent to `list(fixed = p, ...)`

- `s_coll(p, ...)` is equivalent to `list(coll = p, ...)`

- `s_chrcls(p, ...)` is equivalent to `list(charclass = p, ... )`

With the ellipsis (`...`) being passed to the appropriate
'stringi'-functions when it matches their arguments.  
  
'stringi' infix operators start with "`%s`", though they all have an
alias starting with "`%stri`". In analogy to that, the above functions
start with "`s_`" rather than "`stri_`", as they are all meant for
operators only.  

## Usage

``` r
s_regex(
  p,
  case_insensitive,
  comments,
  dotall,
  multiline,
  time_limit,
  stack_limit,
  ...
)

s_fixed(p, case_insensitive, overlap, ...)

s_coll(
  p,
  locale,
  strength,
  alternate_shifted,
  french,
  uppercase_first,
  case_level,
  numeric,
  normalization,
  ...
)

s_chrcls(p, ...)
```

## Arguments

- p:

  a character vector giving the pattern to search for.  
  [![\[REGEX\]](figures/aboutsearch-regex-darkred.svg)](https://stringi.gagolewski.com/rapi/about_search_regex.html)  
  [![\[FIXED\]](figures/aboutsearch-fixed-darkgreen.svg)](https://stringi.gagolewski.com/rapi/about_search_fixed.html)  
  [![\[COLL\]](figures/aboutsearch-coll-pink.svg)](https://stringi.gagolewski.com/rapi/about_search_coll.html)  
  [![\[CHARCLASS\]](figures/aboutsearch-charclass-lightyellow.svg)](https://stringi.gagolewski.com/rapi/about_search_charclass.html)  

- case_insensitive:

  see
  [stri_opts_regex](https://rdrr.io/pkg/stringi/man/stri_opts_regex.html)
  and
  [stri_opts_fixed](https://rdrr.io/pkg/stringi/man/stri_opts_fixed.html).

- comments, dotall, multiline:

  see
  [stri_opts_regex](https://rdrr.io/pkg/stringi/man/stri_opts_regex.html).

- time_limit, stack_limit:

  see
  [stri_opts_regex](https://rdrr.io/pkg/stringi/man/stri_opts_regex.html).

- ...:

  additional arguments not part of the `stri_opts` - functions to be
  passed here.  
  For example: the `at` argument for the
  [str_search](https://tony-aw.github.io/tinycodet/reference/str_search.md)
  operators.

- overlap:

  see
  [stri_opts_fixed](https://rdrr.io/pkg/stringi/man/stri_opts_fixed.html).

- locale, strength, alternate_shifted:

  see
  [stri_opts_collator](https://rdrr.io/pkg/stringi/man/stri_opts_collator.html).

- french, normalization, numeric:

  see
  [stri_opts_collator](https://rdrr.io/pkg/stringi/man/stri_opts_collator.html).

- uppercase_first, case_level:

  see
  [stri_opts_collator](https://rdrr.io/pkg/stringi/man/stri_opts_collator.html).

## Value

A list with arguments to be passed to the appropriate operators.

## See also

[tinycodet_strings](https://tony-aw.github.io/tinycodet/reference/aaa3_tinycodet_strings.md)

## Examples

``` r

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
p <- rep("a|e|i|o|u", 2) # same as p <- list(regex = rep("a|e|i|o|u", 2))
x %s/% p # count how often vowels appear in each string of vector x.
#> [1] 3 2

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
x %s/% list(regex = rep("A|E|I|O|U", 2), case_insensitive = TRUE)
#> [1] 3 2
x %s/% s_regex(rep("A|E|I|O|U", 2), case_insensitive = TRUE)
#> [1] 3 2


x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
p <- list(fixed = c("A", "A"), case_insensitive = TRUE)
x %s{}% p
#> [1]  TRUE FALSE
x %s!{}% p
#> [1] FALSE  TRUE
p <- s_fixed(c("A", "A"), case_insensitive = TRUE)
x %s{}% p
#> [1]  TRUE FALSE
x %s!{}% p
#> [1] FALSE  TRUE

x <- c(paste0(letters[1:13], collapse = ""),
      paste0(letters[14:26], collapse = ""), NA)
p <- s_fixed("abc", at = "start")
x %s{}% p
#> [1]  TRUE FALSE    NA
stringi::stri_startswith(x, fixed = "abc") # same as above
#> [1]  TRUE FALSE    NA

p <- s_fixed("xyz", at = "end")
x %s{}% p
#> [1] FALSE  TRUE    NA
stringi::stri_endswith(x, fixed = "xyz") # same as above
#> [1] FALSE  TRUE    NA
```
