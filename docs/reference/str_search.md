# 'stringi' Pattern Search Operators

**The `x %s{}% p` and `x %s!{}% p` Operators:**  
The `x %s{}% p` operator checks for every string in character vector `x`
if the pattern defined in `p` is present.  
When supplying a list on the right hand side (see
[s_pattern](https://tony-aw.github.io/tinycodet/reference/s_pattern.md)),
one can optionally include the list element `at = "start"` or
`at = "end"`:

- Supplying `at = "start"` will check if the pattern appears at the
  start of a string (like
  [stri_startswith](https://rdrr.io/pkg/stringi/man/stri_startsendswith.html)).

- Supplying `at = "end"` will check if the pattern appears at the end of
  a string (like
  [stri_endswith](https://rdrr.io/pkg/stringi/man/stri_startsendswith.html)).  

The `x %s!{}% p` operator is the same as `x %s{}% p`, except it checks
for **absence** of the pattern, rather than presence.  
  
For string (in)equality operators, see
[%s==%](https://rdrr.io/pkg/stringi/man/operator_compare.html) from the
'stringi' package.  
  
  
**`strfind()<-`:**  
`strfind()<-` locates, extracts, or replaces found patterns.  
It complements the other string-related operators, and uses the same
[s_pattern](https://tony-aw.github.io/tinycodet/reference/s_pattern.md)
API.  
It functions as follows:

- `strfind()` finds all pattern matches, and returns the extractions of
  the findings in a list, just like
  [stri_extract_all](https://rdrr.io/pkg/stringi/man/stri_extract.html).

- `strfind(..., i = "all" )`, finds all pattern matches like
  [stri_locate_all](https://rdrr.io/pkg/stringi/man/stri_locate.html).

- `strfind(..., i = i)`, where `i` is an integer vector, locates the
  \\i^{th}\\ occurrence of a pattern, and reports the locations in a
  matrix, just like
  [stri_locate_ith](https://tony-aw.github.io/tinycodet/reference/stri_locate_ith.md).

- `strfind() <- value` finds pattern matches in variable `x`, replaces
  the pattern matches with the character vector specified in `value`,
  and assigns the transformed character vector back to `x`.  
  This is somewhat similar to
  [stri_replace](https://rdrr.io/pkg/stringi/man/stri_replace.html),
  though the replacement is done in-place.  
    

## Usage

``` r
x %s{}% p

x %s!{}% p

strfind(x, p, ..., i, rt)

strfind(x, p, ..., i, rt) <- value
```

## Arguments

- x:

  a string or character vector.  
  For `strfind()<-`, `x` must obviously be the variable containing the
  character vector/string, since `strfind()<-` performs assignment
  in-place.  

- p:

  either a list with 'stringi' arguments (see
  [s_pattern](https://tony-aw.github.io/tinycodet/reference/s_pattern.md)),
  or else a character vector with regular expressions.  
  See also the Details section.  
  [![\[REGEX\]](figures/aboutsearch-regex-darkred.svg)](https://stringi.gagolewski.com/rapi/about_search_regex.html)  
  [![\[FIXED\]](figures/aboutsearch-fixed-darkgreen.svg)](https://stringi.gagolewski.com/rapi/about_search_fixed.html)  
  [![\[COLL\]](figures/aboutsearch-coll-pink.svg)](https://stringi.gagolewski.com/rapi/about_search_coll.html)  
  [![\[CHARCLASS\]](figures/aboutsearch-charclass-lightyellow.svg)](https://stringi.gagolewski.com/rapi/about_search_charclass.html)  

- ...:

  additional arguments to be specified.

- i:

  either one of the following can be given for `i`:

  - if `i` is not given or `NULL`, `strfind()` extracts all found
    pattern occurrences.

  - if `i` is the string "all", `strfind()` locates all found pattern
    occurrences.

  - if `i` is an integer, `strfind()` locates the \\i^{th}\\ pattern
    occurrences.  
    See the `i` argument in
    [stri_locate_ith](https://tony-aw.github.io/tinycodet/reference/stri_locate_ith.md)
    for details.

  For `strfind() <- value`, `i` must not be specified.

- rt:

  use `rt` to specify the Replacement Type that `strfind()<-` should
  perform.  
  Either one of the following can be given for `rt`:

  - if `rt` is not given, `NULL` or `"vec"`, `strfind()<-` performs
    regular, vectorized replacement of **all** occurrences.

  - if `rt = "dict"`, `strfind()<-` performs dictionary replacement of
    **all** occurrences.  

  - if `rt = "first"`, `strfind()<-` replaces only the first
    occurrences.

  - if `rt = "last"`, `strfind()<-` replaces only the last occurrences.

  Note: `rt = "first"` and `rt = "last"` only exist for convenience; for
  more specific locational replacement, use
  [stri_locate_ith](https://tony-aw.github.io/tinycodet/reference/stri_locate_ith.md)
  or `strfind(..., i)` with numeric `i` (see the Examples section).  
  For `strfind()`, `rt` must not be specified.

- value:

  a character vector giving the replacement values.

## Value

For the `x %s{}% p` and `x %s!{}% p` operators:  
Return logical vectors.  
  
For `strfind()`:  
Returns a list with extractions of all found patterns.  
  
For `strfind(..., i = "all")`:  
Returns a list with all found pattern locations.  
  
For `strfind(..., i = i)` with integer vector `i`:  
Returns an integer matrix with two columns, giving the start and end
positions of the \\i^{th}\\ matches, two NAs if no matches are found,
and also two `NA`s if str is `NA`.  
  
For `strfind() <- value`:  
Returns nothing, but performs in-place replacement (using R's default
in-place semantics) of the found patterns in variable `x`.  
  

## Details

**Right-hand Side List for the `%s{}%` and `%s!{}%` Operators**  
When supplying a list to the right-hand side of the `%s{}%` and `%s!{}%`
operators, one can add the argument `at`.  
If `at = "start"`, the operators will check if the pattern is
present/absent at the start of the string.  
If `at = "end"`, the operators will check if the pattern is
present/absent at the end of the string.  
Unlike
[stri_startswith](https://rdrr.io/pkg/stringi/man/stri_startsendswith.html)
or
[stri_endswith](https://rdrr.io/pkg/stringi/man/stri_startsendswith.html),
`regex` **is** supported by the `%s{}%` and `%s!{}%` operators.  
See examples below.  
  

**Vectorized Replacement vs Dictionary Replacement**  

- Vectorized replacement:  
  `x`, `p`, and `value` are of the same length (or recycled to become
  the same length).  
  **All** occurrences of pattern `p[j]` in `x[j]` is replaced with
  `value[j]`, for every `j`.

- Dictionary replacement:  
  `p` and `value` are of the same length, and their length is
  independent of the length of `x`.  
  For every single string in `x`, all occurrences of pattern `p[1]` are
  replaced with `value[1]`,  
  all occurrences of pattern `p[2]` are replaced with `value[2]`, etc.  

Notice that for single replacement, i.e. `rt = "first"` or
`rt = "last"`, it makes no sense to distinguish between vectorized or
dictionary replacement, since then only a single occurrence is being
replaced per string.  
See examples below.  
  

## Note

`strfind()<-` performs in-place replacement.  
Therefore, the character vector or string to perform replacement on,
must already exist as a variable.  
So take for example the following code:

    strfind("hello", p = "e") <- "a" # this obviously does not work

    y <- "hello"
    strfind(y, p = "e") <- "a" # this works fine

In the above code, the first `strfind()<-` call does not work, because
the string needs to exist as a variable.  
  

## See also

[tinycodet_strings](https://tony-aw.github.io/tinycodet/reference/aaa3_tinycodet_strings.md)

## Examples

``` r
# example of %s{}% and %s!{}% ====

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
x %s{}% "a"
#> [1]  TRUE FALSE
x %s!{}% "a"
#> [1] FALSE  TRUE
which(x %s{}% "a")
#> [1] 1
which(x %s!{}% "a")
#> [1] 2
x[x %s{}% "a"]
#> [1] "abcdefghijklm"
x[x %s!{}% "a"]
#> [1] "nopqrstuvwxyz"
x[x %s{}% "a"] <- 1
x[x %s!{}% "a"] <- 1
print(x)
#> [1] "1" "1"

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
x %s{}% "1"
#> [1] FALSE FALSE
x %s!{}% "1"
#> [1] TRUE TRUE
which(x %s{}% "1")
#> integer(0)
which(x %s!{}% "1")
#> [1] 1 2
x[x %s{}% "1"]
#> character(0)
x[x %s!{}% "1"]
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
x[x %s{}% "1"] <- "a"
x[x %s!{}% "1"] <- "a"
print(x)
#> [1] "a" "a"

#############################################################################


# Example of %s{}% and %s!{}% with "at" argument ====

x <- c(paste0(letters, collapse = ""),
       paste0(rev(letters), collapse = ""), NA)
p <- s_fixed("abc", at = "start")
x %s{}% p
#> [1]  TRUE FALSE    NA
stringi::stri_startswith(x, fixed = "abc") # same as above
#> [1]  TRUE FALSE    NA

p <- s_fixed("xyz", at = "end")
x %s{}% p
#> [1]  TRUE FALSE    NA
stringi::stri_endswith(x, fixed = "xyz") # same as above
#> [1]  TRUE FALSE    NA

p <- s_fixed("cba", at = "end")
x %s{}% p
#> [1] FALSE  TRUE    NA
stringi::stri_endswith(x, fixed = "cba") # same as above
#> [1] FALSE  TRUE    NA

p <- s_fixed("zyx", at = "start")
x %s{}% p
#> [1] FALSE  TRUE    NA
stringi::stri_startswith(x, fixed = "zyx") # same as above
#> [1] FALSE  TRUE    NA



#############################################################################


# Example of transforming ith occurrence ====

# new character vector:
x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"

# report ith (second and second-last) vowel locations:
p <- s_regex( # vowels
  rep("A|E|I|O|U", 2),
  case_insensitive = TRUE
)
loc <- strfind(x, p, i = c(2, -2))
print(loc)
#>      start end
#> [1,]     5   5
#> [2,]     2   2

# extract ith vowels:
extr <- stringi::stri_sub(x, from = loc)
print(extr)
#> [1] "e" "o"

# replace ith vowels with numbers:
repl <- chartr("aeiou", "12345", extr) # transformation
stringi::stri_sub(x, loc) <- repl
print(x)
#> [1] "abcd2fghijklm" "n4pqrstuvwxyz"


#############################################################################


# Example of strfind for regular vectorized replacement ====

x <- rep('The quick brown fox jumped over the lazy dog.', 3)
print(x)
#> [1] "The quick brown fox jumped over the lazy dog."
#> [2] "The quick brown fox jumped over the lazy dog."
#> [3] "The quick brown fox jumped over the lazy dog."
p <- c('quick', 'brown', 'fox')
rp <- c('SLOW',  'BLACK', 'BEAR')
x %s{}% p
#> [1] TRUE TRUE TRUE
strfind(x, p)
#> [[1]]
#> [1] "quick"
#> 
#> [[2]]
#> [1] "brown"
#> 
#> [[3]]
#> [1] "fox"
#> 
strfind(x, p) <- rp
print(x)
#> [1] "The SLOW brown fox jumped over the lazy dog."  
#> [2] "The quick BLACK fox jumped over the lazy dog." 
#> [3] "The quick brown BEAR jumped over the lazy dog."

#############################################################################


# Example of strfind for dictionary replacement ====

x <- rep('The quick brown fox jumped over the lazy dog.', 3)
print(x)
#> [1] "The quick brown fox jumped over the lazy dog."
#> [2] "The quick brown fox jumped over the lazy dog."
#> [3] "The quick brown fox jumped over the lazy dog."
p <- c('quick', 'brown', 'fox')
rp <- c('SLOW',  'BLACK', 'BEAR')
# thus dictionary is:
# quick => SLOW; brown => BLACK; fox => BEAR
strfind(x, p, rt = "dict") <- rp
print(x)
#> [1] "The SLOW BLACK BEAR jumped over the lazy dog."
#> [2] "The SLOW BLACK BEAR jumped over the lazy dog."
#> [3] "The SLOW BLACK BEAR jumped over the lazy dog."


#############################################################################


# Example of strfind for first and last replacement ====

x <- rep('The quick brown fox jumped over the lazy dog.', 3)
print(x)
#> [1] "The quick brown fox jumped over the lazy dog."
#> [2] "The quick brown fox jumped over the lazy dog."
#> [3] "The quick brown fox jumped over the lazy dog."
p <- s_fixed("the", case_insensitive = TRUE)
rp <- "One"
strfind(x, p, rt = "first") <- rp
print(x)
#> [1] "One quick brown fox jumped over the lazy dog."
#> [2] "One quick brown fox jumped over the lazy dog."
#> [3] "One quick brown fox jumped over the lazy dog."

x <- rep('The quick brown fox jumped over the lazy dog.', 3)
print(x)
#> [1] "The quick brown fox jumped over the lazy dog."
#> [2] "The quick brown fox jumped over the lazy dog."
#> [3] "The quick brown fox jumped over the lazy dog."
p <- s_fixed("the", case_insensitive = TRUE)
rp <- "Some Other"
strfind(x, p, rt = "last") <- rp
print(x)
#> [1] "The quick brown fox jumped over Some Other lazy dog."
#> [2] "The quick brown fox jumped over Some Other lazy dog."
#> [3] "The quick brown fox jumped over Some Other lazy dog."



```
