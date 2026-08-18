# String related infix operators

``` r
library(tinycodet)
#> Run `?tinycodet::tinycodet` to open the introduction help page of 'tinycodet'.
```

 

## Overview

‘tinycodet’ adds 3 sets of string-related operators.

First, sub-setting operators:

- `x %s><% ss`: returns the first `n1` and last `n2` characters from
  each string in character vector `x`.
- `x %s<>% ss` trims away first `n1` and last `n2` characters from each
  string in character vector `x`.

Second, ‘stringi’ already has the `%s+%`, `%s*%`, and `%s$%` operators,
and ‘tinycodet’ adds some additional string arithmetic operators to
complete the set:

- `x %s-% p` removes pattern `p` from each string in character vector
  `x`;
- `x %s/% p` counts how often pattern `p` occurs in each string of
  character vector `x`.
- `x %s//% brk` counts how often the text boundary specified in list
  `brk` occurs in each string of character vector `x`.
- `x %ss% p` splits the strings in `x` by a delimiter character/pattern
  defined in `p`, and removes `p` in the process.

And finally, string search operators:

- `x %s{}% p` operator checks for every string in character vector `x`
  if the pattern defined in `p` is present. Can also be used to check if
  the strings specifically start or end with pattern `p`.
- `x %s!{}% p` operator checks for every string in character vector `x`
  if the pattern defined in `p` is **not** present. Can also be used to
  check if the strings specifically **does not** start or end with
  pattern `p`.
- `strfind()<-` locates, extracts, or replaces found patterns.

 

The `x %s-% p` and `x %s/% p` operators, and the string detection
operators (`%s{}%`, `%s!{}%`, `strfind()<-`) perform pattern matching
for various purposes. When a character vector or string is given on the
right hand side, this is interpreted as case-sensitive `regex` patterns
from `stringi`.

But, of course, sometimes one wants to change this. For example, one may
want it to be case insensitive. Or perhaps one wants to use fixed
expressions, or something else.

Instead of giving a string or character vector of regex patterns, one
can also supply a list to the right-hand side, to specify exactly how
the pattern should be interpreted. The list should use the exact same
naming convention as `stringi`. For example:

- `list(regex=p, case_insensitive=FALSE, ...)`
- `list(fixed=p, ...)`
- `list(coll=p, ...)`
- `list(charclass=p, ...)`

For convenience, ‘tinycodet’ adds the following functions for this
purpose:

- `s_regex(p, ...)` is equivalent to `list(regex = p, ...)`
- `s_fixed(p, ...)` is equivalent to `list(fixed = p, ...)`
- `s_coll(p, ...)` is equivalent to `list(coll = p, ...)`
- `s_chrcls(p, ...)` is equivalent to `list(charclass = p, ... )`

The next sections will give more details on the given overview.

 

 

## String subsetting operators

The `x %s><% ss` operator returns a subset of each string in character
vector `x`. Here `ss` is a vector of length 2, or a matrix with
`nrow(ss)=length(x)` and 2 columns. The object `ss` should consist
entirely of non-negative integers (thus 0, 1, 2, etc. are valid, but -1,
-2, -3 etc are not valid). The first element/column of ss gives the
number of characters counting from the left side to be extracted from x.
The second element/column of ss gives the number of characters counting
from the right side to be extracted from x.

Here are 2 examples:

``` r
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(2,3)
x %s><% ss
#> [1] "abklm" "noxyz"

x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(1,0)
x %s><% ss
#> [1] "a" "n"
```

Thus `x %s><% ss` “gets” or extracts the given number of characters from
the left and the right, and removes the rest. There is also
`x %s<>% ss`, which is the opposite: it trims away the number of
characters from the left and right as defined in the matrix `ss`,
leaving you with whatever is left.

Here are again 2 examples:

``` r
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(2,3)
x %s<>% ss
#> [1] "cdefghij" "pqrstuvw"

x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(1,0)
x %s<>% ss
#> [1] "bcdefghijklm" "opqrstuvwxyz"
```

 

## String arithmetic

The `tinycodet` package includes 7 string arithmetic operators (3 of
which re-exported from ‘stringi’):

- `x %s+% y` concatenates `x` and `y` (exported from ‘stringi’);
- `x %s-% p` removes pattern `p` from each string in character vector
  `x`;
- `x %s*% n` repeats each string in character vector `x` for `n` times
  (exported from ‘stringi’);
- `x %s/% p` counts how often pattern `p` occurs in each string of
  character vector `x`.
- `x %s//% brk` counts how often the text boundary specified in list
  `brk` occurs in each string of character vector `x`.
- `e1 %s$% e2` provides access to `stri_sprintf` (exported from
  ‘stringi’);
- `x %ss% p` splits the strings in `x` by a delimiter character/pattern
  defined in `p`, and removes `p` in the process.

I.e.:

``` r
"Hello "%s+% " world"
#> [1] "Hello  world"
c("Hello world", "Goodbye world") %s-% " world"
#> [1] "Hello"   "Goodbye"
c("Hello world", "Goodbye world") %s-% s_fixed(" world")
#> [1] "Hello"   "Goodbye"
c("Ha", "Ho", "Hi", "Hu", "He", "Ha") %s*% 2:7
#> [1] "HaHa"           "HoHoHo"         "HiHiHiHi"       "HuHuHuHuHu"    
#> [5] "HeHeHeHeHeHe"   "HaHaHaHaHaHaHa"
c("hello World & goodbye world", "world domination!") %s/% s_fixed("world", case_insensitive = TRUE)
#> [1] 2 1
c("hello world & goodbye world", "world domination!") %s//% list(type = "word")
#> [1] 9 4
```

The right-side arguments `y`, and `n` can be a single value, or a vector
of the same length as `x`. The right-side argument `p` can be string or
character vector, or a list as described in the Overview section.

 

## Detect Patterns

### Detect

The `x %s{}% p` operator checks for every string in character vector `x`
if the pattern defined in `p` is present. The `x %s!{}% p` operator
checks for every string in character vector `x` if the pattern defined
in `p` is NOT present.

Examples:

``` r
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
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
```

### Detect - start or end with pattern

When supplying a list on the right hand side (see the Overview section
above), one can include the list element `at = "start"` or `at = "end"`:

- Supplying `at = "start"` will check if strings start with the patterns
  (see
  [`stringi::stri_startswith`](https://rdrr.io/pkg/stringi/man/stri_startsendswith.html)).
- Supplying `at = "end"` will check if strings end with the patterns
  (see
  [`stringi::stri_endswith`](https://rdrr.io/pkg/stringi/man/stri_startsendswith.html)).

Examples:

``` r

x <- c(paste0(letters, collapse=""), paste0(rev(letters), collapse=""), NA)
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
```

 

## Locate, Extract, or Replace Patterns

`strfind()<-` locates, extracts, or replaces found patterns. Like the
other operators, the argument `p` can be a string or character vector,
or a list as described in the Overview section above.

It can be used in several different ways.

 

### Extract

[`strfind()`](https://tony-aw.github.io/tinycodet/reference/str_search.md)
finds all pattern matches, and returns the extractions of the findings
in a list, just like
[`stringi::stri_extract_all()`](https://rdrr.io/pkg/stringi/man/stri_extract.html):

``` r
x <- rep('The quick brown fox jumped over the lazy dog.', 3)
p <- s_fixed(c('quick', 'brown', 'fox'))
strfind(x, p)
#> [[1]]
#> [1] "quick"
#> 
#> [[2]]
#> [1] "brown"
#> 
#> [[3]]
#> [1] "fox"
```

 

### Locate

`strfind(..., i = "all" )`, finds all pattern matches like
[`stringi::stri_locate_all()`](https://rdrr.io/pkg/stringi/man/stri_locate.html).
And `strfind(..., i = i)`, where `i` is an integer vector, locates the
ith occurrence of a pattern, and reports the locations in a matrix, just
like
[`stri_locate_ith()`](https://tony-aw.github.io/tinycodet/reference/stri_locate_ith.md):

``` r
p <- s_fixed("the", case_insensitive = TRUE)
strfind(x, p, i = "all")
#> [[1]]
#>      start end
#> [1,]     1   3
#> [2,]    33  35
#> 
#> [[2]]
#>      start end
#> [1,]     1   3
#> [2,]    33  35
#> 
#> [[3]]
#>      start end
#> [1,]     1   3
#> [2,]    33  35
strfind(x, p, i = c(1, -1, 2))
#>      start end
#> [1,]     1   3
#> [2,]    33  35
#> [3,]    33  35
```

 

### Replace

`strfind() <- value` finds pattern matches in variable `x`, replaces the
pattern matches with the character vector specified in `value`, and
assigns the transformed character vector back to `x`. This is somewhat
similar to
[`stringi::stri_replace()`](https://rdrr.io/pkg/stringi/man/stri_replace.html),
though the replacement is done in-place. It supports vectorized,
dictionary, first, and last replacement:

``` r
# vectorized replacement:
x <- rep('The quick brown fox jumped over the lazy dog.', 3)
p <- c('quick', 'brown', 'fox')
rp <- c('SLOW',  'BLACK', 'BEAR')
strfind(x, p) <- rp
print(x)
#> [1] "The SLOW brown fox jumped over the lazy dog."  
#> [2] "The quick BLACK fox jumped over the lazy dog." 
#> [3] "The quick brown BEAR jumped over the lazy dog."

# dictionary replacement:
# quick => SLOW; brown => BLACK; fox => BEAR
x <- rep('The quick brown fox jumped over the lazy dog.', 3)
p <- c('quick', 'brown', 'fox')
rp <- c('SLOW',  'BLACK', 'BEAR')
strfind(x, p, rt = "dict") <- rp
print(x)
#> [1] "The SLOW BLACK BEAR jumped over the lazy dog."
#> [2] "The SLOW BLACK BEAR jumped over the lazy dog."
#> [3] "The SLOW BLACK BEAR jumped over the lazy dog."

# first replacement:
x <- rep('The quick brown fox jumped over the lazy dog.', 3)
p <- s_fixed("the", case_insensitive = TRUE)
rp <- c('ONE')
strfind(x, p, rt = "first") <- rp
print(x)
#> [1] "ONE quick brown fox jumped over the lazy dog."
#> [2] "ONE quick brown fox jumped over the lazy dog."
#> [3] "ONE quick brown fox jumped over the lazy dog."

# last replacement:
x <- rep('The quick brown fox jumped over the lazy dog.', 3)
p <- s_fixed("the", case_insensitive = TRUE)
rp <- c('ONE')
strfind(x, p, rt = "last") <- rp
print(x)
#> [1] "The quick brown fox jumped over ONE lazy dog."
#> [2] "The quick brown fox jumped over ONE lazy dog."
#> [3] "The quick brown fox jumped over ONE lazy dog."
```

 
