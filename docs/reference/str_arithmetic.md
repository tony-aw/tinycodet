# String Arithmetic Operators

String arithmetic operators.  
  
The ` x %s+% y ` operator is exported from 'stringi', and concatenates
character vectors `x` and `y`.  
  
The ` x %s-% p ` operator removes character/pattern defined in `p` from
`x`.  
  
The ` x %s*% n ` operator is exported from 'stringi', and duplicates
each string in `x` `n` times, and concatenates the results.  
  
The ` x %s/% p ` operator counts how often character/pattern defined in
`p` occurs in each element of `x`.  
  
The ` x %s//% brk ` operator counts how often the text boundary
specified in list `brk` occurs in each element of `x`.  
  
The ` e1 %s$% e2 ` operator is exported from 'stringi', and provides
access to
[stri_sprintf](https://rdrr.io/pkg/stringi/man/stri_sprintf.html) in the
form of an infix operator.  
  
The ` x %ss% p ` operator splits the strings in `x` by a delimiter
character/pattern defined in `p`, and removes `p` in the process.  
For cutting strings by text boundaries, or around a location, see
[strcut_brk](https://tony-aw.github.io/tinycodet/reference/strcut.md)
and
[strcut_loc](https://tony-aw.github.io/tinycodet/reference/strcut.md).  

## Usage

``` r
x %s-% p

x %s/% p

x %s//% brk

x %ss% p
```

## Arguments

- x:

  a string or character vector.

- p:

  either a list with 'stringi' arguments (see
  [s_pattern](https://tony-aw.github.io/tinycodet/reference/s_pattern.md)),
  or else a character vector with regular expressions.  
  [![\[REGEX\]](figures/aboutsearch-regex-darkred.svg)](https://stringi.gagolewski.com/rapi/about_search_regex.html)  
  [![\[FIXED\]](figures/aboutsearch-fixed-darkgreen.svg)](https://stringi.gagolewski.com/rapi/about_search_fixed.html)  
  [![\[COLL\]](figures/aboutsearch-coll-pink.svg)](https://stringi.gagolewski.com/rapi/about_search_coll.html)  
  [![\[CHARCLASS\]](figures/aboutsearch-charclass-lightyellow.svg)](https://stringi.gagolewski.com/rapi/about_search_charclass.html)  

- brk:

  a list with break iteration options, like a list produced by
  [stri_opts_brkiter](https://rdrr.io/pkg/stringi/man/stri_opts_brkiter.html).  
  [![\[BOUNDARIES\]](figures/aboutsearch-boundaries-blue.svg)](https://stringi.gagolewski.com/rapi/about_search_boundaries.html)  

## Value

The `%s+%`, `%s-%`, and `%s*%` operators return a character vector of
the same length as `x`.  
The `%s/%` and `%s//%` both return an integer vector of the same length
as `x`.  
The `%s$%` operator returns a character vector.  
The `%ss%` operator returns a list of the split strings - or, if
`simplify = TRUE` / `simplify = NA`, returns a matrix of the split
strings.

## See also

[tinycodet_strings](https://tony-aw.github.io/tinycodet/reference/aaa3_tinycodet_strings.md)

## Examples

``` r
x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
y <- c("a", "b")
p <- rep("a|e|i|o|u", 2) # same as p <- list(regex = rep("a|e|i|o|u", 2))
n <- c(3, 2)

x %s+% y # = paste0(x,y)
#> [1] "abcdefghijklma" "nopqrstuvwxyzb"
x %s-% p # remove all vowels from x
#> [1] "bcdfghjklm"  "npqrstvwxyz"
x %s*% n
#> [1] "abcdefghijklmabcdefghijklmabcdefghijklm"
#> [2] "nopqrstuvwxyznopqrstuvwxyz"             
x %s/% p # count how often vowels appear in each string of vector x
#> [1] 3 2
x %ss% p # split x around vowels, removing the vowels in the process
#> [[1]]
#> [1] ""     "bcd"  "fgh"  "jklm"
#> 
#> [[2]]
#> [1] "n"     "pqrst" "vwxyz"
#> 
x %ss% s_regex(p, simplify = NA) # same as above, but in matrix form
#>      [,1] [,2]    [,3]    [,4]  
#> [1,] ""   "bcd"   "fgh"   "jklm"
#> [2,] "n"  "pqrst" "vwxyz" NA    

test <- c(
  paste0("The\u00a0above-mentioned    features are very useful. ",
  "Spam, spam, eggs, bacon, and spam. 123 456 789"),
  "good morning, good evening, and good night"
)
test %s//% list(type = "character")
#> [1] 95 42


x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
y <- "a"
# pattern that ignores case:
p <- list(regex = rep("A|E|I|O|U", 2), case_insensitive = TRUE)
n <- c(2, 3)

x %s+% y # = paste0(x,y)
#> [1] "abcdefghijklma" "nopqrstuvwxyza"
x %s-% p # remove all vowels from x
#> [1] "bcdfghjklm"  "npqrstvwxyz"
x %s*% n
#> [1] "abcdefghijklmabcdefghijklm"             
#> [2] "nopqrstuvwxyznopqrstuvwxyznopqrstuvwxyz"
x %s/% p # count how often vowels appears in each string of vector x.
#> [1] 3 2

x <- c(paste(letters, collapse = ", "), paste(LETTERS, collapse = ", "))
print(x)
#> [1] "a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z"
#> [2] "A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z"
x %ss% ", "
#> [[1]]
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> 
#> [[2]]
#>  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
#> [20] "T" "U" "V" "W" "X" "Y" "Z"
#> 
t(x %ss% s_fixed(", ", simplify = NA))
#>       [,1] [,2]
#>  [1,] "a"  "A" 
#>  [2,] "b"  "B" 
#>  [3,] "c"  "C" 
#>  [4,] "d"  "D" 
#>  [5,] "e"  "E" 
#>  [6,] "f"  "F" 
#>  [7,] "g"  "G" 
#>  [8,] "h"  "H" 
#>  [9,] "i"  "I" 
#> [10,] "j"  "J" 
#> [11,] "k"  "K" 
#> [12,] "l"  "L" 
#> [13,] "m"  "M" 
#> [14,] "n"  "N" 
#> [15,] "o"  "O" 
#> [16,] "p"  "P" 
#> [17,] "q"  "Q" 
#> [18,] "r"  "R" 
#> [19,] "s"  "S" 
#> [20,] "t"  "T" 
#> [21,] "u"  "U" 
#> [22,] "v"  "V" 
#> [23,] "w"  "W" 
#> [24,] "x"  "X" 
#> [25,] "y"  "Y" 
#> [26,] "z"  "Z" 
```
