# Locate \\i^{th}\\ Pattern Occurrence or Text Boundary

The `stri_locate_ith()` function locates the \\i^{th}\\ occurrence of a
pattern in each string of some character vector.  
  
The `stri_locate_ith_boundaries()` function locates the \\i^{th}\\ text
boundary (like character, word, line, or sentence boundaries).  

## Usage

``` r
stri_locate_ith(str, i, ..., regex, fixed, coll, charclass)

stri_locate_ith_regex(str, pattern, i, ..., opts_regex = NULL)

stri_locate_ith_fixed(str, pattern, i, ..., opts_fixed = NULL)

stri_locate_ith_coll(str, pattern, i, ..., opts_collator = NULL)

stri_locate_ith_charclass(str, pattern, i, merge = TRUE, ...)

stri_locate_ith_boundaries(str, i, ..., opts_brkiter = NULL)
```

## Arguments

- str:

  a string or character vector.

- i:

  an integer scalar, or an integer vector of appropriate length (vector
  recycling is not supported).  
  Positive numbers count occurrences from the left/beginning of the
  strings.  
  Negative numbers count occurrences from the right/end of the
  strings.  
  I.e.:

  - `stri_locate_ith(str, i = 1, ...)`  
    gives the position (range) of the first occurrence of a pattern.

  - `stri_locate_ith(str, i = -1, ...)`  
    gives the position (range) of the last occurrence of a pattern.

  - `stri_locate_ith(str, i = 2, ...)`  
    gives the position (range) of the second occurrence of a pattern.

  - `stri_locate_ith(str, i = -2, ...)`  
    gives the position (range) of the second-last occurrence of a
    pattern.

  If `abs(i)` is larger than the number of pattern occurrences `n`, the
  first (if `i < -n`) or last (if `i > n`) instance will be given.  
  For example: suppose a string has `3` instances of some pattern;  
  then if `i >= 3` the third instance will be located,  
  and if `i <= -3` the first instance will be located.  

- ...:

  more arguments to be supplied to
  [stri_locate_all](https://rdrr.io/pkg/stringi/man/stri_locate.html) or
  [stri_locate_all_boundaries](https://rdrr.io/pkg/stringi/man/stri_locate_boundaries.html).  
  Do not supply the arguments `omit_no_match` or `get_length`, as they
  are already specified internally. Supplying these arguments anyway
  will result in an error.

- pattern, regex, fixed, coll, charclass:

  a character vector of search patterns, as in
  [stri_locate_all](https://rdrr.io/pkg/stringi/man/stri_locate.html).  
  [![\[REGEX\]](figures/aboutsearch-regex-darkred.svg)](https://stringi.gagolewski.com/rapi/about_search_regex.html)  
  [![\[FIXED\]](figures/aboutsearch-fixed-darkgreen.svg)](https://stringi.gagolewski.com/rapi/about_search_fixed.html)  
  [![\[COLL\]](figures/aboutsearch-coll-pink.svg)](https://stringi.gagolewski.com/rapi/about_search_coll.html)  
  [![\[CHARCLASS\]](figures/aboutsearch-charclass-lightyellow.svg)](https://stringi.gagolewski.com/rapi/about_search_charclass.html)  

- opts_regex, opts_fixed, opts_collator, opts_brkiter:

  named list used to tune up the selected search engine's settings.  
  see
  [stri_opts_regex](https://rdrr.io/pkg/stringi/man/stri_opts_regex.html),
  [stri_opts_fixed](https://rdrr.io/pkg/stringi/man/stri_opts_fixed.html),
  [stri_opts_collator](https://rdrr.io/pkg/stringi/man/stri_opts_collator.html),
  and
  [stri_opts_brkiter](https://rdrr.io/pkg/stringi/man/stri_opts_brkiter.html).  
  NULL for the defaults.  
  [![\[REGEX\]](figures/aboutsearch-regex-darkred.svg)](https://stringi.gagolewski.com/rapi/about_search_regex.html)  
  [![\[FIXED\]](figures/aboutsearch-fixed-darkgreen.svg)](https://stringi.gagolewski.com/rapi/about_search_fixed.html)  
  [![\[COLL\]](figures/aboutsearch-coll-pink.svg)](https://stringi.gagolewski.com/rapi/about_search_coll.html)  
  [![\[CHARCLASS\]](figures/aboutsearch-charclass-lightyellow.svg)](https://stringi.gagolewski.com/rapi/about_search_charclass.html)  
  [![\[BOUNDARIES\]](figures/aboutsearch-boundaries-blue.svg)](https://stringi.gagolewski.com/rapi/about_search_boundaries.html)  

- merge:

  logical, indicating if charclass locations should be merged or not.  
  **Details:**  
  For the `charclass` pattern type, the `stri_locate_ith()` function
  gives the start and end of **consecutive** characters by default, just
  like
  [stri_locate_all](https://rdrr.io/pkg/stringi/man/stri_locate.html).  
  To give the start and end positions of single characters, much like
  [stri_locate_first](https://rdrr.io/pkg/stringi/man/stri_locate.html)
  or
  [stri_locate_last](https://rdrr.io/pkg/stringi/man/stri_locate.html),
  set `merge = FALSE`.

## Value

The `stri_locate_ith()` function returns an integer matrix with two
columns, giving the start and end positions of the \\i^{th}\\ matches,
two `NA`s if no matches are found, and also two `NA`s if `str` is
`NA`.  
  
If an empty string or empty pattern is supplied, a warning is given and
a matrix with 0 rows is returned.  
  

## Details

The 'stringi' functions only support operations on the first, last, or
all occurrences of a pattern.  
The `stri_locate_ith()` function allows locating the \\i^{th}\\
occurrence of a pattern.  
This allows for several workflows for operating on the \\i^{th}\\
pattern occurrence.  
See also the examples section.  
  
**Extract \\i^{th}\\ Occurrence of a Pattern**  
For extracting the \\i^{th}\\ pattern occurrence:  
Locate the the \\i^{th}\\ occurrence using `stri_locate_ith()`, and then
extract it using, for example,
[stri_sub](https://rdrr.io/pkg/stringi/man/stri_sub.html).  
  
**Replace/Transform \\i^{th}\\ Occurrence of a Pattern**  
For replacing/transforming the \\i^{th}\\ pattern occurrence:

1.  Locate the the \\i^{th}\\ occurrence using `stri_locate_ith()`.

2.  Extract the occurrence using
    [stri_sub](https://rdrr.io/pkg/stringi/man/stri_sub.html).

3.  Transform or replace the extracted sub-strings.

4.  Return the transformed/replaced sub-string back, using again
    [stri_sub](https://rdrr.io/pkg/stringi/man/stri_sub.html).  
      

**Capture Groups of \\i^{th}\\ Occurrence of a Pattern**  
The `capture_groups` argument for `regex` is not supported within
`stri_locate_ith()`.  
To capture the groups of the \\i^{th}\\ occurrences:

1.  Use `stri_locate_ith()` to locate the \\i^{th}\\ occurrences without
    group capture.

2.  Extract the occurrence using
    [stri_sub](https://rdrr.io/pkg/stringi/man/stri_sub.html).

3.  Get the matched group capture on the extracted occurrences using
    [stri_match](https://rdrr.io/pkg/stringi/man/stri_match.html).  
      

## Note

**Long Vectors**  
The `stri_locate_ith`-functions do not support `long vectors` (i.e.
character vectors with more than `2^31 - 1` strings).  
  
**Performance**  
The performance of `stri_locate_ith()` is about the same as that of
[stri_locate_all](https://rdrr.io/pkg/stringi/man/stri_locate.html).  
  

## See also

[tinycodet_strings](https://tony-aw.github.io/tinycodet/reference/aaa3_tinycodet_strings.md)

## Examples

``` r
#############################################################################

# practical example: transform regex pattern ====

# input character vector:
x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"

# locate ith (second and second-last) vowel locations:
p <- rep("A|E|I|O|U", 2) # vowels
loc <- stri_locate_ith(x, c(2, -2), regex = p, case_insensitive = TRUE)
print(loc)
#>      start end
#> [1,]     5   5
#> [2,]     2   2

# extract ith vowels:
extr <- stringi::stri_sub(x, loc)
print(extr)
#> [1] "e" "o"

# transform & replace ith vowels with numbers:
repl <- chartr("aeiou", "12345", extr)
stringi::stri_sub(x, loc) <- repl

# result (notice ith vowels are now numbers):
print(x)
#> [1] "abcd2fghijklm" "n4pqrstuvwxyz"

#############################################################################


# practical example: group-capture regex pattern ====

# input character:
# first group: c(breakfast=eggs, breakfast=bacon)
# second group: c(lunch=pizza, lunch=spaghetti)
x <- c('breakfast=eggs;lunch=pizza',
       'breakfast=bacon;lunch=spaghetti',
       'no food here') # no group here
print(x)
#> [1] "breakfast=eggs;lunch=pizza"      "breakfast=bacon;lunch=spaghetti"
#> [3] "no food here"                   
       
# locate ith=2nd group:
p <- '(\\w+)=(\\w+)'
loc <- stri_locate_ith(x, i = 2, regex = p)
print(loc)
#>      start end
#> [1,]    16  26
#> [2,]    17  31
#> [3,]    NA  NA

# extract ith=2nd group:
extr <- stringi::stri_sub(x, loc)
print(extr)
#> [1] "lunch=pizza"     "lunch=spaghetti" NA               

# capture ith=2nd group:
stringi::stri_match(extr, regex = p)
#>      [,1]              [,2]    [,3]       
#> [1,] "lunch=pizza"     "lunch" "pizza"    
#> [2,] "lunch=spaghetti" "lunch" "spaghetti"
#> [3,] NA                NA      NA         

#############################################################################


# practical example: replace words using boundaries ====

# input character vector:
x <- c("good morning and good night",
"hello ladies and gentlemen")
print(x)
#> [1] "good morning and good night" "hello ladies and gentlemen" 

# report ith word locations:
loc <- stri_locate_ith_boundaries(x, c(-3, 3), type = "word")
print(loc)
#>      start end
#> [1,]    18  21
#> [2,]     7  12

# extract ith words:
extr <- stringi::stri_sub(x, from = loc)
print(extr)
#> [1] "good"   "ladies"

# transform and replace words (notice ith words have inverted case):
tf <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
stringi::stri_sub(x, loc) <- tf

# result:
print(x)
#> [1] "good morning and GOOD night" "hello LADIES and gentlemen" 


#############################################################################

# find pattern ====

extr <- stringi::stri_sub(x, from = loc)
repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
stringi::stri_sub_replace(x, loc, replacement=repl)
#> [1] "good morning and good night" "hello ladies and gentlemen" 


#############################################################################

# simple pattern ====

x <- rep(paste0(1:10, collapse = ""), 10)
print(x)
#>  [1] "12345678910" "12345678910" "12345678910" "12345678910" "12345678910"
#>  [6] "12345678910" "12345678910" "12345678910" "12345678910" "12345678910"
out <- stri_locate_ith(x, 1:10, regex = as.character(1:10))
cbind(1:10, out)
#>          start end
#>  [1,]  1     1   1
#>  [2,]  2     2   2
#>  [3,]  3     3   3
#>  [4,]  4     4   4
#>  [5,]  5     5   5
#>  [6,]  6     6   6
#>  [7,]  7     7   7
#>  [8,]  8     8   8
#>  [9,]  9     9   9
#> [10,] 10    10  11


x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
p <- rep("a|e|i|o|u", 2)
out <- stri_locate_ith(x, c(-1, 1), regex = p)
print(out)
#>      start end
#> [1,]     9   9
#> [2,]     2   2
substr(x, out[, 1], out[, 2])
#> [1] "i" "o"


#############################################################################

# ignore case pattern ====


x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
p <- rep("A|E|I|O|U", 2)
out <- stri_locate_ith(x, c(1, -1), regex = p, case_insensitive = TRUE)
substr(x, out[, 1], out[, 2])
#> [1] "a" "u"


#############################################################################

# multi-character pattern ====

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
# multi-character pattern:
p <- rep("AB", 2)
out <- stri_locate_ith(x, c(1, -1), regex = p, case_insensitive = TRUE)
print(out)
#>      start end
#> [1,]     1   2
#> [2,]    NA  NA
substr(x, out[, 1], out[, 2])
#> [1] "ab" NA  



#############################################################################

# Replacement transformation using stringi ====

x <- c("hello world", "goodbye world")
loc <- stri_locate_ith(x, c(1, -1), regex = "a|e|i|o|u")
extr <- stringi::stri_sub(x, from = loc)
repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
stringi::stri_sub_replace(x, loc, replacement = repl)
#> [1] "hEllo world"   "goodbye wOrld"


#############################################################################

# Boundaries ====

test <- c(
  paste0("The\u00a0above-mentioned    features are very useful. ",
         "Spam, spam, eggs, bacon, and spam. 123 456 789"),
  "good morning, good evening, and good night"
)

loc <- stri_locate_ith_boundaries(test, i = c(1, -1), type = "word")
stringi::stri_sub(test, from = loc)
#> [1] "The"   "night"

```
