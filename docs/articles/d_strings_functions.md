# String related functions

``` r
library(tinycodet)
#> Run `?tinycodet::tinycodet` to open the introduction help page of 'tinycodet'.
```

 

## Introduction

R’s numerical functions are generally very fast. But R’s native string
functions are somewhat slow, do not have a unified naming scheme, and
are not as comprehensive as R’s impressive numerical functions. The
primary R-package that fixes this is ‘stringi’, which many, if not most,
string related packages depend on (see the list of reverse-dependencies
on CRAN). The ‘stringr’ package, for example, is merely a thin wrapper
around ‘stringi’. As string manipulation is important to programming
languages, even those primarily focused on mathematics, ‘tinycodet’ adds
a little bit new functionality to ‘stringi’.

 

## stri_locate_ith

Suppose one wants to transform the **first** vowels in the strings of a
character vector `str`, such that all upper case vowels become lower
case, and vice-versa. One can do that completely in `stringi` + base R
as follows:

``` r

x <- c("HELLO WORLD", "goodbye world")
loc <- stringi::stri_locate_first(x, regex="a|e|i|o|u", case_insensitive=TRUE)
extr <- stringi::stri_sub(x, from=loc)
repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
stringi::stri_sub_replace(x, loc, replacement=repl)
#> [1] "HeLLO WORLD"   "gOodbye world"
```

But now suppose one wants to transform the **second-last** vowel. How
are you going to do that in an efficient way? For clear and efficient
code, `stringi` really needs some kind of “stri_locate_ith” function.
And, of course, the `tinycodet` package provides just that.

The `stri_locate_ith(str, i, ...)` function locates for every
element/string in character vector `str`, the $`i^\textrm{th}`$
occurrence of some (regex/fixed/etc) pattern. When `i` is positive, the
occurrence is counted from left to right. Negative values for `i` are
also allowed, in which case the occurrence is counted from the right to
left. But `i=0` is not allowed though. Thus, to get the **second**
occurrence of some pattern, use `i=2`, and to get the **second-last**
occurrence, use `i=-2`.

The `stri_locate_ith(str, i, ...)` function uses the exact same argument
and naming convention as `stringi`, to keep your code consistent. And
just like `stringi::stri_locate_first/last`, the
`stri_locate_ith(str, i, ...)` function is a vectorized function: `str`
and `i` as well as the pattern (`regex, fixed, coll, charclass`) can all
be different-valued vectors.

 

To transform the **second-last** occurrence, one can now use
[`stri_locate_ith()`](https://tony-aw.github.io/tinycodet/reference/stri_locate_ith.md)
in a very similar way as was done with `stri_locate_first/last`:

``` r
x <- c("HELLO WORLD", "goodbye world")

loc <- stri_locate_ith( # this part is the key-difference
  x, -2, regex="a|e|i|o|u", case_insensitive=TRUE
)

extr <- stringi::stri_sub(x, from=loc)
repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
stringi::stri_sub_replace(x, loc, replacement=repl)
#> [1] "HELLo WORLD"   "goodbyE world"
```

Notice that the code is virtually equivalent. We *only* need to change
the locate function.

 

There is also the
[`stri_locate_ith_boundaries()`](https://tony-aw.github.io/tinycodet/reference/stri_locate_ith.md)
function, which of course locates the $`i^\textrm{th}`$ text boundary.

 

## strcut\_ - functions

The `tinycodet` R package adds 2 `strcut` functions:
[`strcut_loc()`](https://tony-aw.github.io/tinycodet/reference/strcut.md)
and
[`strcut_brk()`](https://tony-aw.github.io/tinycodet/reference/strcut.md).

The
[`strcut_loc()`](https://tony-aw.github.io/tinycodet/reference/strcut.md)function
cuts every string in a character vector around a location range `loc`,
such that every string is cut into the following parts:

- the sub-string **before** `loc`;
- the sub-string at `loc` itself;
- the sub-string **after** `loc`.

The location range `loc` would usually be matrix with 2 columns, giving
the start and end points of some pattern match.

The
[`strcut_brk()`](https://tony-aw.github.io/tinycodet/reference/strcut.md)
function is basically a wrapper around
`stringi::stri_split_boundaries(..., simplify=NA)`, and with some more
conveniently named arguments.

Examples:

``` r

x <- rep(paste0(1:10, collapse=""), 10)
print(x)
#>  [1] "12345678910" "12345678910" "12345678910" "12345678910" "12345678910"
#>  [6] "12345678910" "12345678910" "12345678910" "12345678910" "12345678910"
loc <- stri_locate_ith(x, 1:10, fixed = as.character(1:10))
strcut_loc(x, loc)
#>       prepart     mainpart postpart    
#>  [1,] ""          "1"      "2345678910"
#>  [2,] "1"         "2"      "345678910" 
#>  [3,] "12"        "3"      "45678910"  
#>  [4,] "123"       "4"      "5678910"   
#>  [5,] "1234"      "5"      "678910"    
#>  [6,] "12345"     "6"      "78910"     
#>  [7,] "123456"    "7"      "8910"      
#>  [8,] "1234567"   "8"      "910"       
#>  [9,] "12345678"  "9"      "10"        
#> [10,] "123456789" "10"     ""
strcut_loc(x, c(5,5))
#>       prepart mainpart postpart
#>  [1,] "1234"  "5"      "678910"
#>  [2,] "1234"  "5"      "678910"
#>  [3,] "1234"  "5"      "678910"
#>  [4,] "1234"  "5"      "678910"
#>  [5,] "1234"  "5"      "678910"
#>  [6,] "1234"  "5"      "678910"
#>  [7,] "1234"  "5"      "678910"
#>  [8,] "1234"  "5"      "678910"
#>  [9,] "1234"  "5"      "678910"
#> [10,] "1234"  "5"      "678910"


test <- c("The above-mentioned    features are very useful. ",
"Spam, spam, eggs, bacon, and spam. 123 456 789")
strcut_brk(test, "line")
#>      [,1]     [,2]     [,3]            [,4]        [,5]   [,6]     [,7]      
#> [1,] "The "   "above-" "mentioned    " "features " "are " "very "  "useful. "
#> [2,] "Spam, " "spam, " "eggs, "        "bacon, "   "and " "spam. " "123 "    
#>      [,8]   [,9] 
#> [1,] NA     NA   
#> [2,] "456 " "789"
strcut_brk(test, "word")
#>      [,1]   [,2] [,3]    [,4]   [,5]        [,6]   [,7]       [,8] [,9] 
#> [1,] "The"  " "  "above" "-"    "mentioned" "    " "features" " "  "are"
#> [2,] "Spam" ","  " "     "spam" ","         " "    "eggs"     ","  " "  
#>      [,10]   [,11]  [,12] [,13]    [,14] [,15]  [,16] [,17] [,18] [,19] [,20]
#> [1,] " "     "very" " "   "useful" "."   " "    NA    NA    NA    NA    NA   
#> [2,] "bacon" ","    " "   "and"    " "   "spam" "."   " "   "123" " "   "456"
#>      [,21] [,22]
#> [1,] NA    NA   
#> [2,] " "   "789"
strcut_brk(test, "sentence")
#>      [,1]                                                [,2]         
#> [1,] "The above-mentioned    features are very useful. " NA           
#> [2,] "Spam, spam, eggs, bacon, and spam. "               "123 456 789"
strcut_brk(test, "character")
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,] "T"  "h"  "e"  " "  "a"  "b"  "o"  "v"  "e"  "-"   "m"   "e"   "n"   "t"  
#> [2,] "S"  "p"  "a"  "m"  ","  " "  "s"  "p"  "a"  "m"   ","   " "   "e"   "g"  
#>      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#> [1,] "i"   "o"   "n"   "e"   "d"   " "   " "   " "   " "   "f"   "e"   "a"  
#> [2,] "g"   "s"   ","   " "   "b"   "a"   "c"   "o"   "n"   ","   " "   "a"  
#>      [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37] [,38]
#> [1,] "t"   "u"   "r"   "e"   "s"   " "   "a"   "r"   "e"   " "   "v"   "e"  
#> [2,] "n"   "d"   " "   "s"   "p"   "a"   "m"   "."   " "   "1"   "2"   "3"  
#>      [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46] [,47] [,48] [,49]
#> [1,] "r"   "y"   " "   "u"   "s"   "e"   "f"   "u"   "l"   "."   " "  
#> [2,] " "   "4"   "5"   "6"   " "   "7"   "8"   "9"   NA    NA    NA
```

 

## Matrix joining

The `tinycodet` package adds a tiny additional function to `stringi`:

`stri_join_mat` (and their aliases `stri_c_mat` and `stri_paste_mat`).

As the name suggests, these functions perform row-wise (`margin=1`; the
default) or column-wise (`margin=2`) joining of a matrix of strings,
thereby transforming it to a vector of strings. You can do this already
in base R, but it requires converting the matrix to a data.frame or
list, and then calling `stri_join` inside
[`do.call()`](https://rdrr.io/r/base/do.call.html), which to me just
seems too much trouble for something *soooo* abysmally simple.

Here is an example of their usage when re-ordering strings, words, or
sentences :

``` r

# sorting characters in strings:
x <- c(paste(sample(letters), collapse = ""), paste(sample(letters), collapse = ""))
print(x)
#> [1] "mwlefoudviyphzbgtjsqxnkrac" "vkgufijbqazscrwoehmdnxtpyl"
mat <- strcut_brk(x)
rank <- stringi::stri_rank(as.vector(mat)) |>  matrix(ncol=ncol(mat))
sorted <- mat %row~% rank
print(sorted)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,] "a"  "b"  "c"  "d"  "e"  "f"  "g"  "h"  "i"  "j"   "k"   "l"   "m"   "n"  
#> [2,] "a"  "b"  "c"  "d"  "e"  "f"  "g"  "h"  "i"  "j"   "k"   "l"   "m"   "n"  
#>      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#> [1,] "o"   "p"   "q"   "r"   "s"   "t"   "u"   "v"   "w"   "x"   "y"   "z"  
#> [2,] "o"   "p"   "q"   "r"   "s"   "t"   "u"   "v"   "w"   "x"   "y"   "z"
stri_join_mat(sorted, margin=1)
#> [1] "abcdefghijklmnopqrstuvwxyz" "abcdefghijklmnopqrstuvwxyz"
stri_join_mat(sorted, margin=2)
#>  [1] "aa" "bb" "cc" "dd" "ee" "ff" "gg" "hh" "ii" "jj" "kk" "ll" "mm" "nn" "oo"
#> [16] "pp" "qq" "rr" "ss" "tt" "uu" "vv" "ww" "xx" "yy" "zz"

# sorting words:
x <- c("2nd 3rd 1st", "Goodbye everyone")
print(x)
#> [1] "2nd 3rd 1st"      "Goodbye everyone"
mat <- strcut_brk(x, "word")
rank <- stringi::stri_rank(as.vector(mat)) |> matrix(ncol=ncol(mat))
sorted <- mat %row~% rank
sorted[is.na(sorted)] <- ""
stri_c_mat(sorted, margin=1, sep = " ") # <- alias for stri_join_mat
#> [1] "    1st 2nd 3rd"      "  everyone Goodbye  "
stri_c_mat(sorted, margin=2, sep = " ")
#> [1] "   "         "  everyone"  "1st Goodbye" "2nd "        "3rd "

# randomly shuffle sentences:
x <- c("Hello, who are you? Oh, really?! Cool!", "I don't care. But I really don't.")
print(x)
#> [1] "Hello, who are you? Oh, really?! Cool!"
#> [2] "I don't care. But I really don't."
mat <- strcut_brk(x, "sentence")
rank <- sample(1:length(mat)) |> matrix(ncol = ncol(mat))
sorted <- mat %row~% rank
sorted[is.na(sorted)] <- ""
stri_paste_mat(sorted, margin=1) # <- another alias for stri_join_mat
#> [1] "Hello, who are you? Oh, really?! Cool!"
#> [2] "But I really don't.I don't care. "
stri_paste_mat(sorted, margin=2)
#> [1] "Hello, who are you? "             "Oh, really?! But I really don't."
#> [3] "Cool!I don't care. "
```

 
