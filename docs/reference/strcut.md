# Cut Strings

The `strcut_loc()` function cuts every string in a character vector
around a location range `loc`, such that every string is cut into the
following parts:

- the sub-string **before** `loc`;

- the sub-string at `loc` itself;

- the sub-string **after** `loc`.

The location range `loc` would usually be matrix with 2 columns, giving
the start and end points of some pattern match.  
  
The `strcut_brk()` function (a wrapper around
[stri_split_boundaries](https://rdrr.io/pkg/stringi/man/stri_split_boundaries.html)`(..., tokens_only = FALSE)`)
cuts every string into individual text breaks (like character, word,
line, or sentence boundaries).  
  

## Usage

``` r
strcut_loc(str, loc)

strcut_brk(str, type = "character", tolist = FALSE, n = -1L, ...)
```

## Arguments

- str:

  a string or character vector.

- loc:

  Either one of the following:

  - the result from the
    [stri_locate_ith](https://tony-aw.github.io/tinycodet/reference/stri_locate_ith.md)
    function.

  - a matrix of 2 integer columns, with `nrow(loc)==length(str)`, giving
    the location range of the middle part.

  - a vector of length 2, giving the location range of the middle part.

- type:

  either one of the following:

  - a single string giving the break iterator type (i.e. `"character"`,
    `"line_break"`, `"sentence"`, `"word"`, or a custom set of ICU break
    iteration rules).

  - a list with break iteration options, like a list produced by
    [stri_opts_brkiter](https://rdrr.io/pkg/stringi/man/stri_opts_brkiter.html).

  [![\[BOUNDARIES\]](figures/aboutsearch-boundaries-blue.svg)](https://stringi.gagolewski.com/rapi/about_search_boundaries.html)  

- tolist:

  logical, indicating if `strcut_brk` should return a list (`TRUE`), or
  a matrix (`FALSE`, default).

- n:

  see
  [stri_split_boundaries](https://rdrr.io/pkg/stringi/man/stri_split_boundaries.html).

- ...:

  additional arguments to be passed to
  [stri_split_boundaries](https://rdrr.io/pkg/stringi/man/stri_split_boundaries.html).

## Value

For `strcut_loc()`:  
A character matrix with `length(str)` rows and 3 columns, where for
every row `i` it holds the following:

- the first column contains the sub-string **before** `loc[i,]`, or `NA`
  if `loc[i,]` contains `NA`;

- the second column contains the sub_string at `loc[i,]`, or the uncut
  string if `loc[i,]` contains `NA`;

- the third and last column contains the sub-string **after** `loc[i,]`,
  or `NA` if `loc[i,]` contains `NA`.  
    

For `strcut_brk(..., tolist = FALSE)`:  
A character matrix with `length(str)` rows and a number of columns equal
to the maximum number of pieces `str` was cut in.  
Empty places are filled with `NA`.  
  
For `strcut_brk(..., tolist = TRUE)`:  
A list with `length(str)` elements, where each element is a character
vector containing the cut string.  
  

## Details

The `strcut_` functions provide a short and concise way to cut strings
into pieces, without removing the delimiters, which is an operation that
lies at the core of virtually all boundaries-operations in 'stringi'.  
  
The main difference between the `strcut_` - functions and
[stri_split](https://rdrr.io/pkg/stringi/man/stri_split.html) /
[strsplit](https://rdrr.io/r/base/strsplit.html), is that the latter
generally removes the delimiter patterns in a string when cutting, while
the `strcut_`-functions do not attempt to remove parts of the string by
default, they only attempt to cut the strings into separate pieces.
Moreover, the `strcut_` - functions return a matrix by default.  
  

## See also

[tinycodet_strings](https://tony-aw.github.io/tinycodet/reference/aaa3_tinycodet_strings.md)

## Examples

``` r

x <- rep(paste0(1:10, collapse = ""), 10)
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
strcut_loc(x, c(5, 5))
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
strcut_loc(x, c(NA, NA))
#>       prepart mainpart      postpart
#>  [1,] NA      "12345678910" NA      
#>  [2,] NA      "12345678910" NA      
#>  [3,] NA      "12345678910" NA      
#>  [4,] NA      "12345678910" NA      
#>  [5,] NA      "12345678910" NA      
#>  [6,] NA      "12345678910" NA      
#>  [7,] NA      "12345678910" NA      
#>  [8,] NA      "12345678910" NA      
#>  [9,] NA      "12345678910" NA      
#> [10,] NA      "12345678910" NA      
strcut_loc(x, c(5, NA))
#>       prepart mainpart      postpart
#>  [1,] NA      "12345678910" NA      
#>  [2,] NA      "12345678910" NA      
#>  [3,] NA      "12345678910" NA      
#>  [4,] NA      "12345678910" NA      
#>  [5,] NA      "12345678910" NA      
#>  [6,] NA      "12345678910" NA      
#>  [7,] NA      "12345678910" NA      
#>  [8,] NA      "12345678910" NA      
#>  [9,] NA      "12345678910" NA      
#> [10,] NA      "12345678910" NA      
strcut_loc(x, c(NA, 5))
#>       prepart mainpart      postpart
#>  [1,] NA      "12345678910" NA      
#>  [2,] NA      "12345678910" NA      
#>  [3,] NA      "12345678910" NA      
#>  [4,] NA      "12345678910" NA      
#>  [5,] NA      "12345678910" NA      
#>  [6,] NA      "12345678910" NA      
#>  [7,] NA      "12345678910" NA      
#>  [8,] NA      "12345678910" NA      
#>  [9,] NA      "12345678910" NA      
#> [10,] NA      "12345678910" NA      

test <- "The\u00a0above-mentioned    features are very useful. " %s+%
  "Spam, spam, eggs, bacon, and spam. 123 456 789"
strcut_brk(test, "line")
#>      [,1]         [,2]            [,3]        [,4]   [,5]    [,6]      
#> [1,] "The above-" "mentioned    " "features " "are " "very " "useful. "
#>      [,7]     [,8]     [,9]     [,10]     [,11]  [,12]    [,13]  [,14]  [,15]
#> [1,] "Spam, " "spam, " "eggs, " "bacon, " "and " "spam. " "123 " "456 " "789"
strcut_brk(test, "word")
#>      [,1]  [,2] [,3]    [,4] [,5]        [,6]   [,7]       [,8] [,9]  [,10]
#> [1,] "The" " "  "above" "-"  "mentioned" "    " "features" " "  "are" " "  
#>      [,11]  [,12] [,13]    [,14] [,15] [,16]  [,17] [,18] [,19]  [,20] [,21]
#> [1,] "very" " "   "useful" "."   " "   "Spam" ","   " "   "spam" ","   " "  
#>      [,22]  [,23] [,24] [,25]   [,26] [,27] [,28] [,29] [,30]  [,31] [,32]
#> [1,] "eggs" ","   " "   "bacon" ","   " "   "and" " "   "spam" "."   " "  
#>      [,33] [,34] [,35] [,36] [,37]
#> [1,] "123" " "   "456" " "   "789"
strcut_brk(test, "sentence")
#>      [,1]                                               
#> [1,] "The above-mentioned    features are very useful. "
#>      [,2]                                  [,3]         
#> [1,] "Spam, spam, eggs, bacon, and spam. " "123 456 789"
strcut_brk(test)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,] "T"  "h"  "e"  " "  "a"  "b"  "o"  "v"  "e"  "-"   "m"   "e"   "n"   "t"  
#>      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#> [1,] "i"   "o"   "n"   "e"   "d"   " "   " "   " "   " "   "f"   "e"   "a"  
#>      [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37] [,38]
#> [1,] "t"   "u"   "r"   "e"   "s"   " "   "a"   "r"   "e"   " "   "v"   "e"  
#>      [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46] [,47] [,48] [,49] [,50]
#> [1,] "r"   "y"   " "   "u"   "s"   "e"   "f"   "u"   "l"   "."   " "   "S"  
#>      [,51] [,52] [,53] [,54] [,55] [,56] [,57] [,58] [,59] [,60] [,61] [,62]
#> [1,] "p"   "a"   "m"   ","   " "   "s"   "p"   "a"   "m"   ","   " "   "e"  
#>      [,63] [,64] [,65] [,66] [,67] [,68] [,69] [,70] [,71] [,72] [,73] [,74]
#> [1,] "g"   "g"   "s"   ","   " "   "b"   "a"   "c"   "o"   "n"   ","   " "  
#>      [,75] [,76] [,77] [,78] [,79] [,80] [,81] [,82] [,83] [,84] [,85] [,86]
#> [1,] "a"   "n"   "d"   " "   "s"   "p"   "a"   "m"   "."   " "   "1"   "2"  
#>      [,87] [,88] [,89] [,90] [,91] [,92] [,93] [,94] [,95]
#> [1,] "3"   " "   "4"   "5"   "6"   " "   "7"   "8"   "9"  
strcut_brk(test, n = 1)
#>      [,1]                                                                                             
#> [1,] "The above-mentioned    features are very useful. Spam, spam, eggs, bacon, and spam. 123 456 789"
strcut_brk(test, "line", tolist = TRUE)
#> [[1]]
#>  [1] "The above-"    "mentioned    " "features "     "are "         
#>  [5] "very "         "useful. "      "Spam, "        "spam, "       
#>  [9] "eggs, "        "bacon, "       "and "          "spam. "       
#> [13] "123 "          "456 "          "789"          
#> 
strcut_brk(test, "word", tolist = TRUE)
#> [[1]]
#>  [1] "The"       " "         "above"     "-"         "mentioned" "    "     
#>  [7] "features"  " "         "are"       " "         "very"      " "        
#> [13] "useful"    "."         " "         "Spam"      ","         " "        
#> [19] "spam"      ","         " "         "eggs"      ","         " "        
#> [25] "bacon"     ","         " "         "and"       " "         "spam"     
#> [31] "."         " "         "123"       " "         "456"       " "        
#> [37] "789"      
#> 
strcut_brk(test, "sentence", tolist = TRUE)
#> [[1]]
#> [1] "The above-mentioned    features are very useful. "
#> [2] "Spam, spam, eggs, bacon, and spam. "              
#> [3] "123 456 789"                                      
#> 

brk <- stringi::stri_opts_brkiter(
  type = "line"
)
strcut_brk(test, brk)
#>      [,1]         [,2]            [,3]        [,4]   [,5]    [,6]      
#> [1,] "The above-" "mentioned    " "features " "are " "very " "useful. "
#>      [,7]     [,8]     [,9]     [,10]     [,11]  [,12]    [,13]  [,14]  [,15]
#> [1,] "Spam, " "spam, " "eggs, " "bacon, " "and " "spam. " "123 " "456 " "789"
```
