# Concatenate Character Matrix Row-wise or Column-wise

The `stri_join_mat()` function (and their aliases `stri_c_mat` and
`stri_paste_mat`) perform row-wise (`margin = 1`; the default) or
column-wise (`margin = 2`) joining of a matrix of strings, thereby
transforming a matrix of strings into a vector of strings.

## Usage

``` r
stri_join_mat(mat, margin = 1, sep = "", collapse = NULL)

stri_c_mat(mat, margin = 1, sep = "", collapse = NULL)

stri_paste_mat(mat, margin = 1, sep = "", collapse = NULL)
```

## Arguments

- mat:

  a matrix of strings

- margin:

  the margin over which the strings must be joined.

  - If `margin = 1`, the elements within each row of matrix `mat` are
    joined into a single string. Thus if the matrix has 10 rows, it
    returns a vector of 10 strings.

  - If `margin = 2`, the elements within each column of matrix `mat` are
    joined into a single string. Thus if the matrix has 10 columns, it
    returns a vector of 10 strings.  

- sep, collapse:

  as in [stri_join](https://rdrr.io/pkg/stringi/man/stri_join.html).

## Value

The `stri_join_mat()` function, and its aliases, return a vector of
strings.

## See also

[tinycodet_strings](https://tony-aw.github.io/tinycodet/reference/aaa3_tinycodet_strings.md)

## Examples

``` r
#############################################################################

# Basic example

x <- matrix(letters[1:25], ncol = 5, byrow = TRUE)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "a"  "b"  "c"  "d"  "e" 
#> [2,] "f"  "g"  "h"  "i"  "j" 
#> [3,] "k"  "l"  "m"  "n"  "o" 
#> [4,] "p"  "q"  "r"  "s"  "t" 
#> [5,] "u"  "v"  "w"  "x"  "y" 
stri_join_mat(x, margin = 1)
#> [1] "abcde" "fghij" "klmno" "pqrst" "uvwxy"

x <- matrix(letters[1:25], ncol = 5, byrow = FALSE)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "a"  "f"  "k"  "p"  "u" 
#> [2,] "b"  "g"  "l"  "q"  "v" 
#> [3,] "c"  "h"  "m"  "r"  "w" 
#> [4,] "d"  "i"  "n"  "s"  "x" 
#> [5,] "e"  "j"  "o"  "t"  "y" 
stri_join_mat(x, margin = 2)
#> [1] "abcde" "fghij" "klmno" "pqrst" "uvwxy"


#############################################################################
# sorting characters in strings ====

x <- c(paste(sample(letters), collapse = ""),
       paste(sample(letters), collapse = ""))
print(x)
#> [1] "ujcdmfnberylwxhtiagoqkzsvp" "iyjzsvoqtewdknfhgcrmpxubal"
mat <- strcut_brk(x)
rank <- stringi::stri_rank(as.vector(mat)) |>  matrix(ncol = ncol(mat))
sorted <- mat %row~% rank
sorted[is.na(sorted)] <- ""
print(sorted)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,] "a"  "b"  "c"  "d"  "e"  "f"  "g"  "h"  "i"  "j"   "k"   "l"   "m"   "n"  
#> [2,] "a"  "b"  "c"  "d"  "e"  "f"  "g"  "h"  "i"  "j"   "k"   "l"   "m"   "n"  
#>      [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26]
#> [1,] "o"   "p"   "q"   "r"   "s"   "t"   "u"   "v"   "w"   "x"   "y"   "z"  
#> [2,] "o"   "p"   "q"   "r"   "s"   "t"   "u"   "v"   "w"   "x"   "y"   "z"  
stri_join_mat(sorted, margin = 1)
#> [1] "abcdefghijklmnopqrstuvwxyz" "abcdefghijklmnopqrstuvwxyz"
stri_join_mat(sorted, margin = 2)
#>  [1] "aa" "bb" "cc" "dd" "ee" "ff" "gg" "hh" "ii" "jj" "kk" "ll" "mm" "nn" "oo"
#> [16] "pp" "qq" "rr" "ss" "tt" "uu" "vv" "ww" "xx" "yy" "zz"


#############################################################################

# sorting words ====

x <- c("2nd 3rd 1st", "Goodbye everyone")
print(x)
#> [1] "2nd 3rd 1st"      "Goodbye everyone"
mat <- strcut_brk(x, "word")
rank <- stringi::stri_rank(as.vector(mat)) |> matrix(ncol = ncol(mat))
sorted <- mat %row~% rank
sorted[is.na(sorted)] <- ""
stri_c_mat(sorted, margin = 1, sep = " ") # <- alias for stri_join_mat
#> [1] "    1st 2nd 3rd"      "  everyone Goodbye  "
stri_c_mat(sorted, margin = 2, sep = " ")
#> [1] "   "         "  everyone"  "1st Goodbye" "2nd "        "3rd "       


#############################################################################

# randomly shuffling sentences ====

x <- c("Hello, who are you? Oh, really?! Cool!",
       "I don't care. But I really don't.")
print(x)
#> [1] "Hello, who are you? Oh, really?! Cool!"
#> [2] "I don't care. But I really don't."     
mat <- strcut_brk(x, "sentence")
rank <- sample(seq_along(mat)) |> matrix(ncol = ncol(mat))
sorted <- mat %row~% rank
sorted[is.na(sorted)] <- ""
stri_paste_mat(sorted, margin = 1) # <- another alias for stri_join_mat
#> [1] "Oh, really?! Hello, who are you? Cool!"
#> [2] "I don't care. But I really don't."     
stri_paste_mat(sorted, margin = 2)
#> [1] "Oh, really?! "                      "Hello, who are you? I don't care. "
#> [3] "Cool!But I really don't."          
```
