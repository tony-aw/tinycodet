# String Subsetting Operators

String subsetting operators.  
  
The `x %s><% ss ` operator gets a certain number of the first and last
characters of every string in character vector `x`.  
`%sget%` is an alias for `%s><%`.  
  
The `x %s<>% ss ` operator trims a certain number of the first and last
characters of every string in character vector `x`.  
`%strim%` is an alias for `%<>%`.  
  

## Usage

``` r
x %s><% ss

x %s<>% ss

x %sget% ss

x %strim% ss
```

## Arguments

- x:

  a character vector.

- ss:

  a vector of length 2, or a matrix with 2 columns with
  `nrow(ss) == length(x)`. The object `ss` should consist entirely of
  non-negative and non-missing integers, or be coerce-able to such
  integers. (thus negative integers, and missing values are not allowed;
  decimal numbers will be converted to integers).  
  The first element/column of `ss` gives the number of characters
  counting from the left side to be extracted/removed from `x`.  
  The second element/column of `ss` gives the number of characters
  counting from the right side to be extracted/removed from `x`.  

## Value

Both operators return a character vector of the same length as `x`.  
  
The `x %s><% ss ` operator gives a certain number of the first and last
characters of each string in the input character vector `x`.  
  
The `x %s<>% ss ` operator removes a certain number of the first and
last characters of each string in the input character vector `x`.  
  

## Details

These operators serve as a way to provide straight-forward string
sub-setting.  
  

## See also

[tinycodet_strings](https://tony-aw.github.io/tinycodet/reference/aaa3_tinycodet_strings.md)

## Examples

``` r

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(2, 3)
x %s><% ss
#> [1] "abklm" "noxyz"

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(1, 0)
x %s><% ss
#> [1] "a" "n"

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(2, 3)
x %s<>% ss
#> [1] "cdefghij" "pqrstuvw"

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(1, 0)
x %s<>% ss
#> [1] "bcdefghijklm" "opqrstuvwxyz"
```
