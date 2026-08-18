# Overview of the 'tinycodet' Extension of 'stringi'

R's numerical functions are generally very fast. But R's native string
functions are somewhat slow, do not have a unified naming scheme, and
are not as comprehensive as R's impressive numerical functions.  
The primary R-package that fixes this is 'stringi', which many, if not
most, string related packages depend on (see the list of
reverse-dependencies on CRAN).  
As string manipulation is important to programming languages, even those
primarily focused on mathematics, 'tinycodet' adds a little bit new
functionality to 'stringi'.  
  
'tinycodet' adds the following functions to extend 'stringi':

- Find \\i^{th}\\ pattern occurrence
  ([stri_locate_ith](https://tony-aw.github.io/tinycodet/reference/stri_locate_ith.md)),
  or \\i^{th}\\ text boundary
  ([stri_locate_ith_boundaries](https://tony-aw.github.io/tinycodet/reference/stri_locate_ith.md)).

- [Concatenate a character matrix row- or
  column-wise](https://tony-aw.github.io/tinycodet/reference/stri_join_mat.md).  
    

'tinycodet' adds the following operators, to complement the already
existing 'stringi' operators:

- Infix operators for [string
  arithmetic](https://tony-aw.github.io/tinycodet/reference/str_arithmetic.md).

- Infix operators for [string
  sub-setting](https://tony-aw.github.io/tinycodet/reference/str_subset_ops.md),
  which get or remove the first and/or last `n` characters from strings.

- Infix operators for [detecting
  patterns](https://tony-aw.github.io/tinycodet/reference/str_search.md),
  and
  [strfind()\<-](https://tony-aw.github.io/tinycodet/reference/str_search.md)
  for locating/extracting/replacing found patterns.  
    

And finally, 'tinycodet' adds the somewhat separate
[strcut\_-functions](https://tony-aw.github.io/tinycodet/reference/strcut.md),
to cut strings into pieces without removing the delimiters.  
  

## Regarding Vector Recycling in the 'stringi'-based Functions

Generally speaking, vector recycling is supported as 'stringi' itself
supports it also.  
There are, however, a few exceptions.  
First, matrix inputs (like in
[strcut_loc](https://tony-aw.github.io/tinycodet/reference/strcut.md)
and [string sub-setting
operators](https://tony-aw.github.io/tinycodet/reference/str_subset_ops.md))
will generally not be recycled.  
Second, the `i` argument in
[stri_locate_ith](https://tony-aw.github.io/tinycodet/reference/stri_locate_ith.md)
does not support vector recycling.  
Scalar recycling is virtually always supported.  
  

## References

Gagolewski M., **stringi**: Fast and portable character string
processing in R, *Journal of Statistical Software* 103(2), 2022, 1–59,
[doi:10.18637/jss.v103.i02](https://doi.org/10.18637/jss.v103.i02)

## See also

[tinycodet_help](https://tony-aw.github.io/tinycodet/reference/aaa0_tinycodet_help.md),
[s_pattern](https://tony-aw.github.io/tinycodet/reference/s_pattern.md)

## Examples

``` r
# character vector:
x <- c("3rd 1st 2nd", "5th 4th 6th")
print(x)
#> [1] "3rd 1st 2nd" "5th 4th 6th"

# detect if there are digits:
x %s{}% "\\d"
#> [1] TRUE TRUE

# find second last digit:
loc <- stri_locate_ith(x, i = -2, regex = "\\d")
stringi::stri_sub(x, from = loc)
#> [1] "1" "4"

# cut x into matrix of individual words:
mat <- strcut_brk(x, "word")

# sort rows of matrix using the fast %row~% operator:
rank <- stringi::stri_rank(as.vector(mat)) |> matrix(ncol = ncol(mat))
sorted <- mat %row~% rank
sorted[is.na(sorted)] <- ""

# join elements of every row into a single character vector:
stri_c_mat(sorted, margin = 1, sep = " ")
#> [1] "    1st 2nd 3rd" "    4th 5th 6th"
```
