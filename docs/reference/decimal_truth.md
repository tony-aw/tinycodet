# Safer Decimal Number (In)Equality Testing Operators

The `%d==%, %d!=% %d<%, %d>%, %d<=%, %d>=%` (in)equality operators
perform decimal (type "double") number truth testing.  
They are virtually equivalent to the regular (in)equality operators,  
`==, !=, <, >, <=, >=`,  
except for 2 aspects:

1.  The decimal number (in)equality operators assume that if the
    absolute difference between any 2 numbers `x` and `y` is smaller
    than the Machine tolerance, `sqrt(.Machine$double.eps)`, then `x`
    and `y` should be consider to be equal.  
    For example: `(0.1 * 7) == 0.7` returns `FALSE`, even though they
    are equal, due to the way decimal numbers are stored in programming
    languages like 'R' and 'Python'.  
    But `(0.1 * 7) %d==% 0.7` returns `TRUE`.  

2.  Only numeric input is allowed, so characters are not coerced to
    numbers.  
    I.e. `1 < "a"` gives `TRUE`, whereas `1 %d<% "a"` gives an error.  
    For character equality testing, see
    [%s==%](https://rdrr.io/pkg/stringi/man/operator_compare.html) from
    the 'stringi' package.  

Thus these operators provide safer decimal number (in)equality tests.  
  
There are also the `x %d{}% bnd` and `x %d!{}% bnd` operators, where
`bnd` is a vector of length 2, or a 2-column matrix
(`nrow(bnd)==length(x)` or `nrow(bnd)==1`).  
The `x %d{}% bnd` operator checks if `x` is within the closed interval
with bounds defined by `bnd`.  
The `x %d!{}% bnd` operator checks if `x` is outside the closed interval
with bounds defined by `bnd`.  
  
Moreover, the function `is_wholenumber()` is added, to safely test for
whole numbers.

## Usage

``` r
x %d==% y

x %d!=% y

x %d<% y

x %d>% y

x %d<=% y

x %d>=% y

x %d{}% bnd

x %d!{}% bnd

is_wholenumber(x, tol = sqrt(.Machine$double.eps))
```

## Arguments

- x, y:

  numeric vectors, matrices, or arrays.

- bnd:

  either a vector of length 2, or a matrix with 2 columns and 1 row, or
  else a matrix with 2 columns where `nrow(bnd)==length(x)` (or can be
  recycled to be `nrow(bnd)==length(x)`).  
  The first element/column of `bnd` gives the lower bound of the closed
  interval;  
  The second element/column of `bnd` gives the upper bound of the closed
  interval.  

- tol:

  a single, strictly positive number close to zero, giving the
  tolerance.

## Value

For the `%d...%` operators:  
A logical vector with the same dimensions as `x`, indicating the result
of the element by element comparison.  
NOTE: `Inf` by `Inf` and `-Inf` by `-Inf` comparisons with the `%d...%`
operators return `NA`.  
  
For `is_wholenumber()`:  
A logical vector with the same dimensions as `x`, indicating the result
of the element by element comparison.  
NOTE: `Inf`, `-Inf`, `NaN` and `NA` all return `NA` for
`is_wholenumber()`.

## Details

The operators described in this page are defined in terms of the
existing base [Logic](https://rdrr.io/r/base/Logic.html) operators, and
should therefore be compatible with (S3) classes that have method
dispatches defined for relational operators.  
  

## See also

[tinycodet_safer](https://tony-aw.github.io/tinycodet/reference/aaa1_tinycodet_safer.md)

## Examples

``` r
x <- c(0.3, 0.6, 0.7)
y <- c(0.1 * 3, 0.1 * 6, 0.1 * 7)
print(x)
#> [1] 0.3 0.6 0.7
print(y)
#> [1] 0.3 0.6 0.7

x == y # gives FALSE, but should be TRUE
#> [1] FALSE FALSE FALSE
x != y # gives TRUE, should be FALSE
#> [1] TRUE TRUE TRUE
x > y # not wrong
#> [1] FALSE FALSE FALSE
x < y # gives TRUE, should be FALSE
#> [1] TRUE TRUE TRUE

# same as above, but here the results are correct:
x %d==% y # correct
#> [1] TRUE TRUE TRUE
x %d!=% y # correct
#> [1] FALSE FALSE FALSE
x %d<% y # correct
#> [1] FALSE FALSE FALSE
x %d>% y # correct
#> [1] FALSE FALSE FALSE
x %d<=% y # correct
#> [1] TRUE TRUE TRUE
x %d>=% y # correct
#> [1] TRUE TRUE TRUE

# check if numbers are in closed interval:
x <- c(0.3, 0.6, 0.7)
bnd <- cbind(x - 0.1, x + 0.1)
x %d{}% bnd
#> [1] TRUE TRUE TRUE
x %d!{}% bnd
#> [1] FALSE FALSE FALSE

# These operators work for integers also:
x <- 1L:5L
y <- 1L:5L
x %d==% y
#> [1] TRUE TRUE TRUE TRUE TRUE
x %d!=% y
#> [1] FALSE FALSE FALSE FALSE FALSE
x %d<% y
#> [1] FALSE FALSE FALSE FALSE FALSE
x %d>% y
#> [1] FALSE FALSE FALSE FALSE FALSE
x %d<=% y
#> [1] TRUE TRUE TRUE TRUE TRUE
x %d>=% y
#> [1] TRUE TRUE TRUE TRUE TRUE

x <- 1L:5L
y <- x + 1L
x %d==% y
#> [1] FALSE FALSE FALSE FALSE FALSE
x %d!=% y
#> [1] TRUE TRUE TRUE TRUE TRUE
x %d<% y
#> [1] TRUE TRUE TRUE TRUE TRUE
x %d>% y
#> [1] FALSE FALSE FALSE FALSE FALSE
x %d<=% y
#> [1] TRUE TRUE TRUE TRUE TRUE
x %d>=% y
#> [1] FALSE FALSE FALSE FALSE FALSE

x <- 1L:5L
y <- x - 1L
x %d==% y
#> [1] FALSE FALSE FALSE FALSE FALSE
x %d!=% y
#> [1] TRUE TRUE TRUE TRUE TRUE
x %d<% y
#> [1] FALSE FALSE FALSE FALSE FALSE
x %d>% y
#> [1] TRUE TRUE TRUE TRUE TRUE
x %d<=% y
#> [1] FALSE FALSE FALSE FALSE FALSE
x %d>=% y
#> [1] TRUE TRUE TRUE TRUE TRUE

# is_wholenumber:
is_wholenumber(1:10 + c(0, 0.1))
#>  [1]  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE
```
