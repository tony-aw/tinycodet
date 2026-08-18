# Overview of the 'tinycodet' "Safer" Functionality

To help make your code safer, the 'tinycodet' R-package introduces a few
functions:

- [Safer decimal (in)equality
  testing](https://tony-aw.github.io/tinycodet/reference/decimal_truth.md).

- Standard evaluated versions of some common expression-evaluation
  functions:  
  [with_pro](https://tony-aw.github.io/tinycodet/reference/pro.md) and
  [aes_pro](https://tony-aw.github.io/tinycodet/reference/pro.md).

- The [lock_TF](https://tony-aw.github.io/tinycodet/reference/lock.md)
  function to set and lock `T` and `F` to `TRUE` and `FALSE`,
  respectively.

- The %\<-c% operator to assign locked constants.

- [safer_partialmatch](https://tony-aw.github.io/tinycodet/reference/safer_partialmatch.md)
  to set options for safer dollar, arguments, and attribute matching.  
    

## See also

[tinycodet_help](https://tony-aw.github.io/tinycodet/reference/aaa0_tinycodet_help.md)

## Examples

``` r
x <- c(0.3, 0.6, 0.7)
y <- c(0.1*3, 0.1*6, 0.1*7)
x == y # gives FALSE, but should be TRUE
#> [1] FALSE FALSE FALSE
x %d==% y # here it's done correctly
#> [1] TRUE TRUE TRUE


```
