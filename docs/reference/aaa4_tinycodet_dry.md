# Overview of the 'tinycodet' "Don't Repeat Yourself" Functionality

"Don't Repeat Yourself", sometimes abbreviated as "DRY", is the coding
principle not to write unnecessarily repetitive code. To help in that
effort, the 'tinycodet' R-package introduces a few features:

- The
  [transform_if](https://tony-aw.github.io/tinycodet/reference/transform_if.md)
  function.

- [Operators](https://tony-aw.github.io/tinycodet/reference/matrix_ops.md)
  for short-hand re-ordering matrices Row- or Column-wise.

## See also

[tinycodet_help](https://tony-aw.github.io/tinycodet/reference/aaa0_tinycodet_help.md)

## Examples

``` r
object <- matrix(c(-9:8, NA, NA) , ncol=2)

# in base R:
ifelse( # repetitive, and gives unnecessary warning
  is.na(object > 0), -Inf,
  ifelse(
    object > 0,  log(object), object^2
  )
)
#> Warning: NaNs produced
#>       [,1]      [,2]
#>  [1,]   81 0.0000000
#>  [2,]   64 0.6931472
#>  [3,]   49 1.0986123
#>  [4,]   36 1.3862944
#>  [5,]   25 1.6094379
#>  [6,]   16 1.7917595
#>  [7,]    9 1.9459101
#>  [8,]    4 2.0794415
#>  [9,]    1      -Inf
#> [10,]    0      -Inf

# with tinycodet:
object |> transform_if(\(x) x > 0, log, \(x) x^2, \(x) -Inf) # compact & no warning
#>       [,1]      [,2]
#>  [1,]   81 0.0000000
#>  [2,]   64 0.6931472
#>  [3,]   49 1.0986123
#>  [4,]   36 1.3862944
#>  [5,]   25 1.6094379
#>  [6,]   16 1.7917595
#>  [7,]    9 1.9459101
#>  [8,]    4 2.0794415
#>  [9,]    1      -Inf
#> [10,]    0      -Inf

```
