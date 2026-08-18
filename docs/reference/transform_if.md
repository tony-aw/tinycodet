# transform_if: Conditional Sub-set Transformation of Atomic objects

The `transform_if()` function transforms an object `x`, based on the
logical result (`TRUE, FALSE, NA`) of condition function `cond(x)` or
logical vector `cond`, such that:  

- For every value where `cond(x)==TRUE` / `cond==TRUE`, function
  `yes(x)` is run or scalar `yes` is returned.

- For every value where `cond(x)==FALSE` / `cond==FALSE`, function
  `no(x)` is run or scalar `no` is returned.

- For every value where `cond(x)==NA` / `cond==NA`, function `other(x)`
  is run or scalar `other` is returned.  

For a more `ifelse`-like function where `yes`, `no`, and `other` are
vectors, see `kit::`[iif](https://rdrr.io/pkg/kit/man/iif.html).

## Usage

``` r
transform_if(x, cond, yes = function(x) x, no = function(x) x, other = NA)
```

## Arguments

- x:

  a vector, matrix, or array.

- cond:

  either an object of class `logical` with the same length as `x`,  
  or a (possibly anonymous) function that returns an object of class
  `logical` with the same length as `x`.  
  For example: `\(x)x>0`.  

- yes:

  the (possibly anonymous) transformation function to use when function
  `cond(x)==TRUE` / logical `cond==TRUE`.  
  Alternatively, one can also supply an atomic scalar.  
  If argument `yes` is not specified, it defaults to `\(x)x`.

- no:

  the (possibly anonymous) transformation function to use when function
  `cond(x)==FALSE` / logical `cond==FALSE`.  
  Alternatively, one can also supply an atomic scalar.  
  If argument `no` is not specified, it defaults to `\(x)x`.

- other:

  the (possibly anonymous) transformation function to use when function
  `cond(x)` / logical `cond` returns `NA`.  
  Alternatively, one can also supply an atomic scalar.  
  If argument `other` is not specified, it defaults to `NA`.  
  Note that function `other(x)` is run or scalar `other` is returned
  when function `cond(x)` or logical `cond` is `NA`, not necessarily
  when `x` itself is `NA`.

## Value

The transformed vector, matrix, or array (attributes are conserved).

## Details

Be careful with coercion! For example the following code:

    x <- c("a", "b")
    transform_if(x, \(x) x == "a", as.numeric, as.logical)

returns:

    [1] NA NA

due to the same character vector being given 2 incompatible classes.  
  

## See also

[tinycodet_dry](https://tony-aw.github.io/tinycodet/reference/aaa4_tinycodet_dry.md)

## Examples

``` r
x <- c(-10:9, NA, NA)
object <- matrix(x, ncol = 2)
attr(object, "helloworld") <- "helloworld"
print(object)
#>       [,1] [,2]
#>  [1,]  -10    1
#>  [2,]   -9    2
#>  [3,]   -8    3
#>  [4,]   -7    4
#>  [5,]   -6    5
#>  [6,]   -5    6
#>  [7,]   -4    7
#>  [8,]   -3    8
#>  [9,]   -2    9
#> [10,]   -1   NA
#> [11,]    0   NA
#> attr(,"helloworld")
#> [1] "helloworld"
y <- 0
z <- 1000

object |> transform_if(\(x) x > y, log, \(x) x^2, \(x) -z)
#>       [,1]          [,2]
#>  [1,]  100     0.0000000
#>  [2,]   81     0.6931472
#>  [3,]   64     1.0986123
#>  [4,]   49     1.3862944
#>  [5,]   36     1.6094379
#>  [6,]   25     1.7917595
#>  [7,]   16     1.9459101
#>  [8,]    9     2.0794415
#>  [9,]    4     2.1972246
#> [10,]    1 -1000.0000000
#> [11,]    0 -1000.0000000
#> attr(,"helloworld")
#> [1] "helloworld"
object |> transform_if(object > y, log, \(x) x^2, -z) # same as previous line
#>       [,1]          [,2]
#>  [1,]  100     0.0000000
#>  [2,]   81     0.6931472
#>  [3,]   64     1.0986123
#>  [4,]   49     1.3862944
#>  [5,]   36     1.6094379
#>  [6,]   25     1.7917595
#>  [7,]   16     1.9459101
#>  [8,]    9     2.0794415
#>  [9,]    4     2.1972246
#> [10,]    1 -1000.0000000
#> [11,]    0 -1000.0000000
#> attr(,"helloworld")
#> [1] "helloworld"
```
