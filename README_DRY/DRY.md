
- [1 The transform_if function, and related
  operators](#1-the-transform_if-function-and-related-operators)
- [2 Generalized in-place (mathematical)
  modifier](#2-generalized-in-place-mathematical-modifier)
- [3 In-place modifying string arithmetic and
  sub-setting](#3-in-place-modifying-string-arithmetic-and-sub-setting)

<!-- README.md is generated from README.Rmd. Please edit that file -->

 

# 1 The transform_if function, and related operators

“Don’t Repeat Yourself”, sometimes abbreviated as “DRY”, is the coding
principle that you should try to reduce repeating patterns in your code
(within reason).

Consider the following code:

`x <- ifelse(cond(x), f(x), g(x))`

Here a conditional subset of the object `x` is transformed where the
condition is using a function referring to `x` itself. Consequently,
reference to `x` is written **four times**! If the object has a short
name like “x, this doesn’t matter too much. But if the object has a
longer name like `very_long_name_1`, doing something like this:

``` r
very_long_name_1 <- ifelse(
  very_long_name_1>0,  log(very_long_name_1), very_long_name_1^2
)
```

becomes cumbersome quickly.

The `tinyoperations` package therefore adds the
`transform_if(x, cond, trans_T, trans_F)` function which will “dry” this
up. The above code can now be re-written as:

``` r
very_long_name_1 %<>% transform_if(\(x)x>0, log, \(x)x^2)
```

Note that `x` must be a vector, matrix, or array. Unlike `ifelse`, the
transformations in `transform_if()` are evaluated as
`trans_T(x[cond(x)])` and `trans_F(x[!cond(x)])`, ensuring no
unnecessary warnings or errors occur.

Besides `transform_if`, the `tinyoperations` package also adds 2
“subset_if” operators:

- The `x %[if]% cond` operator selects elements from vector/matrix/array
  `x`, for which the result of `cond(x)` returns `TRUE`.

- The `x %[!if]% cond` operator selects elements from
  vector/matrix/array `x`, for which the result of `cond(x)` returns
  `FALSE`.

For example:

``` r
object_with_very_long_name <- matrix(-10:9, ncol=2)
print(object_with_very_long_name)
#>       [,1] [,2]
#>  [1,]  -10    0
#>  [2,]   -9    1
#>  [3,]   -8    2
#>  [4,]   -7    3
#>  [5,]   -6    4
#>  [6,]   -5    5
#>  [7,]   -4    6
#>  [8,]   -3    7
#>  [9,]   -2    8
#> [10,]   -1    9
object_with_very_long_name %[if]% \(x)x %in% 1:10
#> [1] 1 2 3 4 5 6 7 8 9
object_with_very_long_name %[!if]% \(x)x %in% 1:10
#>  [1] -10  -9  -8  -7  -6  -5  -4  -3  -2  -1   0
```

Another operator added by `tinyoperations` is `x %unreal =% y`, which
replaces all NA, NaN, Inf and -Inf in `x` with the value given in `y`.

So `x %unreal =% y` is the same as
`x[is.na(x)|is.nan(x)|is.infinite(x)] <- y`.

 

# 2 Generalized in-place (mathematical) modifier

This R package includes infix operators for in-place modifying
mathematical arithmetic.

Consider the following line of code:

``` r
mtcars$mpg[mtcars$cyl>6] <- mtcars$mpg[mtcars$cyl>6]^2
```

The same expression, `mtcars$mpg[mtcars$cyl>6]`, is written twice,
making this code rather long and cumbersome, even though we’re just
squaring the expression. The well-known `magrittr` R-package has an
in-place modifier pipe, `%<>%`. This works excellent for actual
functions, like so:

``` r
mtcars$mpg[mtcars$cyl>6] %<>% mean() # same as mtcars$mpg[mtcars$cyl>6] <- mean(mtcars$mpg[mtcars$cyl>6])
```

But for arithmetic, one needs to translate the operators into a
function, and then perform the in-place modifier pipe:

``` r
mtcars$mpg[mtcars$cyl>6] %<>% raise_to_power(2)
```

This is better, but can we do even better?

This R package solves the above laid-out problem by implementing a
generalized in-place (mathematical) modifier, through the `x %:=% f`
operator.

Lets look at the original problem:

``` r
mtcars$mpg[mtcars$cyl>6] <- mtcars$mpg[mtcars$cyl>6]^2
```

With `tinyoperations` one can now make this more compact (more “tiny”,
if you will) as follows:

``` r
mtcars$mpg[mtcars$cyl>6] %:=% \(x)x^2
```

This function based method is used instead of the more traditional
in-place mathematical modification like `+=` to prevent precedence
issues (functions come before mathematical arithmetic in `R`).
Precedence issues are a common occurrence in R packages that attempt to
implement in-place modifying arithmetic; `tinyoperations`’s `%:=%`
operator does not have this problem, as the math is specified inside a
function.

 

# 3 In-place modifying string arithmetic and sub-setting

With the exception of `%ss%`, all infix operators for string arithmetic
and string sub-setting have their in-place modifying equivalent:

- `x %s+ =% y` is the same as `x <- x %s+% y`
- `x %s- =% p` is the same as `x <- x %s-% p`
- `x %s* =% n` is the same as `x <- x %s*% n`
- `x %s/ =% p` is the same as `x <- x %s/% p`
- `x %sget =% ss` is the same as `x <- x %sget% ss`
- `x %strim =% ss` is the same as `x <- x %strim% ss`

Notice the extra space before the `=` sign.

 
