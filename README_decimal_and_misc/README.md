
- [1 Safer decimal number truth testing
  operators](#1-safer-decimal-number-truth-testing-operators)
- [2 Additional logic operators](#2-additional-logic-operators)
- [3 Other](#3-other)

<!-- README.md is generated from README.Rmd. Please edit that file -->

 

# 1 Safer decimal number truth testing operators

This package adds the `%d==%, %d!=% %d<%, %d>%, %d<=%, %d>=%`
(in)equality operators, which perform safer decimal number truth
testing. They are virtually equivalent to the regular (in)equality
operators, `==, !=, <, >, <=, >=`, except for one aspect: The decimal
number truth testing operators assume that if the absolute difference
between `x` and `y` is smaller than the Machine tolerance,
`sqrt(.Machine$double.eps)`, then `x` and `y` ought to be consider to be
equal.

Thus these provide safer decimal number (in)equality operators.

For example: `(0.1*7) == 0.7` returns `FALSE`, even though they are
equal, due to the way decimal numbers are stored in programming
languages like `R`, `Python`, etc. But `(0.1*7) %d==% 0.7` returns
`TRUE`.

Some examples:

``` r
x <- c(0.3, 0.6, 0.7)
y <- c(0.1*3, 0.1*6, 0.1*7)
print(x); print(y)
#> [1] 0.3 0.6 0.7
#> [1] 0.3 0.6 0.7
x == y # gives FALSE, but should be TRUE
#> [1] FALSE FALSE FALSE
x!= y # gives TRUE, should be FALSE
#> [1] TRUE TRUE TRUE
x > y # not wrong
#> [1] FALSE FALSE FALSE
x < y # gives TRUE, should be FALSE
#> [1] TRUE TRUE TRUE
x %d==% y # here it's done correctly
#> [1] TRUE TRUE TRUE
x %d!=% y
#> [1] FALSE FALSE FALSE
x %d<% y # correct
#> [1] FALSE FALSE FALSE
x %d>% y # correct
#> [1] FALSE FALSE FALSE
x %d<=% y # correct
#> [1] TRUE TRUE TRUE
x %d>=% y # correct
#> [1] TRUE TRUE TRUE

x <- c(0.3, 0.6, 0.7)
bnd <- matrix(c(x-0.1, x+0.1), ncol=2)
x %d{}% bnd
#> [1] TRUE TRUE TRUE
x %d!{}% bnd
#> [1] FALSE FALSE FALSE
```

Although designed for objects (vectors, matrices, arrays) of class
`double` (decimal numbers), these operators also work correctly for
integers. These operators do not work for non-numeric objects.

 

# 2 Additional logic operators

The tinyoperations package adds a few basic logic operators:

- `%xor%`: Exclusive OR
- `%n&%`: NOT AND (i.e. `(!x) & (!y)`). Note that if either `x` or `y`
  is `NA`, `%n&%` will also give `NA` (unlike `(!x) & (!y)`, which would
  give `FALSE`.)
- `%?=%`: checks if both `x` and `y` are unknown or unreal (NA, NaN,
  Inf, -Inf)
- `%out%`: the opposite of `%in%` (i.e. `!x %in% y`)
- `s %sgrep% p` checks if pattern `p` (defined as either `regex`, or as
  a call from `s_pattern()`) appears in character vector `s` (info on
  the `s_pattern()` function can be found in the string section of this
  read-me)

Here are some examples:

``` r
x <- c(TRUE, FALSE, TRUE, FALSE, NA, NaN, Inf, -Inf, TRUE, FALSE)
y <- c(FALSE, TRUE, TRUE, FALSE, rep(NA, 6))
outcome <- data.frame(
  x=x, y=y,
  "x %xor% y"=x %xor% y, "x %n&% y" = x %n&% y, "x %?=% y" = x %?=% y,
  check.names = FALSE
)
kable(outcome)
```

|    x | y     | x %xor% y | x %n&% y | x %?=% y |
|-----:|:------|:----------|:---------|:---------|
|    1 | FALSE | TRUE      | FALSE    | FALSE    |
|    0 | TRUE  | TRUE      | FALSE    | FALSE    |
|    1 | TRUE  | FALSE     | FALSE    | FALSE    |
|    0 | FALSE | FALSE     | TRUE     | FALSE    |
|   NA | NA    | NA        | NA       | TRUE     |
|  NaN | NA    | NA        | NA       | TRUE     |
|  Inf | NA    | NA        | NA       | TRUE     |
| -Inf | NA    | NA        | NA       | TRUE     |
|    1 | NA    | NA        | NA       | FALSE    |
|    0 | NA    | NA        | NA       | FALSE    |

``` r

1:3 %out% 1:10
#> [1] FALSE FALSE FALSE
1:10 %out% 1:3
#>  [1] FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
```

Numbers can have many different sub-types whilst still being `numeric`.
The `n %=numtype% numtype` operator will check for every value of
numeric vector `n` if it can be considered a number belonging to type
`numtype`. The following values for `numtype` are allowed:

- “~0”: zero, or else a number whose absolute value is smaller than the
  Machine tolerance (`sqrt(.Machine$double.eps)`);
- “B”: binary numbers (0 or 1);
- “prop”: proportions;
- “I”: Integers;
- “odd”: odd integers;
- “even”: even integers
- “R”: Real numbers;
- “unreal”: infinity, NA, or NaN;

The string counterpart for `%=numtype%` is `s %=strtype% strtype`, which
checks for every value of character vector `s` if it can seen as a
certain `strtype`. The following values for `strtype` are allowed:

- “empty”: checks if the string only consists of empty spaces.
- “unreal”: checks if the string is NA, or if it has literal string
  “NA”, “NaN” or “Inf”, regardless if it has leading or trailing spaces.
- “numeric”: checks if the string can be converted to a number,
  disregarding leading and trailing spaces. I.e. the string “5.0” can be
  converted to the the actual number 5.0.
- “special”: checks if the string consists of only special characters.

Here are some examples:

``` r

1e-20 %=numtype% "~0"
#> [1] TRUE
n <- c(0:5, 0:-5, 0.1, -0.1, 0, 1, Inf, -Inf, NA, NaN)
n[n %=numtype% "B"]
#> [1] 0 1 0 0 1
n[n %=numtype% "prop"]
#> [1] 0.0 1.0 0.0 0.1 0.0 1.0
n[n %=numtype% "B"]
#> [1] 0 1 0 0 1
n[n %=numtype% "I"]
#>  [1]  0  1  2  3  4  5  0 -1 -2 -3 -4 -5  0  1
n[n %=numtype% "odd"]
#> [1]  1  3  5 -1 -3 -5  1
n[n %=numtype% "even"]
#> [1]  0  2  4  0 -2 -4  0
n[n %=numtype% "R"]
#>  [1]  0.0  1.0  2.0  3.0  4.0  5.0  0.0 -1.0 -2.0 -3.0 -4.0 -5.0  0.1 -0.1  0.0
#> [16]  1.0
n[n %=numtype% "unreal"]
#> [1]  Inf -Inf   NA  NaN

s <- c(" AbcZ123 ", " abc ", " 1.3 ", " !#$%^&*() ", "  ", "  NA  ", "  NaN  ", " Inf ")
s[s %=strtype% "empty"]
#> [1] "  "
s[s %=strtype% "unreal"]
#> [1] "  NA  "  "  NaN  " " Inf "
s[s %=strtype% "numeric"]
#> [1] " 1.3 " " Inf "
s[s %=strtype% "special"]
#> [1] " !#$%^&*() "
```

 

# 3 Other

The `tinyoperations` R package also adds a few other things:

- The `lock_TF()` function locks `T` and `F` to `TRUE` and `FALSE`, to
  prevent the user from re-assigning them. Removing the created `T` and
  `F` constants allows re-assignment again.
- The `X %<-c% A` operator creates a `CONSTANT` `X` with assignment `A`.
  Constants cannot be changed, only accessed or removed. So if you have
  a piece of code that requires some unchangeable constant, use this
  operator to create said constant.

 
