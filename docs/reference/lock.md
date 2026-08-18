# Lock T, Lock F, or Create Locked Constants

The `lock_TF()` function locks the `T` and `F` values and sets them to
`TRUE` and `FALSE`, respectively, to prevent the user from re-assigning
them.  
Removing the created `T` and `F` objects allows re-assignment again.  
  
The `X %<-c% A` operator creates a `constant` `X` and assigns `A` to
it.  
Constants cannot be changed, only accessed or removed. So if you have a
piece of code that requires some unchangeable `constant`, use this
operator to create said `constant`.  
Removing constant `X` also removes its binding lock. Thus to change a
`constant`, simply remove it and re-create it.  

## Usage

``` r
lock_TF(env)

X %<-c% A
```

## Arguments

- env:

  an optional environment to give, determining in which environment `T`
  and `F` should be locked.  
  When not specified, the current environment is used.  

- X:

  a syntactically valid unquoted name of the object to be created.

- A:

  any kind of object to be assigned to `X`.

## Value

For `lock_TF()`:  
Two `constants`, namely `T` and `F`, set to `TRUE` and `FALSE`
respectively, are created in the specified or else current environment,
and locked. Removing the created `T` and `F` objects allows
re-assignment again.  
  
For `X %<-c% A`:  
The object `X` containing `A` is created in the current environment, and
this object cannot be changed. It can only be accessed or removed.

## Details

Note that following statement

    x %<-c% 2+2
    print(x)

returns

    [1] 2

due to R's precedence rules. Therefore, in such cases, the right hand
side of `X %<-c% A` need to be surrounded with brackets. I.e.:

    x %<-c% (2 + 2)

Note that the `lock_TF()` function and `%s<-c%` operator create
constants through [lockBinding](https://rdrr.io/r/base/bindenv.html).  
The constants are protected from modification by copy, but they are
**not** protected from modification by reference (see for example
`collapse::`[setv](https://fastverse.org/collapse/reference/efficient-programming.html)).

## See also

[tinycodet_safer](https://tony-aw.github.io/tinycodet/reference/aaa1_tinycodet_safer.md)

## Examples

``` r
lock_TF()
X %<-c% data.frame(x = 3, y = 2) # this data.frame cannot be changed. Only accessed or removed.
X[1, ,drop=FALSE]
#>   x y
#> 1 3 2

```
