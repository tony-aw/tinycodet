# Set Safer Dollar, Arguments, and Attribute Matching

The `safer_partialmatch()` function simply calls the following:

    options(
       warnPartialMatchDollar = TRUE,
       warnPartialMatchArgs = TRUE,
       warnPartialMatchAttr = TRUE
     )

Thus it forces 'R' to give a warning when partial matching occurs when
using the dollar ([\$](https://rdrr.io/r/base/Extract.html)) operator,
or when other forms of partial matching occurs.  
The `safer_partialmatch()` function is intended for when running R
interactively (see
[interactive](https://rdrr.io/r/base/interactive.html)).  
  

## Usage

``` r
safer_partialmatch()
```

## Value

Sets the options. Returns nothing.  

## See also

[tinycodet_safer](https://tony-aw.github.io/tinycodet/reference/aaa1_tinycodet_safer.md)

## Examples

``` r
if (FALSE) { # interactive()
interactive()


safer_partialmatch()
data(iris)
head(iris)
iris$Sepal.Length <- iris$Sepal.Length^2
head(iris)
}
```
