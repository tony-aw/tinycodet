# Row- or Column-wise Re-ordering of Matrices

Infix operators for custom row- and column-wise re-ordering of
matrices.  
  
The `x %row~% mat` operator re-orders the elements of every row, each
row ordered independently from the other rows, of matrix `x`, according
to the ordering ranks given in matrix `mat`.  
  
The `x %col~% mat` operator re-orders the elements of every column, each
column ordered independently from the other columns, of matrix `x`,
according to the ordering ranks given in matrix `mat`.  
  
Note that these operators strip all attributes, except dimensions.  
  

## Usage

``` r
x %row~% mat

x %col~% mat
```

## Arguments

- x:

  a matrix

- mat:

  a numeric matrix with the same dimensions as `x`, giving the new
  ordering ranks for every element of matrix `x`.  
    

## Value

A re-ordered matrix.

## See also

[tinycodet_dry](https://tony-aw.github.io/tinycodet/reference/aaa4_tinycodet_dry.md)

## Examples

``` r
# numeric matrix ====

x <- matrix(sample(1:25), nrow = 5)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   13   15   22   14   25
#> [2,]   23   20   19   10   11
#> [3,]   12    4    8   18   17
#> [4,]    5   21    2   16    1
#> [5,]    6    9    7   24    3
x %row~% x # sort elements of every row independently
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   13   14   15   22   25
#> [2,]   10   11   19   20   23
#> [3,]    4    8   12   17   18
#> [4,]    1    2    5   16   21
#> [5,]    3    6    7    9   24
x %row~% -x # reverse-sort elements of every row independently
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25   22   15   14   13
#> [2,]   23   20   19   11   10
#> [3,]   18   17   12    8    4
#> [4,]   21   16    5    2    1
#> [5,]   24    9    7    6    3
x %col~% x # sort elements of every column independently
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    5    4    2   10    1
#> [2,]    6    9    7   14    3
#> [3,]   12   15    8   16   11
#> [4,]   13   20   19   18   17
#> [5,]   23   21   22   24   25
x %col~% -x # reverse-sort elements of every column independently
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   23   21   22   24   25
#> [2,]   13   20   19   18   17
#> [3,]   12   15    8   16   11
#> [4,]    6    9    7   14    3
#> [5,]    5    4    2   10    1

x <- matrix(sample(1:25), nrow = 5)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   22    9    8    3    5
#> [2,]   11   10   25   13   16
#> [3,]    7    2    1   20   15
#> [4,]   21   17   14   23   12
#> [5,]    6    4   18   24   19
mat <- sample(seq_along(x)) |> matrix(ncol = ncol(x))
x %row~% mat # randomly shuffle every row independently
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    8    3    9    5   22
#> [2,]   25   11   13   10   16
#> [3,]    2   20    7    1   15
#> [4,]   21   17   12   14   23
#> [5,]    6   19   24    4   18
x %col~% mat # randomly shuffle every column independently
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    6   17    8   20   12
#> [2,]   21    2   25    3   19
#> [3,]    7    9   14   23    5
#> [4,]   11    4    1   24   16
#> [5,]   22   10   18   13   15

# character matrix ====

x <- matrix(sample(letters, 25), nrow = 5)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "b"  "n"  "t"  "r"  "k" 
#> [2,] "f"  "e"  "a"  "z"  "d" 
#> [3,] "g"  "x"  "m"  "p"  "h" 
#> [4,] "l"  "w"  "y"  "s"  "i" 
#> [5,] "v"  "q"  "o"  "c"  "j" 
mat <- stringi::stri_rank(as.vector(x)) |> matrix(ncol = ncol(x))
x %row~% mat # sort elements of every row independently
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "b"  "k"  "n"  "r"  "t" 
#> [2,] "a"  "d"  "e"  "f"  "z" 
#> [3,] "g"  "h"  "m"  "p"  "x" 
#> [4,] "i"  "l"  "s"  "w"  "y" 
#> [5,] "c"  "j"  "o"  "q"  "v" 
x %row~% -mat # reverse-sort elements of every row independently
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "t"  "r"  "n"  "k"  "b" 
#> [2,] "z"  "f"  "e"  "d"  "a" 
#> [3,] "x"  "p"  "m"  "h"  "g" 
#> [4,] "y"  "w"  "s"  "l"  "i" 
#> [5,] "v"  "q"  "o"  "j"  "c" 
x %col~% mat # sort elements of every column independently
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "b"  "e"  "a"  "c"  "d" 
#> [2,] "f"  "n"  "m"  "p"  "h" 
#> [3,] "g"  "q"  "o"  "r"  "i" 
#> [4,] "l"  "w"  "t"  "s"  "j" 
#> [5,] "v"  "x"  "y"  "z"  "k" 
x %col~% -mat # reverse-sort elements of every column independently
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "v"  "x"  "y"  "z"  "k" 
#> [2,] "l"  "w"  "t"  "s"  "j" 
#> [3,] "g"  "q"  "o"  "r"  "i" 
#> [4,] "f"  "n"  "m"  "p"  "h" 
#> [5,] "b"  "e"  "a"  "c"  "d" 

x <- matrix(sample(letters, 25), nrow = 5)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "b"  "f"  "z"  "p"  "l" 
#> [2,] "h"  "m"  "g"  "v"  "o" 
#> [3,] "i"  "t"  "d"  "q"  "y" 
#> [4,] "s"  "c"  "k"  "u"  "a" 
#> [5,] "x"  "w"  "n"  "r"  "j" 
mat <- sample(seq_along(x)) |> matrix(ncol = ncol(x))
x %row~% mat # randomly shuffle every row independently
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "f"  "z"  "l"  "p"  "b" 
#> [2,] "m"  "v"  "g"  "h"  "o" 
#> [3,] "y"  "t"  "q"  "i"  "d" 
#> [4,] "a"  "s"  "k"  "c"  "u" 
#> [5,] "n"  "w"  "r"  "x"  "j" 
x %col~% mat # randomise shuffle every column independently
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "s"  "m"  "z"  "v"  "y" 
#> [2,] "b"  "t"  "g"  "p"  "a" 
#> [3,] "h"  "f"  "n"  "q"  "l" 
#> [4,] "i"  "w"  "k"  "r"  "o" 
#> [5,] "x"  "c"  "d"  "u"  "j" 
```
