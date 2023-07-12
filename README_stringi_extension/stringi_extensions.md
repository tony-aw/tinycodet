
- [1 Matrix re-ordering operators](#1-matrix-re-ordering-operators)
- [2 String functions](#2-string-functions)
  - [2.1 Matrix joining](#21-matrix-joining)
  - [2.2 stri_locate_ith](#22-stri_locate_ith)
  - [2.3 Substr - functions](#23-substr---functions)
- [3 String infix operators](#3-string-infix-operators)
  - [3.1 sgrep](#31-sgrep)
  - [3.2 String subsetting operators](#32-string-subsetting-operators)
  - [3.3 String arithmetic](#33-string-arithmetic)
  - [3.4 In-place modifying string arithmetic and
    sub-setting](#34-in-place-modifying-string-arithmetic-and-sub-setting)
  - [3.5 Specifying Pattern search attributes in string infix
    operators](#35-specifying-pattern-search-attributes-in-string-infix-operators)
- [4 Multi-threading of
  Substr-functions](#4-multi-threading-of-substr-functions)
- [5 Compatibility with other R
  packages](#5-compatibility-with-other-r-packages)

<!-- README.md is generated from README.Rmd. Please edit that file -->

 

# 1 Matrix re-ordering operators

The `tinyoperations` R package adds 2 additional matrix operators:

- The `x %row~% mat` operator re-orders the elements within every row of
  matrix `x` by the ordering ranks given in matrix `mat`.
- The `x %col~% mat` operator re-orders the elements within every column
  of matrix `x` by the ordering ranks given in matrix `mat`.

If matrix `x` is a numeric matrix, and one wants to numerically sort the
elements of every row or column, `x %row~% x` or `x %col~% x` would
suffice, respectively.

If matrix `x` is not numeric, sorting using `x %row~% x` and
`x %col~% x`are still possible, but probably not the best option. In the
non-numeric case, providing a matrix of ordering ranks would probably be
faster and give more accurate ordering.

If `mat` is a matrix of non-repeating random integers
(i.e. `sample(1:length(x), replace=FALSE)`), `x %row~% mat` will
randomly shuffle the elements of every row, where the shuffling order of
every row is independent of the other rows. Similarly, `x %col~% mat`
will randomly shuffle the elements of every column, where the shuffling
order of every column is independent of the other columns.

 

Examples with a numeric matrix:

``` r
x <- matrix(sample(1:25), nrow=5)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25   11   16    9    8
#> [2,]    4   14   10   15   13
#> [3,]    7   18    6   12   21
#> [4,]    1   22   19   17    3
#> [5,]    2    5   23   20   24
x %row~% x # sort elements of every row
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    8    9   11   16   25
#> [2,]    4   10   13   14   15
#> [3,]    6    7   12   18   21
#> [4,]    1    3   17   19   22
#> [5,]    2    5   20   23   24
x %row~% -x # reverse-sort elements of every row
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25   16   11    9    8
#> [2,]   15   14   13   10    4
#> [3,]   21   18   12    7    6
#> [4,]   22   19   17    3    1
#> [5,]   24   23   20    5    2
x %col~% x # sort elements of every column
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    5    6    9    3
#> [2,]    2   11   10   12    8
#> [3,]    4   14   16   15   13
#> [4,]    7   18   19   17   21
#> [5,]   25   22   23   20   24
x %col~% -x # reverse-sort elements of every column
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25   22   23   20   24
#> [2,]    7   18   19   17   21
#> [3,]    4   14   16   15   13
#> [4,]    2   11   10   12    8
#> [5,]    1    5    6    9    3

x <- matrix(sample(1:25), nrow=5)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   25    3   23    9   11
#> [2,]   12    6    4    7   21
#> [3,]   15   10   24   16    2
#> [4,]    1   18   14   22    8
#> [5,]   20   19   17   13    5
rand <- sample(1:length(x)) |> matrix(ncol=ncol(x)) # matrix of random integers
x %row~% rand # random shuffle every row independent of other rows
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   23   11    9    3   25
#> [2,]   12    4    6    7   21
#> [3,]    2   15   24   16   10
#> [4,]    1   14    8   18   22
#> [5,]   19   20   17    5   13
x %col~% rand # random shuffle every column independent of other columns
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]   20   19   23   16   11
#> [2,]   15    3   17    9    2
#> [3,]   12    6   24   13    5
#> [4,]    1   10    4    7    8
#> [5,]   25   18   14   22   21
```

Examples with a character matrix:

``` r
x <- matrix(sample(letters, 25), nrow=5)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "w"  "l"  "s"  "q"  "e" 
#> [2,] "n"  "p"  "i"  "b"  "j" 
#> [3,] "t"  "a"  "z"  "r"  "k" 
#> [4,] "g"  "v"  "y"  "d"  "x" 
#> [5,] "m"  "f"  "c"  "u"  "o"
mat <- stringi::stri_rank(as.vector(x)) |>
  matrix(ncol=ncol(x)) # matrix of ordering ranks
x %row~% mat # sort elements of every row
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "e"  "l"  "q"  "s"  "w" 
#> [2,] "b"  "i"  "j"  "n"  "p" 
#> [3,] "a"  "k"  "r"  "t"  "z" 
#> [4,] "d"  "g"  "v"  "x"  "y" 
#> [5,] "c"  "f"  "m"  "o"  "u"
x %row~% -mat # reverse-sort elements of every row
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "w"  "s"  "q"  "l"  "e" 
#> [2,] "p"  "n"  "j"  "i"  "b" 
#> [3,] "z"  "t"  "r"  "k"  "a" 
#> [4,] "y"  "x"  "v"  "g"  "d" 
#> [5,] "u"  "o"  "m"  "f"  "c"
x %col~% mat # sort elements of every column
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "g"  "a"  "c"  "b"  "e" 
#> [2,] "m"  "f"  "i"  "d"  "j" 
#> [3,] "n"  "l"  "s"  "q"  "k" 
#> [4,] "t"  "p"  "y"  "r"  "o" 
#> [5,] "w"  "v"  "z"  "u"  "x"
x %col~% -mat # reverse-sort elements of every column
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "w"  "v"  "z"  "u"  "x" 
#> [2,] "t"  "p"  "y"  "r"  "o" 
#> [3,] "n"  "l"  "s"  "q"  "k" 
#> [4,] "m"  "f"  "i"  "d"  "j" 
#> [5,] "g"  "a"  "c"  "b"  "e"

x <- matrix(sample(letters, 25), nrow=5)
print(x)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "z"  "u"  "c"  "f"  "s" 
#> [2,] "o"  "l"  "w"  "m"  "r" 
#> [3,] "x"  "g"  "v"  "d"  "n" 
#> [4,] "j"  "h"  "k"  "t"  "b" 
#> [5,] "p"  "a"  "q"  "e"  "y"
rand <- sample(1:length(x)) |> matrix(ncol=ncol(x)) # matrix of random integers
x %row~% rand # random shuffle every row independent of other rows
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "z"  "f"  "s"  "u"  "c" 
#> [2,] "r"  "l"  "w"  "o"  "m" 
#> [3,] "n"  "x"  "d"  "v"  "g" 
#> [4,] "k"  "b"  "j"  "h"  "t" 
#> [5,] "a"  "p"  "y"  "e"  "q"
x %col~% rand # random shuffle every column independent of other columns
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] "x"  "a"  "k"  "d"  "n" 
#> [2,] "z"  "l"  "w"  "f"  "r" 
#> [3,] "p"  "u"  "v"  "e"  "b" 
#> [4,] "o"  "g"  "q"  "m"  "y" 
#> [5,] "j"  "h"  "c"  "t"  "s"
```

These operators internally only use vectorized operations (no loops or
apply-like functions), and are faster than re-ordering matrices using
loops or apply-like functions.

 

# 2 String functions

## 2.1 Matrix joining

The `tinyoperations` package adds a tiny additional function to
`stringi`:

`stri_join_mat` (and their aliases `stri_c_mat` and `stri_paste_mat`).

As the name suggests, these functions perform row-wise (`margin=1`; the
default) or column-wise (`margin=2`) joining of a matrix of strings,
thereby transforming it to a vector of strings. You can do this already
in base R, but it requires converting the matrix to a data.frame or
list, and then calling `stri_join` inside `do.call()`, which to me just
seems too much trouble for something *soooo* abysmally simple.

Here is an example of their usage when re-ordering strings, words, or
sentences :

``` r
# sorting characters in strings:
x <- c("Hello world", "Goodbye world")
print(x)
#> [1] "Hello world"   "Goodbye world"
x <- stringi::stri_split_boundaries(x, simplify = TRUE, type="character")
mat <- stringi::stri_rank(as.vector(x)) |>  matrix(ncol=ncol(x))
sorted <- x %row~% mat # fast sort matrix row-wise
stri_join_mat(sorted, margin=1, sep="") # <- using new function here
#> [1] " deHllloorw"   " bddeGlooorwy"

# sorting words:
x <- c("Hello everyone", "Goodbye everyone")
print(x)
#> [1] "Hello everyone"   "Goodbye everyone"
x <- stringi::stri_split_boundaries(x, simplify = TRUE, type="word")
mat <- stringi::stri_rank(as.vector(x)) |>  matrix(ncol=ncol(x))
sorted <- x %row~% mat # <- fast sort matrix row-wise
stri_c_mat(sorted, margin=1, sep=" ") # <- alias for stri_join_mat
#> [1] "  everyone Hello"   "  everyone Goodbye"

# randomly shuffle sentences:
x <- c("Hello, who are you? Oh, really?! Cool!", "I don't care. But I really don't.")
print(x)
#> [1] "Hello, who are you? Oh, really?! Cool!"
#> [2] "I don't care. But I really don't."
x <- stringi::stri_split_boundaries(x, simplify = TRUE, type="sentence")
mat <- sample(1:length(x)) |> matrix(ncol=ncol(x))
shuffled <- x %row~% mat # <- fast sort matrix row-wise
print(shuffled)
#>      [,1]                   [,2]             [,3]                 
#> [1,] "Hello, who are you? " "Oh, really?! "  "Cool!"              
#> [2,] ""                     "I don't care. " "But I really don't."
stri_paste_mat(shuffled, margin=1, sep=" ") # <- another alias for stri_join_mat
#> [1] "Hello, who are you?  Oh, really?!  Cool!"
#> [2] " I don't care.  But I really don't."
```

 

## 2.2 stri_locate_ith

Suppose one wants to transform the **first** vowels in the strings of a
character vector `str`, such that all upper case vowels become lower
case, and vice-versa. One can do that completely in `stringi` + base R
as follows:

``` r

x <- c("HELLO WORLD", "goodbye world")
loc <- stringi::stri_locate_first(x, regex="a|e|i|o|u", case_insensitive=TRUE)
extr <- stringi::stri_sub(x, from=loc)
repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
stringi::stri_sub_replace(x, loc, replacement=repl)
#> [1] "HeLLO WORLD"   "gOodbye world"
```

But now suppose one wants to transform the **second-last** vowel. How
are you going to do that? It’s not impossible, but also not super
straight-forward. For clear code, `stringi` really needs some kind of
“stri_locate_ith” function. And, of course, the `tinyoperations` package
provides just that.

The `stri_locate_ith(str, i, ...)` function locates for every
element/string in character vector `str`, the $i^\textrm{th}$ occurrence
of some (regex/fixed/etc) pattern. When `i` is positive, the occurrence
is counted from left to right. Negative values for `i` are also allowed,
in which case the occurrence is counted from the right to left. But
`i=0` is not allowed though. Thus, to get the **second** occurrence of
some pattern, use `i=2`, and to get the **second-last** occurrence, use
`i=-2`.

The `stri_locate_ith(str, i, ...)` function uses the exact same argument
and naming convention as `stringi`, to keep your code consistent. And
just like `stringi::stri_locate_first/last`, the
`stri_locate_ith(str, i, ...)` function is a vectorized function: `str`
and `i` as well as the pattern (`regex, fixed, coll, charclass`) can all
be different-valued vectors. It is also vectorized in the sense that no
loops (in R) are used, only vectorized functions.

 

To transform the **second-last** occurrence, one can now use
`stri_locate_ith()` in a very similar way as was done with
`stri_locate_first/last`:

``` r
x <- c("HELLO WORLD", "goodbye world")

loc <- stri_locate_ith( # this part is the key-difference
  x, -2, regex="a|e|i|o|u", case_insensitive=TRUE
)

extr <- stringi::stri_sub(x, from=loc)
repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
stringi::stri_sub_replace(x, loc, replacement=repl)
#> [1] "HELLo WORLD"   "goodbyE world"
```

Notice that the code is virtually equivalent. We *only* need to change
the locate function.

Regarding its speed:

The `stri_locate_ith()` function needs to know the number of matches of
some pattern for every string in a character vector, as well as the
actual matches themselves, before being able to actually give the
$i^\textrm{th}$ occurrence of some pattern. Thus `stri_locate_ith()` (at
least in its current implementation) cannot be faster than the combined
run-time of the `stri_locate_all()` and `stri_count()` functions. As
`stri_locate_ith()` is written only in fully vectorized statements in R
(no loops), the function is very fast, and takes about as much time as
the time of `stri_locate_all()` and `stri_count()` combined.

 

## 2.3 Substr - functions

The `tinyoperations` R-package includes the following “substr-”
functions:

- The `substr_repl(x, rp, ...)` function replaces a position (range)
  with string `rp`.
- The `substr_chartr(x, old, new, ...)` function transforms the
  sub-string at a position (range) using `chartr(old, new)`. By default,
  it will translate upper-case characters to lower-case, and vice-versa.
- The `substr_addin(x, addition, side, ...)` function adds the
  additional string `addition` at the side `side` of a position.
- The `substr_extract(x, type, ...)` function extracts the string at,
  before, or after some position (range).
- The `substr_arrange(x, arr, ...)` function re-arranges (sort, reverse,
  randomize) the sub-string at a position (range).

The “position” in the functions above can be specified either by giving
the result of the `stri_locate_ith()` function (see the previous
section) in argument `loc`, or one can give manual positions using the
`start, end` or `at` arguments.

Examples:

``` r
x <- c("Good - good - GOOD", "Good - GOOD - good")
loc <- stri_locate_ith(
  # locate second "good" (ignore case) of each string in x:
  x, 2, regex="good", case_insensitive=TRUE 
)
```

|                                           |                       |                       |
|:------------------------------------------|:----------------------|:----------------------|
| x                                         | Good - good - GOOD    | Good - GOOD - good    |
| substr_extract(x, loc=loc)                | good                  | GOOD                  |
| substr_extract(x, “before”, loc=loc)      | Good -                | Good -                |
| substr_extract(x, “after”, loc=loc)       | \- GOOD               | \- good               |
| substr_repl(x, “??”, loc=loc)             | Good - ?? - GOOD      | Good - ?? - good      |
| substr_chartr(x, loc=loc)                 | Good - GOOD - GOOD    | Good - good - good    |
| substr_addin(x, “\~~”, “after”, loc=loc)  | Good - good\~~ - GOOD | Good - GOOD\~~ - good |
| substr_addin(x, “\~~”, “before”, loc=loc) | Good - \~~good - GOOD | Good - \~~GOOD - good |
| substr_arrange(x, loc=loc)                | Good - dgoo - GOOD    | Good - DGOO - good    |
| substr_arrange(x, “decr”, loc=loc)        | Good - oogd - GOOD    | Good - OOGD - good    |
| substr_arrange(x, “rev”, loc=loc)         | Good - doog - GOOD    | Good - DOOG - good    |
| substr_arrange(x, “rand”, loc=loc)        | Good - ogod - GOOD    | Good - ODGO - good    |

 

# 3 String infix operators

The `tinyoperations` R package implements infix operators for string
arithmetic and sub-setting, as well some of their in-place modifier
equivalents. For consistency, and to avoid masking other common
operators, all string-related operators start with `%s`, where the “s”
stands for “string”.

 

## 3.1 sgrep

The `s %sgrep% p` checks if pattern `p` appears in character vector `s`.

## 3.2 String subsetting operators

As a first sub-setting operator, we have `x %sget% ss`, which returns a
subset of each string in character vector `x`. Here `ss` is a vector of
length 2, or a matrix with `nrow(ss)=length(x)` and 2 columns. The
object `ss` should consist entirely of non-negative integers (thus 0, 1,
2, etc. are valid, but -1, -2, -3 etc are not valid). The first
element/column of ss gives the number of characters counting from the
left side to be extracted from x. The second element/column of ss gives
the number of characters counting from the right side to be extracted
from x.

Here are 2 examples:

``` r
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(2,3)
x %sget% ss
#> [1] "abklm" "noxyz"

x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(1,0)
x %sget% ss
#> [1] "a" "n"
```

Thus `x %sget% ss` “gets” or extracts the given number of characters
from the left and the right, and removes the rest. There is also
`x %strim% ss`, which is the opposite: it trims away the number of
characters from the left and right as defined in the matrix `ss`,
leaving you with whatever is left.

Here are again 2 examples:

``` r
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(2,3)
x %strim% ss
#> [1] "cdefghij" "pqrstuvw"

x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
#> [1] "abcdefghijklm" "nopqrstuvwxyz"
ss <- c(1,0)
x %strim% ss
#> [1] "bcdefghijklm" "opqrstuvwxyz"
```

 

## 3.3 String arithmetic

The `tinyoperations` package adds 4 string arithmetic operators:

- `x %s+% y` concatenates `x` and `y`;
- `x %s-% p` removes pattern `p` from each string in character vector
  `x`;
- `x %s*% n` repeats each string in character vector `x` for `n` times;
- `x %s/% p` counts how often pattern `p` occurs in string or vector `x`

I.e.:

``` r
"Hello "%s+% " world"
#> [1] "Hello  world"
c("Hello world", "Goodbye world") %s-% " world"
#> [1] "Hello"   "Goodbye"
c("Ha", "Ho", "Hi", "Hu", "He", "Ha") %s*% 2:7
#> [1] "HaHa"           "HoHoHo"         "HiHiHiHi"       "HuHuHuHuHu"    
#> [5] "HeHeHeHeHeHe"   "HaHaHaHaHaHaHa"
c("hello world & goodbye world", "world domination!") %s/% "world"
#> [1] 2 1
```

The right-side arguments `p`, `y`, and `n` can be a single value, or a
vector of the same length as `x`.

 

## 3.4 In-place modifying string arithmetic and sub-setting

With the exception of `%ss%`, all infix operators for string arithmetic
and string sub-setting have their in-place modifying equivalent:

- `x %s+ =% y` is the same as `x <- x %s+% y`
- `x %s- =% p` is the same as `x <- x %s-% p`
- `x %s* =% n` is the same as `x <- x %s*% n`
- `x %s/ =% p` is the same as `x <- x %s/% p`
- `x %sget =% ss` is the same as `x <- x %sget% ss`
- `x %strim =% ss` is the same as `x <- x %strim% ss`

Notice the extra space before the `=` sign.

 

## 3.5 Specifying Pattern search attributes in string infix operators

The `x %s-% p` and `x %s/% p` operators (and their in-place equivalents,
given later), and the `%sgrep%` operator perform pattern matching for
various purposes. By default the pattern matching is interpreted as
case-sensitive `regex` patterns from `stringi`.

But, of course, sometimes one wants to change this. For example, one may
want it to be case insensitive. Or perhaps one wants to use fixed
expressions, or something else.

The `tinyoperations` package provides options for these cases. To use
more refined pattern definition, simply replace the
argument/right-hand-side expression `p` in the relevant operators with a
call from the `s_pattern()` function.

The `s_pattern()` function uses the exact same argument convention as
`stringi`. For example:

- `s_pattern(regex=p, case_insensitive=FALSE, ...)`
- `s_pattern(fixed=p, ...)`
- `s_pattern(coll=p, ...)`
- `s_pattern(charclass=p, ...)`

Note that the `s_pattern()` function only works for the infix operators
(functions surrounded by `%` signs) related to strings in this R
package.

Examples with Regular expressions:

``` r
x <- c("Hello world", "Goodbye world")
p <- s_pattern(regex=" world")
x %s-% p
#> [1] "Hello"   "Goodbye"

x <- c("Ha", "Ho", "Hi", "Hu", "He", "Ha") %s*% 10
p <- s_pattern(regex="Ha")
x %s/% p
#> [1] 10  0  0  0  0 10
```

Examples with Fixed expressions:

``` r
x <- c("Hello world", "Goodbye world")
p <- s_pattern(fixed=" world")
x %s-% p
#> [1] "Hello"   "Goodbye"

x <- c("Ha", "Ho", "Hi", "Hu", "He", "Ha") %s*% 10
p <- s_pattern(fixed="Ha")
x %s/% p
#> [1] 10  0  0  0  0 10
```

And so on. I’m sure you get the idea.

 

# 4 Multi-threading of Substr-functions

All the string sub-setting functions have the `fish` argument, which is
`FALSE` by default. If `fish=TRUE`, these functions will use
`stringfish` functions instead of base R and `stringi` functions. The
stringfish functions are usually faster, and also allow native
multi-threading. Note that `stringfish` must be installed in order for
this to work. Multi-threading in `stringfish` can be set-up by running
`setoption(stringfish.nthreads=cl)` somewhere at the start of your code,
where `cl` is the number of threads you want to use.

Don’t use multi-threading unless you need to, as multi-threading has
some overhead, thus its only faster with very large character vectors.

Example:

``` r
x <- c("Goodmorning--Goodevening, and Goodnight",
       paste0(letters[1:13], collapse=""))
print(x)
#> [1] "Goodmorning--Goodevening, and Goodnight"
#> [2] "abcdefghijklm"
loc <- stri_locate_ith(
  # locate second-last occurrence of "good" (ignore case) of each string in x:
  x, -2, regex="good", case_insensitive=TRUE 
)
substr_extract(x, loc=loc, fish = TRUE)
#> [1] "Good" NA
substr_extract(x, "before", loc=loc, fish = TRUE)
#> [1] "Goodmorning--" NA
substr_extract(x, "after", loc=loc, fish = TRUE)
#> [1] "evening, and Goodnight" NA
substr_repl(x, "??", loc=loc, fish = TRUE)
#> [1] "Goodmorning--??evening, and Goodnight"
#> [2] "abcdefghijklm"
substr_chartr(x, loc=loc, fish = TRUE)
#> [1] "Goodmorning--gOODevening, and Goodnight"
#> [2] "abcdefghijklm"
substr_addin(x, " ", "after", loc=loc, fish = TRUE) 
#> [1] "Goodmorning--Good evening, and Goodnight"
#> [2] "abcdefghijklm"
substr_addin(x, " ", "before", loc=loc, fish = TRUE) 
#> [1] "Goodmorning-- Goodevening, and Goodnight"
#> [2] "abcdefghijklm"
substr_arrange(x, loc=loc, fish = TRUE)
#> [1] "Goodmorning--dGooevening, and Goodnight"
#> [2] "abcdefghijklm"
substr_arrange(x, "rev", loc=loc, fish = TRUE)
#> [1] "Goodmorning--dooGevening, and Goodnight"
#> [2] "abcdefghijklm"
substr_arrange(x, "decr", loc=loc, fish = TRUE)
#> [1] "Goodmorning--ooGdevening, and Goodnight"
#> [2] "abcdefghijklm"
substr_arrange(x, "rand", loc=loc, fish = TRUE)
#> [1] "Goodmorning--Gdooevening, and Goodnight"
#> [2] "abcdefghijklm"
```

 

# 5 Compatibility with other R packages

The `stringi` R package has the `%s+%` and `%s*%` operators. They do
exactly the same things as in `tinyoperations`, and so the masking of
these functions can safely be ignored.

 
