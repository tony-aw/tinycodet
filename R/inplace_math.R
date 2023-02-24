#' In-place modifying mathematical arithmetic
#'
#' @description
#' In-place modifiers for addition, subtraction, multiplication, division, power, root, logarithm, and anti-logarithm. \cr
#'
#' \code{x %+ <-% y } is the same as \code{x <- x + y} \cr
#' \cr
#' \code{x %- <-% y } is the same as \code{x <- x - y} \cr
#' \cr
#' \code{x %* <-% y } is the same as \code{x <- x * y} \cr
#' \cr
#' \code{x %/ <-% y } is the same as \code{x <- x / y} \cr
#' \cr
#' \code{x %^ <-% p } is the same as \code{x <- x^p} \cr
#' \cr
#' \code{x %rt <-% p } is the same as \code{x <- x^(1/p)} \cr
#' \cr
#' \code{x %logb <-% b } is the same as \code{x <- log(x, base=b)} \cr
#' \cr
#' \code{x %alogb <-% b } is the same as \code{x <- b^x}; if \code{b=exp(1)}, this is the same as \code{x <- exp(x)} \cr
#'
#' @param x a number or numeric (or 'number-like') vector, matrix, or array.
#' @param y a number, or numeric (or 'number-like') vector, matrix, or array of the same length/dimension as \code{x}.
#'  It gives the number to add, substract, multiply by, or divide by.
#' @param p a number, or a numeric vector of the same length as \code{x}.
#' It gives the power to be used.
#' @param b a number, or a numeric vector of the same length as \code{x}.
#' It gives the logarithmic base to be used.
#'
#' @return
#' These operators do not return any value: they are in-place modifiers, and thus modify \code{x} directly.
#'
#' @examples
#' x <- matrix(rpois(10, 10), ncol=2)
#' print(x)
#' x %+ <-% 3 # same as x <- x + 3
#' print(x)
#'
#' x <- matrix(rpois(10, 10), ncol=2)
#' print(x)
#' x %- <-% 3 # same as x <- x - 3
#' print(x)
#'
#' x <- matrix(rpois(10, 10), ncol=2)
#' print(x)
#' x %* <-% 3 # same as x <- x * 3
#' print(x)
#'
#' x <- matrix(rpois(10, 10), ncol=2)
#' print(x)
#' x %/ <-% 3 # same as x <- x / 3
#' print(x)
#'
#' x <- matrix(rpois(10, 10), ncol=2)
#' print(x)
#' x %^ <-% 3 # same as x <- x^3
#' print(x)
#'
#' x <- matrix(rpois(10, 10), ncol=2)
#' print(x)
#' x %rt <-% 3 # same as x <- x^(1/3)
#' print(x)
#'
#' x <- matrix(rpois(10, 10), ncol=2)
#' print(x)
#' x %logb <-% 3 # same as x <- log(x, base=3)
#' print(x)
#'
#' x <- matrix(rpois(10, 10), ncol=2)
#' print(x)
#' x %alogb <-% 3 # same as x <- 3^x
#' print(x)
#'
#' x <- 3
#' print(x)
#' x %alogb <-% exp(1) # same as x <- exp(x)
#' print(x)
#' exp(3) # notice this is the same.
#'

#' @rdname inplace_math
#' @export
`%+ <-%` <- function(x, y) {
  eval(call("<-", substitute(x), x + y), envir = parent.frame(n = 1))
}

#' @rdname inplace_math
#' @export
`%- <-%` <- function(x, y) {
  eval(call("<-", substitute(x), x - y), envir = parent.frame(n = 1))
}

#' @rdname inplace_math
#' @export
`%* <-%` <- function(x, n) {
  eval(call("<-", substitute(x), x * n), envir = parent.frame(n = 1))
}

#' @rdname inplace_math
#' @export
`%/ <-%` <- function(x, y) {
  eval(call("<-", substitute(x), x / y), envir = parent.frame(n = 1))
}

#' @rdname inplace_math
#' @export
`%^ <-%` <- function(x, p) {
  eval(call("<-", substitute(x), x^p), envir = parent.frame(n = 1))
}

#' @rdname inplace_math
#' @export
`%rt <-%` <- function(x, p) {
  eval(call("<-", substitute(x), x^(1/p)), envir = parent.frame(n = 1))
}

#' @rdname inplace_math
#' @export
`%logb <-%` <- function(x, b) {
  eval(call("<-", substitute(x), log(x, base = b)), envir = parent.frame(n = 1))
}

#' @rdname inplace_math
#' @export
`%alogb <-%` <- function(x, b) {
  eval(call("<-", substitute(x), b^x), envir = parent.frame(n = 1))
}

