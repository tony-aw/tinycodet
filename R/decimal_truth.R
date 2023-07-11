#' Safer decimal number (in)equality testing operators
#'
#'@description
#' The \code{%d==%, %d!=% %d<%, %d>%, %d<=%, %d>=%} (in)equality operator
#' perform decimal (class "double") number truth testing. \cr
#' They are virtually equivalent to the regular (in)equality operators, \cr
#' \code{==, !=, <, >, <=, >=}, \cr
#' except for one aspect: \cr
#' The decimal number (in)equality operators assume that
#' if the absolute difference between any two numbers
#' \code{x} and \code{y}
#' is smaller than the Machine tolerance,
#' \code{sqrt(.Machine$double.eps)},
#' then \code{x} and \code{y}
#' should be consider to be equal. \cr
#' \cr
#' Thus these operators provide safer decimal number (in)equality tests. \cr
#' \cr
#' For example: \code{0.1*7 == 0.7} returns \code{FALSE}, even though they are equal,
#' due to the way decimal numbers are stored in programming languages (like R).
#' But \code{0.1*7 %d==% 0.7} returns \code{TRUE}. \cr
#' \cr
#' There are also the \code{x %d{}% bnd} and \code{x %d!{}% bnd} operators,
#' where \code{bnd} is a vector of length 2,
#' or a 2-column matrix (\code{nrow(bnd)==length(x)} or \code{nrow(bnd)==1}). \cr
#' The \code{x %d{}% bnd} operator checks if \code{x}
#' is within the closed interval with bounds defined by \code{bnd}. \cr
#' The \code{x %d!{}% bnd} operator checks if \code{x}
#' is outside the closed interval with bounds defined by \code{bnd}. \cr
#'
#' @param x,y numeric vectors, matrices, or arrays,
#' though these operators were specifically designed for decimal numbers (class "double").
#' @param bnd either a vector of length 2, or a matrix with 2 columns and 1 row,
#' or else a matrix with 2 columns where \code{nrow(bnd)==length(x)}. \cr
#' The first element/column of \code{bnd} gives the lower bound of the closed interval; \cr
#' The second element/column of \code{bnd} gives the upper bound of the closed interval; \cr
#'
#'
#' @seealso \link{tinyoperations_help}
#'
#' @examples
#' x <- c(0.3, 0.6, 0.7)
#' y <- c(0.1*3, 0.1*6, 0.1*7)
#' print(x); print(y)
#' x == y # gives FALSE, but should be TRUE
#' x!= y # gives TRUE, should be FALSE
#' x > y # not wrong
#' x < y # gives TRUE, should be FALSE
#' x %d==% y # here it's done correctly
#' x %d!=% y # correct
#' x %d<% y # correct
#' x %d>% y # correct
#' x %d<=% y # correct
#' x %d>=% y # correct
#'
#' x <- c(0.3, 0.6, 0.7)
#' bnd <- matrix(c(0.29, 0.59, 0.69, 0.31, 0.61, 0.71), ncol=2)
#' x %d{}% bnd
#' x %d!{}% bnd
#'
#' # These operators still work for non-decimal number numerics also:
#' x <- 1:5
#' y <- 1:5
#' x %d==% y
#' x %d!=% y
#' x %d<% y
#' x %d>% y
#' x %d<=% y
#' x %d>=% y
#'
#' x <- 1:5
#' y <- x+1
#' x %d==% y
#' x %d!=% y
#' x %d<% y
#' x %d>% y
#' x %d<=% y
#' x %d>=% y
#'
#' x <- 1:5
#' y <- x-1
#' x %d==% y
#' x %d!=% y
#' x %d<% y
#' x %d>% y
#' x %d<=% y
#' x %d>=% y

#' @name decimal_truth
NULL

#' @rdname decimal_truth
#' @export
`%d==%` <- function(x, y) {
  return(abs(x - y) < sqrt(.Machine$double.eps))
}

#' @rdname decimal_truth
#' @export
`%d!=%` <- function(x, y) {
  return(abs(x - y) >= sqrt(.Machine$double.eps))
}

#' @rdname decimal_truth
#' @export
`%d<%` <- function(x, y) {
  check <- (x %d!=% y)
  return((x < y) & check)
}

#' @rdname decimal_truth
#' @export
`%d>%` <- function(x, y) {
  check <- (x %d!=% y)
  return((x > y) & check)
}

#' @rdname decimal_truth
#' @export
`%d<=%` <- function(x, y) {
  check <- (x %d==% y)
  return((x <= y) | check)
}

#' @rdname decimal_truth
#' @export
`%d>=%` <- function(x, y) {
  check <- (x %d==% y)
  return((x >= y) | check)
}

#' @rdname decimal_truth
#' @export
`%d{}%` <- function(x, bnd) {
  bnd <- matrix(bnd, ncol=2)
  check <- nrow(bnd)==1 | nrow(bnd)==length(x)
  if(!check){stop("nrow(bnd) must be equal to 1 or equal the number of elements of x")}
  return(x %d>=% bnd[,1] & x %d<=% bnd[,2])
}

#' @rdname decimal_truth
#' @export
`%d!{}%` <- function(x, bnd) {
  bnd <- matrix(bnd, ncol=2)
  check <- nrow(bnd)==1 | nrow(bnd)==length(x)
  if(!check){stop("nrow(bnd) must be equal to 1 or equal the number of elements of x")}
  return(x %d<% bnd[,1] | x %d>% bnd[,2])
}

#' @rdname decimal_truth
#' @export
tinyoperations_decimal_truth <- function() {
  utils::`?`(tinyoperations_decimal_truth)
}
