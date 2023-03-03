#' Safer float (in)equality operators
#'
#'@description
#' The \code{%==%, %!=% %<%, %>%, %<=%, %>=%} operators perform "float logic".
#' They are virtually equivalent to the regular (in)equality operators, \cr
#' \code{==, !=, <, >, <=, >=}, \cr
#' except for one aspect.
#' The float logic operators assume that if the absolute difference between
#' \code{x} and \code{y} is smaller than the Machine tolerance,
#' \code{sqrt(.Machine$double.eps)}, then \code{x} and \code{y}
#' ought to be consider to be equal. \cr
#' Thus these provide safer float logic. \cr
#' For example: \code{0.1*7 == 0.7} returns \code{FALSE}, even though they are equal,
#' due to the way floating numbers are stored in programming languages like R. \cr
#' \cr
#'
#' @param x,y numeric vectors, matrices, or arrays.
#' though these operators were specifically designed for floats ("double").
#'
#' @examples
#' x <- c(0.3, 0.6, 0.7)
#' y <- c(0.1*3, 0.1*6, 0.1*7)
#' print(x); print(y)
#' x == y # gives FALSE, but should be TRUE
#' x!= y # gives TRUE, should be FALSE
#' x > y # not wrong
#' x < y # gives TRUE, should be FALSE
#' x %==% y # here it's done correctly
#' x %<% y # correct
#' x %>% y # correct
#' x %<=% y # correct
#' x %>=% y # correct
#'
#' # These operators still work for non-float numerics also:
#' x <- 1:5
#' y <- 1:5
#' x %==% y
#' x %<% y
#' x %>% y
#' x %<=% y
#' x %>=% y
#'
#' x <- 1:5
#' y <- x+1
#' x %==% y
#' x %<% y
#' x %>% y
#' x %<=% y
#' x %>=% y
#'
#' x <- 1:5
#' y <- x-1
#' x %==% y
#' x %<% y
#' x %>% y
#' x %<=% y
#' x %>=% y

#' @rdname float_logic
#' @export
`%==%` <- function(x, y) {
  return(abs(x - y) < sqrt(.Machine$double.eps))
}

#' @rdname float_logic
#' @export
`%!=%` <- function(x, y) {
  return(abs(x - y) >= sqrt(.Machine$double.eps))
}

#' @rdname float_logic
#' @export
`%<%` <- function(x, y) {
  check <- (x %!=% y)
  return((x < y) & check)
}

#' @rdname float_logic
#' @export
`%>%` <- function(x, y) {
  check <- (x %!=% y)
  return((x > y) & check)
}

#' @rdname float_logic
#' @export
`%<=%` <- function(x, y) {
  check <- (x %==% y)
  return((x <= y) | check)
}

#' @rdname float_logic
#' @export
`%>=%` <- function(x, y) {
  check <- (x %==% y)
  return((x >= y) | check)
}

