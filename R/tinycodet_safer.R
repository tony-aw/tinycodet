#' Overview of the 'tinycodet' "Safer" Functionality
#'
#' @description
#' To help make your code safer, the 'tinycodet' R-package
#' introduces a few functions:
#'
#'  * \link[=%d==%]{Safer decimal (in)equality testing}.
#'  * \link[=as_int]{Atomic type casting without stripping attributes}.
#'  * The \link{lock_TF} function to
#'  set and lock \code{T} and \code{F} to \code{TRUE} and \code{FALSE}.
#'  * The \link{%<-c%} operator to assign locked constants.
#'
#'
#'
#'
#' @seealso [tinycodet_help()]
#'
#' @examples
#' x <- c(0.3, 0.6, 0.7)
#' y <- c(0.1*3, 0.1*6, 0.1*7)
#' x == y # gives FALSE, but should be TRUE
#' x %d==% y # here it's done correctly
#'
#'
#'

#' @rdname tinycodet_safer
#' @name tinycodet_safer
NULL
