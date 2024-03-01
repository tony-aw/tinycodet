#' Overview of the 'tinycodet' "Safer" Functionality
#'
#' @description
#' To help make your code safer, the 'tinycodet' R-package
#' introduces a few functions:
#'
#'  * \link[=%d==%]{Safer decimal (in)equality testing}.
#'  * \link[=as_int]{Atomic type casting without stripping attributes}.
#'  * The \link{lock_TF} function to
#'  set and lock \code{T} and \code{F} to \code{TRUE} and \code{FALSE},
#'  respectively.
#'  * The \link{%<-c%} operator to assign locked constants.
#'  * \link{form} to construct a formula with safer environment specification.
#'  * Standard evaluated versions of some common expression-evaluation functions: \cr
#'   \link{with_pro} and \link{aes_pro}.
#'  * \link{safer_partialmatch} to set options for
#'  safer dollar, arguments, and attribute matching. \cr \cr
#'
#'
#'
#'
#' @seealso \link{tinycodet_help}
#'
#' @examples
#' x <- c(0.3, 0.6, 0.7)
#' y <- c(0.1*3, 0.1*6, 0.1*7)
#' x == y # gives FALSE, but should be TRUE
#' x %d==% y # here it's done correctly
#'
#'
#'

#' @rdname aaa1_tinycodet_safer
#' @name aaa1_tinycodet_safer
#' @aliases tinycodet_safer
NULL
