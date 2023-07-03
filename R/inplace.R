#' Generalized in-place (mathematical) modifier
#'
#' @description
#' Generalized in-place (mathematical) modifier. \cr
#' \cr
#' The \code{x %:=% f} operator allows performing
#' in-place modification of some object \code{x}
#' with a function \code{f}. \cr
#' \cr
#' For example this: \cr
#' \code{object\[object > 0\] %:=% \(x) x + 1} \cr
#' Is the same as: \cr
#' \code{object[object > 0] <- object[object > 0] + 1} \cr
#' \cr
#' This function-based method is used instead of the more traditional
#' in-place mathematical modification like
#' \code{+=} to prevent precedence issues
#' (functions come before mathematical arithmetic in \code{R}).
#'
#' @param x an object, with properties such that function \code{f} can be use on it. \cr
#' For example, when function \code{f} is mathematical,
#' \code{x} should be a number or numeric (or 'number-like') vector, matrix, or array.
#' @param f a function to be applied in-place on \code{x}.
#'
#' @return
#' This operator does not return any value: \cr
#' it is an in-place modifiers, and thus modifies \code{x} directly.
#'
#' @examples
#' set.seed(1)
#' object <- matrix(rpois(10, 10), ncol=2)
#' print(object)
#' object %:=% \(x) x+3 # same as object <- object + 3
#' print(object)
#'

#' @name inplace
NULL

#' @rdname inplace
#' @export
`%:=%` <- function(x, f) {
  eval(call("<-", substitute(x), f(x)), envir = parent.frame(n = 1))
}

