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
#' \code{mtcars$mpg[mtcars$cyl>6] <- mtcars$mpg[mtcars$cyl>6]^2} \cr
#' Can now be re-written as: \cr
#' \code{mtcars$mpg\[mtcars$cyl>6\] %:=% \(x)x^2} \cr
#' \cr
#'
#' @param x an object, with properties such that function \code{f} can be used on it. \cr
#' For example, when function \code{f} is mathematical,
#' \code{x} should be a numeric (or 'number-like') vector, matrix, or array.
#' @param f a (possibly anonymous) function to be applied in-place on \code{x}.
#' The function must take one argument only.
#'
#' @return
#' This operator does not return any value: \cr
#' It is an in-place modifier, and thus modifies the object directly.
#'
#' @seealso [tinyoperations_dry()]
#'
#'
#' @examples
#' set.seed(1)
#' object <- matrix(rpois(10, 10), ncol=2)
#' print(object)
#' y <- 3
#' object %:=% \(x) x+y # same as object <- object + y
#' print(object)
#'

#' @name inplace
NULL

#' @rdname inplace
#' @export
`%:=%` <- function(x, f) {
  if(!is.function(f)) {
    stop("right hand side must be a function")
  }
  eval(call("<-", substitute(x), f(x)), envir = parent.frame(n = 1))
}

