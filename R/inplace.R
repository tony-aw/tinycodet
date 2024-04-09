#' General In-place Modifier Operator
#'
#' @description
#' The \code{x %:=% f} operator performs
#' in-place modification of some object \code{x}
#' with a function \code{f}. \cr
#' \cr
#' For example this:
#'
#' ```{r echo=TRUE, eval = FALSE}
#' mtcars$mpg[mtcars$cyl > 6] <- mtcars$mpg[mtcars$cyl>6]^2
#' ```
#'
#' Can now be re-written as:
#'
#' ```{r echo=TRUE, eval = FALSE}
#' mtcars$mpg[mtcars$cyl > 6] %:=% \(x) x^2
#' ```
#'
#' @param x a variable.
#' @param f a (possibly anonymous) function to be applied in-place on \code{x}.
#' The function must take one argument only.
#'
#' @returns
#' This operator does not return any value: \cr
#' It is an in-place modifier, and thus modifies the object directly.
#'
#' @seealso \link{tinycodet_dry}
#'
#'
#' @examples
#' set.seed(1)
#' object <- matrix(rpois(10, 10), ncol = 2)
#' print(object)
#' y <- 3
#' object %:=% \(x) x + y # same as object <- object + y
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

