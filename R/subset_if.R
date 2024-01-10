#' Conditional Sub-setting and In-place Replacement of Unreal Values
#'
#' @description
#'
#' The \code{x %\[if\]% cond} operator
#' selects elements from vector/matrix/array \code{x},
#' for which the result of \code{cond(x)} returns \code{TRUE}. \cr
#' And the \code{x %\[!if\]% cond} operator
#' selects elements from vector/matrix/array \code{x},
#' for which the result of \code{cond(x)} returns \code{FALSE}. \cr
#' \cr
#' The \code{x %unreal =% repl} operator
#' modifies all unreal (\code{NA, NaN, Inf, -Inf}) values of \code{x}
#' with replacement value \code{repl}. \cr
#' Thus, \cr
#' \code{x %unreal =% repl}, \cr
#' is the same as, \cr
#' \code{x[is.na(x) | is.nan(x) | is.infinite(x)] <- repl} \cr
#'
#' @param x a vector, matrix, or array.
#' @param cond a (possibly anonymous) function that returns a \code{logical} vector
#' of the same length/dimensions as \code{x}. \cr
#' For example: \code{\(x)x>0}. \cr
#' @param repl the replacement value.
#'
#'
#'
#' @returns
#' For the \code{x %\[if\]% cond} and \code{x %\[!if\]% cond} operators: \cr
#' The subset_if - operators all return a vector with the selected elements. \cr
#' \cr
#' For the \code{x %unreal =% repl} operator: \cr
#' The \code{x %unreal =% repl} operator does not return any value: \cr
#' It is an in-place modifier, and thus modifies \code{x} directly.
#' The object \code{x} is modified such that all
#' \code{NA}, \code{NaN}, \code{Inf}, and \code{-Inf} elements are replaced with \code{repl}.
#'
#'
#' @seealso \link{tinycodet_dry}
#'
#' @examples
#' x <- c(-10:9, NA, NA)
#' object_with_very_long_name <- matrix(x, ncol=2)
#' print(object_with_very_long_name)
#' object_with_very_long_name %[if]% \(x)x %in% 1:10
#' object_with_very_long_name %[!if]% \(x)x %in% 1:10
#'
#' x <- c(1:9, NA, NaN, Inf)
#' print(x)
#' x %unreal =% 0 # same as x[is.na(x)|is.nan(x)|is.infinite(x)] <- 0
#' print(x)


#' @name subset_if
NULL

#' @rdname subset_if
#' @export
`%[if]%` <- function(x, cond) {
  indx <- cond(x)
  if(!is.logical(indx)) {
    stop("`cond` must return a logical vector")
  }
  return(x[which(indx)])
}

#' @rdname subset_if
#' @export
`%[!if]%` <- function(x, cond) {
  indx <- cond(x)
  if(!is.logical(indx)) {
    stop("`cond` must return a logical vector")
  }
  return(x[which(!indx)])
}

#' @rdname subset_if
#' @export
`%unreal =%` <- function(x, repl) {
  y <- x
  y[is.na(y)|is.nan(y)|is.infinite(y)] <- repl
  temp_name <- substitute(x)

  eval(call("<-", temp_name, y), envir = parent.frame(n = 1))
}
