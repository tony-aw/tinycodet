#' In-place unreal replacers
#'
#' @description
#' In-place modifiers to replace unreal (NA, NaN, Inf, -Inf) elements. \cr
#' Works on vectors, matrices, and arrays. \cr
#' The following \cr
#' \code{x %unreal <-% 0} \cr
#' is the same as \cr
#' \code{x[is.na(x)|is.nan(x)|is.infinite(x)] <- 0} \cr
#'
#'
#' @param x a vector or matrix whose unreal values are to be replaced.
#' @param replacement the replacement value.
#'
#' @returns
#' This operator does not return any value:
#' it is an in-place modifiers, and thus modifies x directly.
#' The x vector is modified such that all NA, NaN and infinities are replaced with the given replacement value.
#'
#' @examples
#' x <- c(1:9, NA, NaN, Inf)
#' print(x)
#' x %unreal <-% 0 # same as x[is.na(x)|is.nan(x)|is.infinite(x)] <- 0
#' print(x)
#'

#' @name inplace_unreal
NULL

#' @rdname inplace_unreal
#' @export
`%unreal <-%` <- function(x, replacement) {
  y <- x
  y[is.na(y)|is.nan(y)|is.infinite(y)] <- replacement
  temp_name <- substitute(x)

  eval(call("<-", temp_name, y), envir = parent.frame(n = 1))
}
