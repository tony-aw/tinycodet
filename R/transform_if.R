#' The transform_if function and the subset_if operators
#'
#'@description
#' Consider the following code: \cr
#' \code{ifelse(cond(x), f(x), g(x))} \cr
#' Here a conditional subset of the object \code{x} is transformed,
#' where the condition is using a function referring to \code{x} itself.
#' Consequently, reference to \code{x} is written four times! \cr
#' The \code{tinyoperators} package therefore adds the
#' \code{transform_if()} function
#' which will tiny this up. \cr
#' \cr
#' The \code{tinyoperators} package also adds 2 "subset_if" operators: \cr
#' The \code{x %\[if\]% cond} operator
#' selects elements from vector/matrix/array \code{x},
#' for which the result of \code{cond(x)} returns \code{TRUE}. \cr
#' The \code{x %\[!if\]% cond} operator
#' selects elements from vector/matrix/array \code{x},
#' for which the result of \code{cond(x)} returns \code{FALSE}. \cr
#' \cr
#' The \code{tinyoperators} package also adds the \code{x %unreal =% repl} operator: \cr
#' \code{x %unreal =% repl} is the same as
#' \code{x[is.na(x)|is.nan(x)|is.infinite(x)] <- repl} \cr
#'
#' @param x a vector, matrix, or array.
#' @param cond a function that returns a binary logic (\code{TRUE,FALSE}) vector
#' of the same length/dimensions as \code{x} (for example: \code{is.na}).
#' @param trans_T the transformation function to use when \code{cond(x)==TRUE}. \cr
#' For example: \code{log}. \cr
#' If this is not specified, \code{trans_T} defaults to \code{function(x)x}.
#' @param trans_F the transformation function to use when \code{cond(x)==FALSE}. \cr
#' For example: \code{log}. \cr
#' If this is not specified, \code{trans_F} defaults to \code{function(x)x}.
#' @param repl the replacement value.
#'
#' @details
#' The \code{transform_if(x, cond, trans)} function
#' does not rely on any explicit or implicit loops, nor any third-party functions.
#'
#' @returns
#' For \code{transform_if()}: \cr
#' Similar to \link[base]{ifelse}.
#' However, unlike \code{ifelse()}, the transformations are evaluated as
#' \code{trans_T(x[cond(x)])} and \code{trans_F(x[!cond(x)])},
#' ensuring no unnecessary warnings or errors occur. \cr
#' \cr
#' The subset_if - operators all return a vector with the selected elements. \cr
#' \cr
#' The \code{x %unreal =% repl} operator does not return any value: \cr
#' It is an in-place modifiers, and thus modifies \code{x} directly.
#' The object \code{x} is modified such that all
#' \code{NA}, \code{NaN} and \code{Inf} elements are replaced with \code{repl}.
#'
#'
#' @examples
#' object_with_very_long_name <- matrix(-10:9, ncol=2)
#' print(object_with_very_long_name)
#' object_with_very_long_name |> transform_if(\(x)x>0, log)
#' object_with_very_long_name |> transform_if(\(x)x>0, log, \(x)x^2)
#' object_with_very_long_name %[if]% \(x)x %in% 1:10
#' object_with_very_long_name %[!if]% \(x)x %in% 1:10
#'
#' x <- c(1:9, NA, NaN, Inf)
#' print(x)
#' x %unreal =% 0 # same as x[is.na(x)|is.nan(x)|is.infinite(x)] <- 0
#' print(x)



#' @rdname transform_if
#' @export
transform_if <- function(x, cond, trans_T=function(x)x, trans_F=function(x)x) {
  check <- all(c(
    isTRUE(is.function(cond)), isTRUE(is.function(trans_T)), isTRUE(is.function(trans_F))
  ))
  if(!check) {
    stop("`cond`, `trans_T`, and `trans_F` must all be functions")
  }
  y <- x
  y[cond(y)] <- trans_T(y[cond(y)])
  y[!cond(y)] <- trans_F(y[!cond(y)])
  return(y)
}

#' @rdname transform_if
#' @export
`%[if]%` <- function(x, cond) {
  indx <- as.logical(cond(x))
  if(!is.logical(indx)){
    stop("cond must return either TRUE or FALSE")
  }
  return(x[indx])
}

#' @rdname transform_if
#' @export
`%[!if]%` <- function(x, cond) {
  indx <- as.logical(cond(x))
  if(!is.logical(indx)){
    stop("cond must return either TRUE or FALSE")
  }
  return(x[!indx])
}


#' @rdname transform_if
#' @export
`%unreal =%` <- function(x, repl) {
  y <- x
  y[is.na(y)|is.nan(y)|is.infinite(y)] <- repl
  temp_name <- substitute(x)

  eval(call("<-", temp_name, y), envir = parent.frame(n = 1))
}
