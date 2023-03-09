#' "Which"-Operators
#'
#'@description
#' These operators are designed to simplify operations where one
#' subsets an object based on conditions referring to itself.
#' Consider for example the following code: \cr
#' \code{x[x>0]} \cr
#' here the object \code{x} is sub-setted based on conditions referring to itself.
#' Thus, \code{x} is written twice.
#' If the object has a short name like \code{x},
#' these operators do not necessarily make your code tidier
#' (except for perhaps pattern matching).
#' But if \code{x} has a longer name like \code{very_long_name_1},
#' doing something like this: \cr
#' \code{very_long_name_1[very_long_name_1 > 0]} \cr
#' becomes cumbersome, and not so tidy.
#' The \code{tidyoperators} package adds several "which"-operators,
#' which will tidy this up. \cr
#' \cr
#' All which-operators are surrounded by
#' \code{%\[} and \code{\]%} \cr
#' \cr
#' The which operators are as follows: \cr
#' \cr
#' The \code{x %\[fun\]% fun} operator
#' selects elements from vector/matrix/array \code{x},
#' for which the result of \code{fun(x)} returns \code{TRUE}. \cr
#' \cr
#' The \code{x %\[!fun\]% fun} operator
#' selects elements from ector/matrix/array \code{x},
#' for which the result of \code{fun(x)} returns \code{FALSE}. \cr
#' \cr
#' The \code{s %\[sp\]% p} operator
#' selects elements from character vector \code{s}
#' if they contain pattern \code{p}. \cr
#' \cr
#' The \code{s %\[!sp\]% p} operator
#' selects elements from character vector \code{s}
#' if they do NOT contain pattern \code{p}. \cr
#' \cr
#' The \code{n %\[intv\]% ss} operator
#' selects elements from numeric vector/matrix/array \code{n},
#' whose values are within the interval defined by \code{ss}. \cr
#' \cr
#' The \code{n %\[!intv\]% ss} operator
#' selects elements from numeric vector/matrix/array \code{n},
#' whose values are outside the interval defined by \code{ss}. \cr
#' \cr
#'
#' @param x a vector, matrix, array,
#' or anything else that can be sub-setted with single square brackets ("[]").
#' @param s a character vector.
#' @param n a numeric vector, matrix, or array.
#' @param fun a function that returns a binary logic (\code{TRUE,FALSE}) vector
#' of the same length as \code{x}.
#' @param p the result from \link{s_pattern},
#' or else a character vector of the same length as \code{s} with regular expressions.
#' @param ss a vector of length 2, or a matrix with \code{nrow(ss)==length(s)}
#' and 2 columns.
#' The first element/column of \code{ss} gives the lower bound of the interval,
#' and the second element/column of \code{ss} gives the upper bound.
#'
#' @details
#' The \code{x %\[intv\]% ss} operator essentially performs the command: \cr
#' \code{x[x >= ss[, 1] & x <= ss[, 2]] } \cr
#' and \code{x %\[!intv\]% ss} performs the opposite: \cr
#' \code{x[[!(x >= ss[, 1] & x <= ss[, 2])]} \cr
#'
#' @returns
#' A vector with the selected elements.
#'
#' @examples
#' object_with_very_long_name <- -5:5
#' object_with_very_long_name %[fun]% \(x)x %in% c(1, 3, 5, 7)
#' object_with_very_long_name %[!fun]% \(x)x %in% c(1, 3, 5, 7)
#' n <- matrix(-5:4, ncol=2)
#' ss <- cbind(rep(-2, length(n)), rep(2, length(n)))
#' n %[intv]% ss
#' n %[!intv]% ss
#' s <- c("hello world", "goodbye world")
#' p <- s_pattern(regex = rep("hello", 2))
#' s %[sp]% p
#' s %[!sp]% p

#' @name which_ops
NULL

#' @rdname which_ops
#' @export
`%[fun]%` <- function(x, fun) {
  indx <- as.logical(fun(x))
  if(!is.logical(indx)){
    stop("fun must return either TRUE or FALSE")
  }
  return(x[indx])
}

#' @rdname which_ops
#' @export
`%[!fun]%` <- function(x, fun) {
  indx <- as.logical(fun(x))
  if(!is.logical(indx)){
    stop("fun must return either TRUE or FALSE")
  }
  return(x[!indx])
}

#' @rdname which_ops
#' @export
`%[intv]%` <- function(n, ss) {
  ss <- matrix(ss, ncol=2)
  indx <- n >= ss[, 1] & n <= ss[, 2]
  return(n[indx])
}

#' @rdname which_ops
#' @export
`%[!intv]%` <- function(n, ss) {
  ss <- matrix(ss, ncol=2)
  indx <- n >= ss[, 1] & n <= ss[, 2]
  return(n[!indx])
}

#' @rdname which_ops
#' @export
`%[sp]%` <- function(s, p) {
  if(isTRUE(attr(p, "engine")=="stringi")){
    indx <- do.call(stringi::stri_detect, c(list(str=s), p))
  }
  else{
    indx <- stringi::stri_detect(s, regex=p)
  }
  return(s[indx])
}

#' @rdname which_ops
#' @export
`%[!sp]%` <- function(s, p) {
  if(isTRUE(attr(p, "engine")=="stringi")){
    indx <- do.call(stringi::stri_detect, c(list(str=s), p))
  }
  else{
    indx <- stringi::stri_detect(s, regex=p)
  }
  return(s[!indx])
}
