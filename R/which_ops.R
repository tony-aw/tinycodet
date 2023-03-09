#' "Which"-Operators
#'
#'@description
#' These operators are designed to simplify operations where one
#' subsets an object based on conditions referring to itself.
#' For example: in \code{x\[x>0\]},
#' the object \code{x} is sub-setted based on conditions referring to itseld.
#' Thus, \code{x} is written twice.
#' If the object has a short name like \code{x},
#' these operators do not necessarily make your code tidier
#' (except for perhaps pattern matching).
#' But if \code{x} has a longer name like \code{very_long_name_1},
#' doing \code{very_long_name_1\[very_long_name_1>0\]} becomes cumbersome.
#' The \code{tidyoperators} package adds several "which"-operators,
#' which will simplify this. \cr
#' \cr
#' All which-operators are surrounded by
#' \code{%\[} and \code{\]%} \cr
#' \cr
#' The which operators are as follows: \cr
#' \cr
#' The \code{x %\[fun\]% fun} operator selects elements from vector/matrix \code{x},
#' for which the result of \code{fun(x)} returns \code{TRUE}. \cr
#' \cr
#' The \code{x %\[!fun\]% fun} operator selects elements from vector/matrix \code{x},
#' for which the result of \code{fun(x)} returns \code{FALSE}. \cr
#' \cr
#' The \code{s %\[sp\]% p} operator selects elements from character vector \code{s}
#' if they contain pattern \code{p}. \cr
#' \cr
#' The \code{s %\[!sp\]% p} operator selects elements from character vector \code{s}
#' if they do NOT contain pattern \code{p}. \cr
#' \cr
#'
#' @param x a vector or matrix,
#' or anything else that can be sub-setted with single square brackets ("[]").
#' @param s a character vector.
#' @param fun a function that returns a binary logic (\code{TRUE,FALSE}) vector
#' of the same length as \code{x}.
#' @param p the result from \link{s_pattern},
#' or else a character vector of the same length as \code{s} with regular expressions.
#'
#' @examples
#' object_with_very_long_name <- -5:5
#' object_with_very_long_name %[fun]% \(x)x>2
#' object_with_very_long_name %[!fun]% \(x)x>2
#' s <- c("hello world", "goodbye world")
#' p <- s_pattern(regex = rep("hello", 2))
#' s %[sp]% p
#'s %[!sp]% p


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

