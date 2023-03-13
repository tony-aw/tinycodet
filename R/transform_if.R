#' The transform_if function and the subset_if operators
#'
#'@description
#' Consider the following code: \cr
#' \code{x[cond(x)] <- f(x[cond(x)])} \cr
#' Here a conditional subset of the object \code{x} is transformed with function \code{f},
#' where the condition is using a function referring to \code{x} itself (namely \code{cond(x)}).
#' Consequently, reference to \code{x} is written four times! \cr
#' If the object has a short name like "x", this doesn't matter too much.
#' But if the object has a longer name like "very_long_name_1",
#' doing something like this: \cr
#' \code{very_long_name_1[very_long_name_1 > 0] <- log(very_long_name_1[very_long_name_1 > 0])} \cr
#' becomes cumbersome, and not so tidy. \cr
#' The \code{tidyoperators} package therefore adds the
#' \code{transform_if()} function
#' which will tidy this up. \cr
#' \cr
#' The code \cr
#' \code{x <- transform(x, cond, trans)} \cr
#' is exactly equivalent to \cr
#' \code{x[cond(x)] <- trans(x[cond(x)])}
#'
#' Besides \code{transform_if},
#' the \code{tidyoperators} package also adds 2 "subset_if" operators: \cr
#' \cr
#' The \code{x %\[if\]% cond} operator
#' selects elements from vector/matrix/array \code{x},
#' for which the result of \code{cond(x)} returns \code{TRUE}. \cr
#' \cr
#' The \code{x %\[!if\]% cond} operator
#' selects elements from vector/matrix/array \code{x},
#' for which the result of \code{cond(x)} returns \code{FALSE}. \cr
#' \cr
#'
#' @param x a vector, matrix, array,
#' or anything else that can be sub-setted with single square brackets ("[]").
#' @param cond a function that returns a binary logic (\code{TRUE,FALSE}) vector
#' of the same length/dimensions as \code{x} (for example: \code{is.na}). \cr
#'  * Elements of \code{x} for which \code{cond(x)==TRUE} are transformed / selected; \cr
#'  * Elements of \code{x} for which \code{cond(x)==FALSE} are not transformed /selected. \cr
#' @param trans the transformation function to use. For example: \code{log}.
#'
#' @details
#' The \code{transform_if(x, cond, trans)} function
#' does not rely on any explicit or implicit loops, nor any third-party functions.
#'
#' @returns
#' The \code{transform_if()} function
#' returns the same object \code{x}, with the same dimensions,
#' except with the subset transformed. \cr
#' Note that this function **returns** object \code{x},
#' to modify \code{x} directly, one still has to assign it.
#' To keep your code tidy, consider combining this function with
#' \code{magrittr}'s in-place modifying piper-operator (\code{%<>%}).
#' I.e.: \cr
#' \code{very_long_name_1 %<>% transform_if(cond, trans)} \cr
#' \cr
#' The subset_if - operators all return a vector with the selected elements. \cr
#' \cr
#'
#'
#' @examples
#' object_with_very_long_name <- matrix(-10:9, ncol=2)
#' print(object_with_very_long_name)
#' object_with_very_long_name |> transform_if(\(x)x>0, log)
#' object_with_very_long_name %[if]% \(x)x %in% 1:10
#' object_with_very_long_name %[!if]% \(x)x %in% 1:10



#' @rdname transform_if
#' @export
transform_if <- function(x, cond, trans=NULL) {
  y <- x
  indx <- cond(y)
  if(is.null(trans)){
    return(y[indx])
  }
  if(!is.null(trans)){
    y[indx] <- trans(y[indx])
  }
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
