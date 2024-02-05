#' Safer Decimal Number (In)Equality Testing Operators
#'
#' @description
#' The \code{%d==%, %d!=% %d<%, %d>%, %d<=%, %d>=%} (in)equality operators
#' perform decimal (class "double") number truth testing. \cr
#' They are virtually equivalent to the regular (in)equality operators, \cr
#' \code{==, !=, <, >, <=, >=}, \cr
#' except for 4 aspects:
#' 
#'  1) The \code{%d...%} operators assume that
#'  if the absolute difference between any two numbers
#'  \code{x} and \code{y}
#'  is smaller than the Machine tolerance,
#'  \code{sqrt(.Machine$double.eps)},
#'  then \code{x} and \code{y}
#'  should be consider to be equal. \cr
#'  For example: \code{0.1*7 == 0.7} returns \code{FALSE}, even though they are equal,
#'  due to the way decimal numbers are stored in programming languages like 'R' and  'Python'. \cr
#'  But \code{0.1*7 %d==% 0.7} returns \code{TRUE}.
#'  2) Only numeric input is allowed, so characters are not coerced to numbers. \cr
#'  I.e. \code{1 < "a"} gives \code{TRUE}, whereas \code{1 %d<% "a"} gives an error. \cr
#'  For character equality testing, see \link[stringi]{%s==%} from the 'stringi' package.
#'  3) These operators do not support vector re-cycling,
#'  except for simple scalar recycling.
#'  4) The \code{%d...%} operators use a much simpler implementation
#'  and always return a simple, attribute-free, logical vector. \cr
#'  Only dimensions are preserved, when it makes sense. \cr
#' 
#' Thus these operators provide safer decimal number (in)equality tests. \cr
#' \cr
#' There are also the \code{x %d{}% bnd} and \code{x %d!{}% bnd} operators,
#' where \code{bnd} is a vector of length 2 or a 2-column matrix
#' (\code{nrow(bnd)==length(x)} or \code{nrow(bnd)==1}). \cr
#' The \code{x %d{}% bnd} operator checks if \code{x}
#' is within the closed interval with bounds defined by \code{bnd}. \cr
#' The \code{x %d!{}% bnd} operator checks if \code{x}
#' is outside the closed interval with bounds defined by \code{bnd}. \cr
#'
#' @param x,y numeric vectors, matrices, or arrays.
#' @param bnd a vector of length 2, a matrix with 2 columns and 1 row,
#' or a matrix with 2 columns where \code{nrow(bnd)==length(x)}. \cr
#' The first element/column of \code{bnd} gives the lower bound of the closed interval; \cr
#' The second element/column of \code{bnd} gives the upper bound of the closed interval. \cr
#' NOTE: The lower bound \bold{must} be lower than the upper bound,
#' otherwise an error is produced. \cr
#' 
#' 
#' 
#' @returns
#' The decimal truth operators all return a logical vector,
#' indicating the result of the element by element comparison. \cr
#' \cr
#' For the \code{%d{}%} and \code{%d!{}%} operators,
#' the produced logical vector has the same dimensions as \code{x}.\cr
#' \cr
#' For the other decimal truth operators, the following holds: \cr
#' If both \code{x} and \code{y} have the same dimensions,
#' the produced logical vector shares the same dimensions. \cr
#' Otherwise, dimensions are dropped. \cr
#' 
#' 
#'
#' @seealso \link{tinycodet_safer}
#'
#' @examples
#' x <- c(0.3, 0.6, 0.7)
#' y <- c(0.1 * 3, 0.1 * 6, 0.1 * 7)
#' print(x); print(y)
#' x == y # gives FALSE, but should be TRUE
#' x!= y # gives TRUE, should be FALSE
#' x > y # not wrong
#' x < y # gives TRUE, should be FALSE
#' x %d==% y # here it's done correctly
#' x %d!=% y # correct
#' x %d<% y # correct
#' x %d>% y # correct
#' x %d<=% y # correct
#' x %d>=% y # correct
#'
#' x <- c(0.3, 0.6, 0.7)
#' eps <- sqrt(.Machine$double.eps) * 2
#' bnd <- cbind(x - c(eps, -eps, eps), x + c(eps, eps, eps))
#' x %d{}% bnd
#' x %d!{}% bnd
#'
#' # These operators work for integers also:
#' x <- 1L:5L
#' y <- x + c(0L, 1L, -1L, 1L, -1L)
#' x %d==% y
#' x %d!=% y
#' x %d<% y
#' x %d>% y
#' x %d<=% y
#' x %d>=% y
#'
#'

#' @name decimal_truth
NULL

#' @rdname decimal_truth
#' @export
`%d==%` <- function(x, y) {
  out <- .decimal_equal(x, y, sqrt(.Machine$double.eps), TRUE, sys.call())
  out <- .decimal_setattr(out, x, y)
  return(out)
}

#' @rdname decimal_truth
#' @export
`%d!=%` <- function(x, y) {
  out <- .decimal_equal(x, y, sqrt(.Machine$double.eps), FALSE, sys.call())
  out <- .decimal_setattr(out, x, y)
  return(out)
}

#' @rdname decimal_truth
#' @export
`%d<%` <- function(x, y) {

  out <- .decimal_smaller(x, y, sqrt(.Machine$double.eps), FALSE, sys.call())
  out <- .decimal_setattr(out, x, y)
  return(out)
  
}

#' @rdname decimal_truth
#' @export
`%d>%` <- function(x, y) {

  out <- .decimal_greater(x, y, sqrt(.Machine$double.eps), FALSE, sys.call())
  out <- .decimal_setattr(out, x, y)
  return(out)
}

#' @rdname decimal_truth
#' @export
`%d<=%` <- function(x, y) {

  out <- .decimal_smaller(x, y, sqrt(.Machine$double.eps), TRUE, sys.call())
  out <- .decimal_setattr(out, x, y)
  return(out)
}

#' @rdname decimal_truth
#' @export
`%d>=%` <- function(x, y) {

  out <- .decimal_greater(x, y, sqrt(.Machine$double.eps), TRUE, sys.call())
  out <- .decimal_setattr(out, x, y)
  return(out)
}

#' @rdname decimal_truth
#' @export
`%d{}%` <- function(x, bnd) {
  out <- .decimal_between(x, bnd, sqrt(.Machine$double.eps), sys.call())
  dim(out) <- dim(x)
  return(out)
}

#' @rdname decimal_truth
#' @export
`%d!{}%` <- function(x, bnd) {
  out <- !.decimal_between(x, bnd, sqrt(.Machine$double.eps), sys.call())
  dim(out) <- dim(x)
  return(out)
}


#' @keywords internal
#' @noRd
.decimal_equal <- function(x, y, tol, equal, abortcall) {
  if(!is.numeric(x) || !is.numeric(y)) {
    stop(simpleError("both sides must be numeric", call = abortcall))
  }
  lenx <- length(x)
  leny <- length(y)
  is_int <- is.integer(x) && is.integer(y)
  if(lenx == leny) {
    if(is_int) return(.rcpp_ntt_eq_int_00(x, y, equal))
    return(.rcpp_ntt_eq_dbl_00(x, y, tol, equal))
  }
  else if(lenx == 1) {
    if(is_int) return(.rcpp_ntt_eq_int_10(x, y, equal))
    return(.rcpp_ntt_eq_dbl_10(x, y, tol, equal))
  }
  else if(leny == 1) {
    if(is_int) return(.rcpp_ntt_eq_int_01(x, y, equal))
    return(.rcpp_ntt_eq_dbl_01(x, y, tol, equal))
  }
  else stop(simpleError("vector recycling not supported", call = abortcall))
}


#' @keywords internal
#' @noRd
.decimal_greater <- function(x, y, tol, equal, abortcall) {
  if(!is.numeric(x) || !is.numeric(y)) {
    stop(simpleError("both sides must be numeric", call = abortcall))
  }
  lenx <- length(x)
  leny <- length(y)
  is_int <- is.integer(x) && is.integer(y)
  if(lenx == leny) {
    if(is_int) return(.rcpp_ntt_greater_int_00(x, y, equal))
    return(.rcpp_ntt_greater_dbl_00(x, y, tol, equal))
  }
  else if(lenx == 1) {
    if(is_int) return(.rcpp_ntt_greater_int_10(x, y, equal))
    return(.rcpp_ntt_greater_dbl_10(x, y, tol, equal))
  }
  else if(leny == 1) {
    if(is_int) return(.rcpp_ntt_greater_int_01(x, y, equal))
    return(.rcpp_ntt_greater_dbl_01(x, y, tol, equal))
  }
  else stop(simpleError("vector recycling not supported", call = abortcall))
}


#' @keywords internal
#' @noRd
.decimal_smaller <- function(x, y, tol, equal, abortcall) {
  if(!is.numeric(x) || !is.numeric(y)) {
    stop(simpleError("both sides must be numeric", call = abortcall))
  }
  lenx <- length(x)
  leny <- length(y)
  is_int <- is.integer(x) && is.integer(y)
  if(lenx == leny) {
    if(is_int) return(.rcpp_ntt_smaller_int_00(x, y, equal))
    return(.rcpp_ntt_smaller_dbl_00(x, y, tol, equal))
  }
  else if(lenx == 1) {
    if(is_int) return(.rcpp_ntt_smaller_int_10(x, y, equal))
    return(.rcpp_ntt_smaller_dbl_10(x, y, tol, equal))
  }
  else if(leny == 1) {
    if(is_int) return(.rcpp_ntt_smaller_int_01(x, y, equal))
    return(.rcpp_ntt_smaller_dbl_01(x, y, tol, equal))
  }
  else stop(simpleError("vector recycling not supported", call = abortcall))
}


#' @keywords internal
#' @noRd
.decimal_between <- function(x, bnd, tol, abortcall) {
  if(!is.numeric(x) || !is.numeric(bnd)) {
    stop(simpleError("both sides must be numeric", call = abortcall))
  }
  if(!is.matrix(bnd)) {
    if(length(bnd) == 2) {
      bnd <- matrix(bnd, ncol = 2)
    } else {
      stop(simpleError(
        "`bnd` must be a matrix with 2 columns or vector with 2 elements", call = abortcall
      ))
    }
  }
  lenx <- length(x)
  lenbnd <- nrow(bnd)
  is_int <- is.integer(x) && is.integer(bnd)
  if(lenx == lenbnd) {
    if(is_int) return(.rcpp_ntt_between_int_00(x, bnd[,1], bnd[,2]))
    return(.rcpp_ntt_between_dbl_00(x, bnd[,1], bnd[,2], sqrt(.Machine$double.eps)))
  }
  else if(lenx == 1) {
    if(is_int) return(.rcpp_ntt_between_int_10(x, bnd[,1], bnd[,2]))
    return(.rcpp_ntt_between_dbl_10(x, bnd[,1], bnd[,2], sqrt(.Machine$double.eps)))
  }
  else if(lenbnd == 1) {
    if(is_int) return(.rcpp_ntt_between_int_01(x, bnd[1], bnd[2]))
    return(.rcpp_ntt_between_dbl_01(x, bnd[1], bnd[2], sqrt(.Machine$double.eps)))
  }
  else stop(simpleError("vector recycling not supported", call = abortcall))
}


#' @keywords internal
#' @noRd
.decimal_setattr <- function(out, x, y) {
  
  # first null, as null comparisons don't always work properly
  if(is.null(dim(x)) || is.null(dim(y))) {
    return(out)
  }
  
  if(all(dim(x) == dim(y))) {
    dim(out) <- dim(x)
    return(out)
  }
  
  # if dim(x) and dim(y) are different, no attribute retention:
  return(out)
  
}

