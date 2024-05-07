#' Safer Decimal Number (In)Equality Testing Operators
#'
#' @description
#' The \code{%d==%, %d!=% %d<%, %d>%, %d<=%, %d>=%} (in)equality operators
#' perform decimal (type "double") number truth testing. \cr
#' They are virtually equivalent to the regular (in)equality operators, \cr
#' \code{==, !=, <, >, <=, >=}, \cr
#' except for 2 aspects:
#' 
#'  1) The decimal number (in)equality operators assume that
#'  if the absolute difference between any 2 numbers
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
#' 
#' Thus these operators provide safer decimal number (in)equality tests. \cr
#' \cr
#' There are also the \code{x %d{}% bnd} and \code{x %d!{}% bnd} operators,
#' where \code{bnd} is a vector of length 2,
#' or a 2-column matrix (\code{nrow(bnd)==length(x)} or \code{nrow(bnd)==1}). \cr
#' The \code{x %d{}% bnd} operator checks if \code{x}
#' is within the closed interval with bounds defined by \code{bnd}. \cr
#' The \code{x %d!{}% bnd} operator checks if \code{x}
#' is outside the closed interval with bounds defined by \code{bnd}. \cr
#' \cr
#' Moreover, the function \code{is_wholenumber()} is added, to safely test for whole numbers.
#'
#' @param x,y numeric vectors, matrices, or arrays.
#' @param bnd either a vector of length 2, or a matrix with 2 columns and 1 row,
#' or else a matrix with 2 columns where \code{nrow(bnd)==length(x)}
#' (or can be recycled to be \code{nrow(bnd)==length(x)}). \cr
#' The first element/column of \code{bnd} gives the lower bound of the closed interval; \cr
#' The second element/column of \code{bnd} gives the upper bound of the closed interval. \cr
#' @param tol a single, strictly positive number close to zero, giving the tolerance.
#'
#' @returns
#' A logical vector with the same dimensions as \code{x},
#' indicating the result of the element by element comparison.
#'
#' @seealso \link{tinycodet_safer}
#'
#' @examples
#' x <- c(0.3, 0.6, 0.7)
#' y <- c(0.1*3, 0.1*6, 0.1*7)
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
#' bnd <- cbind(x-0.1, x+0.1)
#' x %d{}% bnd
#' x %d!{}% bnd
#'
#' # These operators work for integers also:
#' x <- 1L:5L
#' y <- 1L:5L
#' x %d==% y
#' x %d!=% y
#' x %d<% y
#' x %d>% y
#' x %d<=% y
#' x %d>=% y
#'
#' x <- 1L:5L
#' y <- x+1
#' x %d==% y
#' x %d!=% y
#' x %d<% y
#' x %d>% y
#' x %d<=% y
#' x %d>=% y
#'
#' x <- 1L:5L
#' y <- x-1
#' x %d==% y
#' x %d!=% y
#' x %d<% y
#' x %d>% y
#' x %d<=% y
#' x %d>=% y
#' 
#' # is_wholenumber:
#' is_wholenumber(1:10 + c(0, 0.1))
#' 
#' 

#' @name decimal_truth
NULL

#' @rdname decimal_truth
#' @export
`%d==%` <- function(x, y) {
  
  return(abs(x - y) < sqrt(.Machine$double.eps))
}

#' @rdname decimal_truth
#' @export
`%d!=%` <- function(x, y) {
  
  return(abs(x - y) >= sqrt(.Machine$double.eps))
}

#' @rdname decimal_truth
#' @export
`%d<%` <- function(x, y) {
  return((y - x) >= sqrt(.Machine$double.eps))
}

#' @rdname decimal_truth
#' @export
`%d>%` <- function(x, y) {
  return((x - y) >= sqrt(.Machine$double.eps))
}

#' @rdname decimal_truth
#' @export
`%d<=%` <- function(x, y) {
  
  return((x <= y) | (x %d==% y))
}

#' @rdname decimal_truth
#' @export
`%d>=%` <- function(x, y) {
  
  return((x >= y) | (x %d==% y))
}

#' @rdname decimal_truth
#' @export
`%d{}%` <- function(x, bnd) {
  bnd <- .decimal_bnd(bnd, sys.call())
  return(x %d>=% bnd[,1] & x %d<=% bnd[,2])
}

#' @rdname decimal_truth
#' @export
`%d!{}%` <- function(x, bnd) {
  bnd <- .decimal_bnd(bnd, sys.call())
  return(x %d<% bnd[,1] | x %d>% bnd[,2])
}


#' @keywords internal
#' @noRd
.decimal_bnd <- function(bnd, abortcall) {
  if(!is.matrix(bnd)) {
    if(length(bnd) == 2L) {
      bnd <- matrix(bnd, ncol = 2L)
    } else {
      stop(simpleError(
        "`bnd` must be a matrix with 2 columns or vector with 2 elements",
        call = abortcall
      ))
    }
  }
  if(any(bnd[,2] < bnd[,1], na.rm = TRUE)) {
    stop(simpleError("`bnd[, 2] < bnd[, 1]`", call = abortcall))
  }
  return(bnd)
}


#' @rdname decimal_truth
#' @export
is_wholenumber <- function(x, tol = sqrt(.Machine$double.eps)) {
  if(!is.numeric(tol) || length(tol) != 1L) {
    stop("`tol` must be a single, strictly positive number close to 0")
  }
  if(tol >= 1 || tol <= 0) {
    stop("`tol` must be a single, strictly positive number close to 0")
  }
  return(abs(x - round(x)) < tol)
}
