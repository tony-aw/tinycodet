#' Row- or Column-wise Re-ordering of Matrices
#'
#' @description
#' Infix operators for custom row- and column-wise re-ordering of matrices. \cr
#' \cr
#' The \code{x %row~% mat} operator re-orders the elements of every row,
#' each row ordered independently from the other rows, of matrix \code{x},
#' according to the ordering ranks given in matrix \code{mat}. \cr
#' \cr
#' The \code{x %col~% mat} operator re-orders the elements of every column,
#' each column ordered independently from the other columns, of matrix \code{x},
#' according to the ordering ranks given in matrix \code{mat}. \cr
#' \cr
#' Note that these operators strip all attributes,
#' except dimensions. \cr
#' \cr
#' 
#'
#' @param x a matrix
#' @param mat a numeric matrix with the same dimensions as \code{x},
#' giving the new ordering ranks for every element of matrix \code{x}. \cr \cr
#' 
#' @returns
#' A re-ordered matrix.
#'
#'
#' @seealso \link{tinycodet_dry}
#' @example inst/examples/matrix_ops.R
#' 

#' @name matrix_ops
NULL

#' @rdname matrix_ops
#' @export
`%row~%` <- function(x, mat) {
  .matrix_ops_checks(x, mat, sys.call())
  out <- matrix(
    x[order(row(x), mat, decreasing = FALSE)],
    nrow=nrow(x),
    byrow = TRUE
  )
  return(out)
}


#' @rdname matrix_ops
#' @export
`%col~%` <- function(x, mat) {
  .matrix_ops_checks(x, mat, sys.call())
  out <- matrix(
    x[order(col(x), mat, decreasing = FALSE)],
    ncol=ncol(x),
    byrow = FALSE
  )
  return(out)
}


#' @keywords internal
#' @noRd
.matrix_ops_checks <- function(x, mat, abortcall) {
  if(!is.matrix(x) || !is.matrix(mat)) {
    stop(simpleError("both arguments must be matrices", call = abortcall))
  }
  if(.ndim(x) != .ndim(mat) || any(dim(x) != dim(mat))) {
    stop(simpleError("non-conformable matrices", call = abortcall))
  }
  if(!is.numeric(mat)) {
    stop(simpleError("right-hand side must be a numeric matrix of order rankings", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.ndim <- function(x) {
  length(dim(x))
}
