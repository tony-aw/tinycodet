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
#' according to the ordering ranks given in matrix \code{mat}. \cr \cr
#' 
#'
#' @param x a matrix
#' @param mat a matrix with the same dimensions as \code{x},
#' giving the ordering ranks of every element of matrix \code{x}. \cr
#'
#' @details
#' If matrix \code{x} is a numeric matrix,
#' and one wants to sort the elements of every row or column numerically,
#' \code{x %row~% x} or \code{x %col~% x} would suffice, respectively. \cr
#' \cr
#' If matrix \code{x} is not numeric,
#' sorting the elements using \code{x %row~% x} and \code{x %col~% x} is still possible,
#' but probably not the best option.
#' In the non-numeric case,
#' providing a matrix of ordering ranks for \code{mat} would be faster and give more accurate ordering.
#' See the examples section. \cr
#' \cr
#' If \code{mat} is a matrix of non-repeating random integers, i.e. \cr
#' \code{mat <- sample(seq_along(x)) |> matrix(ncol = ncol(x))}) \cr
#' then the code \cr
#' \code{x %row~% mat} \cr
#' will randomly shuffle the elements of every row of \code{x},
#' where the shuffling order in each row is independent from the shuffling order in the other rows. \cr
#' Similarly, \cr
#' \code{x %col~% mat} \cr
#' will randomly shuffle the elements of every column of \code{x},
#' where the shuffling order in each column is independent from the shuffling order in the other columns. \cr
#' \cr
#' Re-ordering/sorting every row/column of a matrix with these operators
#' is generally faster than doing so through loops or apply-like functions. \cr
#' \cr
#' Note that these operators strip all attributes except dimensions. \cr
#' \cr
#' 
#' @returns
#' A modified matrix.
#'
#'
#' @seealso \link{tinycodet_misc}
#'
#' @example inst/examples/matrix_ops.R
#' 

#' @name matrix_ops
NULL

#' @rdname matrix_ops
#' @export
`%row~%` <- function(x, mat) {
  x <- matrix(
    x[order(row(x), mat, decreasing = FALSE)],
    nrow=nrow(x),
    byrow = TRUE
  )
  return(x)
}

#' @rdname matrix_ops
#' @export
`%col~%` <- function(x, mat) {
  x <- matrix(
    x[order(col(x), mat, decreasing = FALSE)],
    ncol=ncol(x),
    byrow = FALSE
  )
  return(x)
}

