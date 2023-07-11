#' Infix operators for row- and column-wise re-ordering of matrices
#'
#'@description
#' Infix operators for custom row- and column-wise re-ordering of matrices \cr
#' \cr
#' The \code{x %row~% mat} operator re-orders the elements of every row of matrix \code{x}
#' according to the ordering ranks given in matrix \code{mat}. \cr
#' \cr
#' The \code{x %col~% mat} operator re-orders the elements of every column of matrix \code{x}
#' according to the ordering ranks given in matrix \code{mat}. \cr
#' \cr
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
#' \code{mat <- sample(1:length(x), replace=FALSE) |> matrix(ncol=ncol(x))}) \cr
#' then the code \cr
#' \code{x %row~% mat} \cr
#' will randomly shuffle the elements of every row,
#' where the shuffling order of every row is independent of the other rows. \cr
#' Similarly, \cr
#' \code{x %col~% mat} \cr
#' will randomly shuffle the elements of every column,
#' where the shuffling order of every column is independent of the other columns. \cr
#' \cr
#' These operators are faster than re-ordering matrices using loops or apply-like functions.
#'
#' @returns
#' A modified matrix.
#'
#'
#' @examples
#'
#'
#' # numeric matrix ====
#'
#' x <- matrix(sample(1:25), nrow=5)
#' print(x)
#' x %row~% x # sort elements of every row
#' x %row~% -x # reverse-sort elements of every row
#' x %col~% x # sort elements of every column
#' x %col~% -x # reverse-sort elements of every column
#'
#' x <- matrix(sample(1:25), nrow=5)
#' print(x)
#' mat <- sample(1:length(x)) |> matrix(ncol=ncol(x)) # matrix of non-repeating random integers
#' x %row~% mat # randomly shuffle every row independently
#' x %col~% mat # randomize shuffle every column independently
#'
#' # character matrix ====
#'
#' x <- matrix(sample(letters, 25), nrow=5)
#' print(x)
#' mat <- stringi::stri_rank(as.vector(x)) |> matrix(ncol=ncol(x))
#' x %row~% mat # sort elements of every row
#' x %row~% -mat # reverse-sort elements of every row
#' x %col~% mat # sort elements of every column
#' x %col~% -mat # reverse-sort elements of every column
#'
#' x <- matrix(sample(letters, 25), nrow=5)
#' print(x)
#' mat <- sample(1:length(x)) |> matrix(ncol=ncol(x)) # matrix of non-repeating random integers
#' x %row~% mat # randomly shuffle every row independently
#' x %col~% mat # randomize shuffle every column independently
#'
#'
#'

#' @name matrix_ops
NULL

#' @rdname matrix_ops
#' @export
`%row~%` <- function(x, mat) {
  x <- matrix(
    x[order(row(x), mat, decreasing = FALSE)],
    nrow=nrow(x),
    byrow=TRUE
  )
  return(x)
}

#' @rdname matrix_ops
#' @export
`%col~%` <- function(x, mat) {
  x <- matrix(
    x[order(col(x), mat, decreasing = FALSE)],
    ncol=ncol(x),
    byrow=FALSE
  )
  return(x)
}

