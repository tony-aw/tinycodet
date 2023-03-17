#' Matrix custom sorting operators
#'
#'@description
#' Infix operators for custom row- and column-wise sorting of matrices \cr
#' \cr
#' The \code{x %r~% rank} operator sorts the elements of every row of matrix \code{x}
#' by the rank given in matrix \code{rank}. \cr
#' \cr
#' The \code{x %c~% rank} operator sorts the elements of every column of matrix \code{x}
#' by the rank given in matrix \code{rank}. \cr
#' \cr
#'
#' @param x a matrix
#' @param rank a matrix with the same dimensions as \code{x},
#' giving the ordering rank of every element of matrix \code{x}. \cr
#'
#' @details
#' If matrix \code{x} is a numeric matrix,
#' and one wants to order the elements of every row or column numerically,
#' \code{x %r~% x} or \code{x %c~% x} would suffice, respectively. \cr
#' \cr
#' If matrix \code{x} is not numeric,
#' \code{x %r~% x} and \code{x %c~% x} are still possible,
#' but probably not the best option.
#' In the non-numeric case,
#' providing a ranking matrix for \code{rank} would be faster and give more accurate ordering.
#' See the examples section. \cr
#' \cr
#' These operators are fully vectorized (no loops or apply-like functions are used).
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
#' mat <- matrix(sample(1:25), nrow=5)
#' print(mat)
#' mat %r~% mat # sort elements of every row
#' mat %r~% -mat # reverse-sort elements of every row
#' mat %c~% mat # sort elements of every column
#' mat %c~% -mat # reverse-sort elements of every column
#'
#'
#' # character matrix (provide own ranking matrix) ====
#'
#' mat <- matrix(sample(letters, 25), nrow=5)
#' print(mat)
#' rank <- stringi::stri_rank(as.vector(mat))
#' rank <- matrix(rank, ncol=ncol(mat))
#' mat %r~% rank # sort elements of every row
#' mat %r~% -rank # reverse-sort elements of every row
#' mat %c~% rank # sort elements of every column
#' mat %c~% -rank # reverse-sort elements of every column
#'
#'
#'
#'

#' @name matrix_ops
NULL

#' @rdname matrix_ops
#' @export
`%r~%` <- function(x, rank) {
  x <- matrix(
    x[order(row(x), rank, decreasing = FALSE)],
    nrow=nrow(x),
    byrow=TRUE
  )
  return(x)
}

#' @rdname matrix_ops
#' @export
`%c~%` <- function(x, rank) {
  x <- matrix(
    x[order(col(x), rank, decreasing = FALSE)],
    ncol=ncol(x),
    byrow=FALSE
  )
  return(x)
}

