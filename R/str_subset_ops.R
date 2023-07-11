#' String subsetting operators
#'
#'@description
#' String subsetting operators. \cr
#' \cr
#' The \code{x %ss% s } operator
#' allows indexing a single string as-if it is an iterable object. \cr
#' \cr
#' The \code{x %sget% ss } operator
#' gives a certain number of the first and last characters of
#' character vector \code{x}. \cr
#' \cr
#' The \code{x %strim% ss } operator
#' removes a certain number of the first and last characters of
#' character vector \code{x}. \cr
#' \cr
#'
#' @param s a numeric vector giving the subset indices.
#' @param x a string or character vector.
#' @param ss a vector of length 2, or a matrix with 2 columns with \code{nrow(ss)==length(x)}.
#' The object \code{ss} should consist entirely of non-negative integers
#' (thus 0, 1, 2, etc. are valid, but -1, -2, -3 etc are not valid). \cr
#' The first element/column of \code{ss}
#' gives the number of characters counting from the left side to be
#' extracted/removed from \code{x}. \cr
#' The second element/column of \code{ss}
#' gives the number of characters counting from the right side to be
#' extracted/removed from \code{x}. \cr
#'
#'
#'
#' @details
#' These operators serve as a way to provide straight-forward string sub-setting. \cr
#' \cr
#'
#'
#' @returns
#' The \code{x %sget% ss } operator
#' gives a certain number of the first and last characters of
#' character vector \code{x}. \cr
#' \cr
#' The \code{x %strim% ss } operator
#' removes a certain number of the first and last characters of
#' character vector \code{x}. \cr
#' \cr
#' The \code{%ss%} operator always returns a vector or matrix,
#' where each element is a single character. \cr
#' \cr
#'
#'
#' @examples
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' ss <- c(2,3)
#' x %sget% ss
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' ss <- c(1,0)
#' x %sget% ss
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' ss <- c(2,3)
#' x %strim% ss
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' ss <- c(1,0)
#' x %strim% ss
#' 
#' "hello" %ss% 5:1
#'
#'
#'
#'
#'
#'
#'


#' @name str_subset_ops
NULL

#' @rdname str_subset_ops
#' @export
`%ss%` <- function(x, s){
  unlist(strsplit(x, split=""))[s]
}

#' @rdname str_subset_ops
#' @export
`%sget%` <- function(x, ss) {
  ss <- matrix(ss, ncol=2)
  if(isTRUE(any(ss<0))){stop("ss cannot contain negative numbers")}
  n <- nchar(x)
  first <- substr(x, 1, ss[,1])
  last <- substr(x, n - ss[,2] + 1, n)
  out <- ifelse(rowSums(ss)>=n, x, paste(first, last, sep = ""))
  return(out)
}

#' @rdname str_subset_ops
#' @export
`%strim%` <- function(x, ss) {
  ss <- matrix(ss, ncol=2)
  if(isTRUE(any(ss<0))){stop("ss cannot contain negative numbers")}
  n <- nchar(x)
  out <- ifelse(rowSums(ss)>=n, "", substr(x, 1+ss[,1], n-ss[,2]))
  return(out)
}

