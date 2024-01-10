#' String Subsetting Operators
#'
#' @description
#'
#' String subsetting operators. \cr
#' \cr
#' The \code{x %sget% ss } operator
#' gets a certain number of the first and last characters of every string in
#' character vector \code{x}. \cr
#' \cr
#' The \code{x %strim% ss } operator
#' trims a certain number of the first and last characters of every string in
#' character vector \code{x}. \cr
#' \cr
#'
#' @param x a character vector.
#' @param ss a vector of length 2, or a matrix with 2 columns with \code{nrow(ss) == length(x)}.
#' The object \code{ss} should consist entirely of non-negative and non-missing integers,
#' or be coerce-able to such integers.
#' (thus negative integers, and missing values are not allowed; decimal numbers will be converted to integers). \cr
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
#'
#'
#' @seealso \link{tinycodet_strings}
#'
#'
#' @examples
#'
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' ss <- c(2,3)
#' x %sget% ss
#'
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' ss <- c(1,0)
#' x %sget% ss
#'
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' ss <- c(2,3)
#' x %strim% ss
#'
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' ss <- c(1,0)
#' x %strim% ss
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
`%sget%` <- function(x, ss) {
  ss <- matrix(as.integer(ss), ncol = 2)
  if(isTRUE(anyNA(ss))) { stop("right hand side cannot contain NA") }
  if(isTRUE(.rcpp_any_neg(ss))) { stop("right hand side cannot contain negative numbers") }
  if(length(ss) != 2 && (length(ss)/2) != length(x)) {
    stop("`nrow(ss)` must be equal to `length(x)`, or must be a vector of length 2")
  }
  n <- stringi::stri_length(x)
  first <- stringi::stri_sub(x, from = 1, to = ss[,1])
  last <-  stringi::stri_sub(x, from = n - ss[,2] + 1, to = n)
  
  out <- character(length(x))
  test <- rowSums(ss) >= n
  ind_T <- which(test)
  ind_F <- which(!test)
  out[ind_T] <- x[ind_T]
  out[ind_F] <- stringi::stri_c(first[ind_F], last[ind_F], sep = "")
  
  return(out)
}

#' @rdname str_subset_ops
#' @export
`%strim%` <- function(x, ss) {
  ss <- matrix(as.integer(ss), ncol=2)
  if(isTRUE(anyNA(ss))) { stop("right hand side cannot contain NA") }
  if(isTRUE(.rcpp_any_neg(ss))) { stop("right hand side cannot contain negative numbers") }
  
  n <- stringi::stri_length(x)
  out <- character(length(x))
  test <- rowSums(ss) >= n
  ind_T <- which(test)
  ind_F <- which(!test)
  out[ind_T] <- ""
  if(length(ss) == 2) {
    out[ind_F] <- stringi::stri_sub(x[ind_F], from = 1 + ss[,1], to = n[ind_F] - ss[,2])
  } else if((length(ss)/2) == length(x)) {
    out[ind_F] <- stringi::stri_sub(x[ind_F], from = 1 + ss[ind_F,1], to = n[ind_F] - ss[ind_F,2])
  } else {
    stop("`nrow(ss)` must be equal to `length(x)`, or must be a vector of length 2")
  }
  return(out)
}

