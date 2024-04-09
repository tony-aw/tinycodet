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
#' @example inst/examples/str_subset_ops.R


#' @name str_subset_ops
NULL



#' @rdname str_subset_ops
#' @export
`%sget%` <- function(x, ss) {
  ss <- .ss_check(ss, x, sys.call())
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
  ss <- .ss_check(ss, x, sys.call())
  
  n <- stringi::stri_length(x)
  out <- character(length(x))
  test <- rowSums(ss) >= n
  ind_T <- which(test)
  ind_F <- which(!test)
  out[ind_T] <- ""
  if(length(ss) == 2) {
    out[ind_F] <- stringi::stri_sub(x[ind_F], from = 1 + ss[,1], to = n[ind_F] - ss[,2])
  } else {
    out[ind_F] <- stringi::stri_sub(x[ind_F], from = 1 + ss[ind_F,1], to = n[ind_F] - ss[ind_F,2])
  }
  return(out)
}


#' @keywords internal
#' @noRd
.ss_check <- function(ss, x, abortcall) {
  if(!is.numeric(ss)) { stop(simpleError(
    "right hand side must be an integer vector or matrix", call = abortcall
  ))}
  if(!is.integer(ss)) {
    ss.dim <- dim(ss)
    ss <- as.integer(ss)
    dim(ss) <- ss.dim
  }
  if(!is.matrix(ss)) {
    if(length(ss) == 2) {
      ss <- matrix(ss, ncol = 2)
    } else { stop(simpleError(
      "right hand side has wrong length or dimensions", call = abortcall
    ))}
  }
  if(ncol(ss) != 2) {
    stop(simpleError(
      "right hand side has wrong length or dimensions", call = abortcall
    ))
  }
  if(nrow(ss) != length(x) && nrow(ss) != 1) {
    stop(simpleError(
      "right hand side has wrong length or dimensions", call = abortcall
    ))
  }
  if(anyNA(ss)) { stop(simpleError(
    "right hand side cannot contain NA", call = abortcall
  )) } # checking for NAs BEFORE passing to .C_any_neg().
  if(.C_any_neg(ss)) { stop(simpleError(
    "right hand side cannot contain negative numbers", call = abortcall
  )) }
  return(ss)
}
