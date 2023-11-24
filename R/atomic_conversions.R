#' Atomic Type Casting Without Stripping Attributes
#'
#' @description
#' Atomic type casting in R is generally performed using the functions
#' \link[base]{as.logical}, \link[base]{as.integer}, \link[base]{as.double},
#' \link[base]{as.character}. \cr
#' \cr
#' Converting an object between atomic types using these functions
#' strips the object of its attributes,
#' including attributes such as names and dimensions. \cr
#' \cr
#' The functions provided here by the 'tinycodet' package
#' preserve all attributes - except the "class" attribute. \cr
#' \cr
#' The functions are as follows: \cr
#'
#'  * \code{as_bool()}: converts object to atomic type \code{logical} (\code{TRUE, FALSE, NA}).
#'  * \code{as_int()}: converts object to atomic type \code{integer}.
#'  * \code{as_dbl()}: converts object to atomic type \code{double} (AKA decimal numbers).
#'  * \code{as_chr()}: converts object to atomic type \code{character}.
#'
#' Moreover, the function \code{is_wholenumber()} is added, to safely test for whole numbers.
#'
#' @param x vector, matrix, array
#' (or a similar object where all elements share the same type).
#' @param tol numeric, giving the tolerance.
#' @param ... further arguments passed to or from other methods.
#'
#' @returns
#' The converted object.
#'
#' @seealso [tinycodet_safer()]
#'
#' @examples
#' x <- c(rep(0, 2), seq(0, 2.5, by=0.5)) |> matrix(ncol=2)
#' colnames(x) <- c("one", "two")
#' attr(x, "test") <- "test"
#' print(x)
#'
#' # notice that in all following, attributes (except class) are conserved:
#' as_bool(x)
#' as_int(x)
#' as_dbl(x)
#' as_chr(x)
#'
#' # is_wholenumber:
#' is_wholenumber(1:10 + c(0, 0.1))
#'
#'

#' @name atomic_conversions
NULL

#' @rdname atomic_conversions
#' @export
as_bool <- function(x, ...) {
  temp.attr <- attributes(x)
  temp.attr[["class"]] <- NULL
  out <- as.logical(x, ...)
  attributes(out) <- temp.attr
  return(out)
}

#' @rdname atomic_conversions
#' @export
as_int <- function(x, ...) {
  temp.attr <- attributes(x)
  temp.attr[["class"]] <- NULL
  out <- as.integer(x, ...)
  attributes(out) <- temp.attr
  return(out)
}

#' @rdname atomic_conversions
#' @export
as_dbl <- function(x, ...) {
  temp.attr <- attributes(x)
  temp.attr[["class"]] <- NULL
  out <- as.double(x, ...)
  attributes(out) <- temp.attr
  return(out)
}

#' @rdname atomic_conversions
#' @export
as_chr <- function(x, ...) {
  temp.attr <- attributes(x)
  temp.attr[["class"]] <- NULL
  out <- as.character(x, ...)
  attributes(out) <- temp.attr
  return(out)
}

#' @rdname atomic_conversions
#' @export
is_wholenumber <- function(x, tol = sqrt(.Machine$double.eps)) {
  if(!is.numeric(tol)) {
    stop("`tol` must be numeric")
  }
  return(abs(x - round(x)) < tol)
}
