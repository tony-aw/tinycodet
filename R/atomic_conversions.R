#' Safer atomic type casting
#'
#' @description
#' Atomic type casting in R is generally performed using the functions
#' \link[base]{as.logical}, \link[base]{as.integer}, \link[base]{as.double},
#' \link[base]{as.character}. \cr
#' There are a few annoying aspect of R with respect to atomic type casting in R
#' using these functions:
#'
#'  * converting an object between atomic types strips the object of its attributes,
#'  including attributes such as names and dimensions.
#'  * the conversions are somewhat inconsistent. For example,
#'  to prevent stripping attributes, one can do something like this: \cr
#'  \code{x[] <- as.numeric()} \cr
#'  but this is not always the same as
#'  first converting the object and then re-assigning the attributes.
#'
#'
#' The functions provided here by the \code{tinyoperations} package
#' do not strip strip away attributes. \cr
#' \cr
#' The functions are as follows: \cr
#'
#'  * \code{as_bool()}: converts object to class \code{logical} (\code{TRUE, FALSE}).
#'  * \code{as_int()}: converts object to class \code{integer}.
#'  * \code{as_dbl()}: converts object to class \code{double} (AKA decimal numbers).
#'  * \code{as_chr()}: converts object to class \code{character}.
#'
#' Moreover, the function \code{is_wholenumber()} is added, to safely test for whole numbers.
#'
#' @param x vector, matrix, array
#' (or similar object where all elements share the same \code{atomic class}),
#' to be converted to some other \code{atomic class}.
#' @param tol the tolerance.
#' @param ... further arguments passed to or from other methods.
#'
#' @returns
#' The converted object.
#'
#' @seealso \link{tinyoperations_dry}
#'
#' @examples
#' x <- c(rep(0, 2), seq(0, 2.5, by=0.5)) |> matrix(ncol=2)
#' colnames(x) <- c("one", "two")
#' attr(x, "test") <- "test"
#' print(x)
#'
#' # notice that in all following, attributes are conserved:
#' as_bool(x)
#' as_int(x)
#' as_dbl(x)
#' as_chr(x)
#'
#'

#' @name atomic_conversions
NULL

#' @rdname atomic_conversions
#' @export
as_bool <- function(x, ...) {
  temp.attr <- attributes(x)
  out <- as.logical(x, ...)
  attributes(out) <- temp.attr
  return(out)
}

#' @rdname atomic_conversions
#' @export
as_int <- function(x, ...) {
  temp.attr <- attributes(x)
  out <- as.integer(x, ...)
  attributes(out) <- temp.attr
  return(out)
}

#' @rdname atomic_conversions
#' @export
as_dbl <- function(x, ...) {
  temp.attr <- attributes(x)
  out <- as.double(x, ...)
  attributes(out) <- temp.attr
  return(out)
}

#' @rdname atomic_conversions
#' @export
as_chr <- function(x, ...) {
  temp.attr <- attributes(x)
  out <- as.character(x, ...)
  attributes(out) <- temp.attr
  return(out)
}

#' @rdname atomic_conversions
#' @export
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  return(abs(x - round(x)) < tol)
}
