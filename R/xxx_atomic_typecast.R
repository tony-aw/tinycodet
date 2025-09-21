#' DEPRECATED: Atomic Type Casting With Names and Dimensions Preserved
#'
#' @description
#' THESE FUNCTIONS ARE DEPRECATED, AND WILL BE REMOVED IN A FUTURE VERSION. \cr
#' Please use the functions of the same name from the 'broadcast' package instead. \cr
#'
#'
#' @param x vector, matrix, array
#' (or a similar object where all elements share the same type).
#' @param ... further arguments passed to or from other methods.
#'
#'
#' 
#'
#' @returns
#' The converted object. \cr \cr
#'
#'
#' @example inst/examples/atomic_typecast.R
#'
#' 
#'
#'

#' @name xxx_atomic_typecast
NULL


#' @rdname xxx_atomic_typecast
#' @export
as_bool <- function(x, ...) {
  warning(
    "the `as_*` functions from 'tinycodet' are deprecated;\n",
    "please use the `as_*` functions from the 'broadcast' package instead."
  )
  out <- as.logical(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}


#' @rdname xxx_atomic_typecast
#' @export
as_int <- function(x, ...) {
  warning(
    "the `as_*` functions from 'tinycodet' are deprecated;\n",
    "please use the `as_*` functions from the 'broadcast' package instead."
  )
  out <- as.integer(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}


#' @rdname xxx_atomic_typecast
#' @export
as_dbl <- function(x, ...) {
  warning(
    "the `as_*` functions from 'tinycodet' are deprecated;\n",
    "please use the `as_*` functions from the 'broadcast' package instead."
  )
  out <- as.double(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}


#' @rdname xxx_atomic_typecast
#' @export
as_chr <- function(x, ...) {
  warning(
    "the `as_*` functions from 'tinycodet' are deprecated;\n",
    "please use the `as_*` functions from the 'broadcast' package instead."
  )
  out <- as.character(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}



#' @rdname xxx_atomic_typecast
#' @export
as_cplx <- function(x, ...) {
  warning(
    "the `as_*` functions from 'tinycodet' are deprecated;\n",
    "please use the `as_*` functions from the 'broadcast' package instead."
  )
  out <- as.complex(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}


#' @rdname xxx_atomic_typecast
#' @export
as_raw <- function(x, ...) {
  warning(
    "the `as_*` functions from 'tinycodet' are deprecated;\n",
    "please use the `as_*` functions from the 'broadcast' package instead."
  )
  out <- as.raw(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}

