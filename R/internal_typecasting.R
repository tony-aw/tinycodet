
#' @keywords internal
#' @noRd
.as_bool <- function(x, ...) {
  
  out <- as.logical(x, ...)
  .typecast_attr(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
.as_int <- function(x, ...) {
  
  out <- as.integer(x, ...)
  .typecast_attr(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
.as_dbl <- function(x, ...) {
  
  out <- as.double(x, ...)
  .typecast_attr(out) <- x
  return(out)
}

#' @keywords internal
#' @noRd
.as_num <- .as_dbl


#' @keywords internal
#' @noRd
.as_chr <- function(x, ...) {
  
  out <- as.character(x, ...)
  .typecast_attr(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
.as_str <- .as_chr



#' @keywords internal
#' @noRd
.as_cplx <- function(x, ...) {
  
  out <- as.complex(x, ...)
  .typecast_attr(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
.as_raw <- function(x, ...) {
  
  out <- as.raw(x, ...)
  .typecast_attr(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
`.typecast_attr<-` <- function(x, value) {
  if(length(value) == length(x)) {
    dim(x) <- dim(value)
    dimnames(x) <- dimnames(value)
    names(x) <- names(value)
    comment(x) <- comment(value)
  }
  x
}


