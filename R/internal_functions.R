#' Internal functions
#'
#'
#'
#'
#'
#'
#'


#' @keywords internal
#' @noRd
.internal_dirfolder <- function(x) {
  return(dirname(x) |> normalizePath())
}

#' @keywords internal
#' @noRd
.internal_is_formula <- function(form) {
  check <- inherits(form, "formula") && is.call(form) && isTRUE(form[[1]] == "~")
  return(check)
}


#' @keywords internal
#' @noRd
.internal_is_missingstring <- function(x) {
  if(!is.character(x) || length(x) > 1L) return(FALSE)
  if(length(x) == 0L) return(TRUE)
  return(is.na(x) || nchar(x) == 0L)
}

#' @keywords internal
#' @noRd
.internal_is_string <- function(x) {
  return(is.character(x) && length(x) == 1L && !is.na(x) && nchar(x) > 0L)
}

#' @keywords internal
#' @noRd
.internal_is_wholenum <- function(x) {
  return(is.numeric(x) && length(x) == 1L && !is.na(x) && !is.infinite(x) && round(x) == x)
}
#' 
#' #' @keywords internal
#' #' @noRd
#' .internal_grep_ops <- function(nms, type, invert = FALSE) {
#'   
#'   is_infix <- stringi::stri_detect_regex(nms, "%|:=")
#'   is_rpops <- stringi::stri_endswith_fixed(nms, "<-")
#'   rpbases <- stringi::stri_replace_all_fixed(nms[is_rpops], "<-", "")
#'   is_rpbase <- nms %in% rpbases
#'   
#'   
#'   if(!invert) {
#'     is <- is_infix | is_rpops | is_rpbase
#'   }
#'   else {
#'     is <- !is_infix & !is_rpops & !is_rpbase
#'   }
#'   
#'   if(type == 0) {
#'     return(is)
#'   }
#'   else if(type == 1) {
#'     return(which(is))
#'   }
#'   else if(type == 2) {
#'     return(nms[is])
#'   }
#'   else {
#'     stop("unknown type given")
#'   }
#' }
#' 
