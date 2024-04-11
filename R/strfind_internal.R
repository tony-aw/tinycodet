
#' @keywords internal
#' @noRd
.strfind_locate_mode <- function(x, p, mode, ..., abortcall) {
  # using if-else instead of stringi::stri_locate():
  # if-else is significantly faster than match.arg + switch
  
  if(mode == "all") {
    return(.strfind_locate_all(x, p, ..., abortcall = abortcall))
  }
  else if(mode == "first") {
    return(.strfind_locate_first(x, p, ..., abortcall = abortcall))
  }
  else if(mode == "last") {
    return(.strfind_locate_last(x, p, ..., abortcall = abortcall))
  }
  else {
    stop("improper `i` given")
  }
}

#' @keywords internal
#' @noRd
.strfind_locate_all <- function(x, p, ..., abortcall) {
  if(is.list(p)){
    return(do.call(stringi::stri_locate_all, c(list(str = x), p, list(...))))
  }
  else if(is.character(p)) {
    return(stringi::stri_locate_all_regex(
      str = x, pattern = p, ...
    ))
  }
  else {
    stop(simpleError("`p` must be a character vector or list", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.strfind_locate_first <- function(x, p, ..., abortcall) {
  if(is.list(p)){
    return(do.call(stringi::stri_locate_first, c(list(str = x), p, list(...))))
  }
  else if(is.character(p)) {
    return(stringi::stri_locate_first_regex(
      str = x, pattern = p, ...
    ))
  }
  else {
    stop(simpleError("`p` must be a character vector or list", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.strfind_locate_last <- function(x, p, ..., abortcall) {
  if(is.list(p)){
    return(do.call(stringi::stri_locate_last, c(list(str = x), p, list(...))))
  }
  else if(is.character(p)) {
    return(stringi::stri_locate_last_regex(
      str = x, pattern = p, ...
    ))
  }
  else {
    stop(simpleError("`p` must be a character vector or list", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.strfind_locate_ith <- function(x, p, i, ..., abortcall) {
  if(is.list(p)){
    
    args <- list(str = x, i = i)
    return(do.call(stri_locate_ith, c(args, p, list(...))))
    
  } else if(is.character(p)) {
    
    return(stri_locate_ith_regex(
      str = x, pattern = p, i = i,  ...
    ))
    
  } else {
    stop(simpleError("`p` must be a character vector or list", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.strfind_extract_all <- function(x, p, ..., abortcall) {
  if(is.list(p)){
    return(do.call(stringi::stri_extract_all, c(list(str = x), p, list(...))))
  }
  else if(is.character(p)) {
    return(stringi::stri_extract_all_regex(
      str = x, pattern = p, ...
    ))
  }
  else {
    stop(simpleError("`p` must be a character vector or list", call = abortcall))
  }
}
