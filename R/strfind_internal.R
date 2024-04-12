

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
