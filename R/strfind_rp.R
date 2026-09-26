#' @rdname str_search
#' @export
`strfind<-` <- function(x, p, ..., i, rt, value) {
  
  if(!missing(i)) warning("`i` ignored in `strfind() <-`")
  if(missing(rt)) rt <- NULL
  
  if(!is.atomic(value)) {
    stop("right-hand side must be atomic")
  }
  if(length(rt) > 1) {
    stop("improper `rt` given")
  }
  
  if(is.list(p))
  {
    if(is.null(rt) || isTRUE(rt == "vec")) {
      args <- list(str = x, replacement = value, vectorize_all = TRUE)
      return(do.call(stringi::stri_replace_all, c(args, p, list(...))))
    }
    else if(rt == "dict") {
      args <- list(str = x, replacement = value, vectorize_all = FALSE)
      return(do.call(stringi::stri_replace_all, c(args, p, list(...))))
    }
    else if(rt == "first") {
      args <- list(str = x, replacement = value)
      return(do.call(stringi::stri_replace_first, c(args, p, list(...))))
    }
    else if(rt == "last") {
      args <- list(str = x, replacement = value)
      return(do.call(stringi::stri_replace_last, c(args, p, list(...))))
    }
    else {stop("improper `rt` given")}
  }
  else if(is.character(p))
  {
    if(is.null(rt) || isTRUE(rt =="vec")) {
      return(stringi::stri_replace_all_regex(
        x, p, replacement = value, vectorize_all = TRUE, ...
      ))
    }
    else if(rt == "dict") {
      return(stringi::stri_replace_all_regex(
        x, p, replacement = value, vectorize_all = FALSE, ...
      ))
    }
    else if(rt == "first") {
      return(stringi::stri_replace_first_regex(
        x, p, replacement = value, ...
      ))
    }
    else if(rt == "last") {
      return(stringi::stri_replace_last_regex(
        x, p, replacement = value, ...
      ))
    }
    else {stop("improper `rt` given")}
  }
  else {
    stop("`p` must be a character vector or list")
  }
}
