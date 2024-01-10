#' 'stringi' Pattern Search Operators
#'
#' @description
#'
#' The \code{x %s{}% p} operator
#' checks for every string in character vector \code{x} if
#' the pattern defined in \code{p} is present. \cr
#' When supplying a list on the right hand side (see \link{s_pattern}),
#' one can optionally include the list element \code{at = "start"} or \code{at = "end"}:
#' 
#'  * Supplying  \code{at = "start"}
#'  will check if the pattern appears at the start of a string
#'  (like \link[stringi]{stri_startswith}).
#'  * Supplying  \code{at = "end"}
#'  will check if the pattern appears at the end of a string
#'  (like\link[stringi]{stri_endswith}). \cr
#'  
#' The \code{x %s!{}% p} operator is the same as \code{x %s{}% p},
#' except it checks for \bold{absence} of the pattern occurrence,
#' rather then presence. \cr
#' \cr
#' For string (in)equality operators, see \link[stringi]{%s==%} from the 'stringi' package. \cr
#' \cr
#' \code{strfind()<-}
#' locates, extracts, or replaces found patterns. \cr
#' It complements the other string-related operators,
#' and uses the same \link{s_pattern} API. \cr
#' It functions as follows:
#'  * \code{strfind()} attempts to find all pattern matches,
#' and returns the extractions of the findings in a list,
#' just like \link[stringi]{stri_extract_all}.
#'  * \code{strfind(..., i = "all")} attempts to find all pattern matches,
#' and reports the locations of the findings in a list,
#' just like \link[stringi]{stri_locate_all}.
#'  * \code{strfind(..., i = i)}, where \code{i} is an integer vector,
#' locates the \eqn{i^{th}} occurrence of a pattern,
#' and reports the locations in a matrix,
#' just like \link{stri_locate_ith}.
#'  * \code{strfind(...) <- value} attempts to find all pattern matches,
#' and replaces them with the character vector specified in \code{value}. \cr
#' This is similar to \link[stringi]{stri_replace_all},
#' except the replacement is done in-place
#' (though not by reference, technically speaking). \cr \cr
#' 
#'
#'
#' @param x a string or character vector.
#' @param p either a list with 'stringi' arguments (see \link{s_pattern}),
#' or else a character vector of the same length as \code{x} or length 1
#' with regular expressions. See also the Details section. \cr
#' `r .mybadge_string("regex", "darkred")` \cr
#' `r .mybadge_string("fixed", "darkgreen")` \cr
#' `r .mybadge_string("coll", "pink")` \cr
#' `r .mybadge_string("charclass", "lightyellow")` \cr
#' @param i either one of the following:
#'  * if \code{i} is not given or \code{NULL}, 
#'  \code{strfind()} extracts all found pattern occurrences.
#'  * if \code{i = "all"}, \code{strfind()} locates all found pattern occurrences.
#'  * if \code{i} is an integer,
#'  \code{strfind()} locates the \eqn{i^{th}} pattern occurrences. \cr
#'  See the `i` argument in \link{stri_locate_ith} for details.
#'  * for \code{strfind() <- value}, `i` must not be specified.
#' @param value a character vector giving the replacement values.
#' @param ... additional arguments to be passed to the 'stringi' functions. \cr \cr
#'
#'
#' @details
#' \bold{Right-hand Side List for the \code{%s{}%} and \code{%s!{}%} Operators} \cr
#' When supplying a list to the right-hand side of the
#' \code{%s{}%} and \code{%s!{}%} operators,
#' one can add the argument \code{at}. \cr
#' If \code{at = "start"},
#' the operators will check if the pattern is present/absent at the start of the string. \cr
#' If \code{at = "end"},
#' the operators will check if the pattern is present/absent at the end of the string. \cr
#' Unlike \link[stringi]{stri_startswith} or \link[stringi]{stri_endswith},
#' \code{regex} \bold{is} supported by the \code{%s{}%} and \code{%s!{}%} operators. \cr
#' See examples below. \cr
#' 
#'
#' @returns
#' The \code{x %s{}% p} and \code{x %s!{}% p} operators
#' return logical vectors. \cr
#' \cr
#' \code{strfind()} returns a list with extractions of all found patterns. \cr
#' \cr
#' \code{strfind(..., i = "all")} returns a list with all found pattern locations. \cr
#' \cr
#' \code{strfind(..., i = i)},
#' with `i` being an integer,
#' returns an integer matrix with two columns,
#' giving the start and end positions of the \eqn{i^{th}} matches,
#' two NAs if no matches are found, and also two `NA`s if str is `NA`. \cr
#' \cr
#' \code{strfind(x, p) <- value} returns nothing,
#' but performs in-place replacement of the found patterns in `x`. \cr
#'
#'
#' @seealso \link{tinycodet_strings}
#'
#'
#' @examples
#'
#' # example of %s{}% and %s!{}% ====
#'
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' x %s{}% "a"
#' x %s!{}% "a"
#' which(x %s{}% "a")
#' which(x %s!{}% "a")
#' x[x %s{}% "a"]
#' x[x %s!{}% "a"]
#' x[x %s{}% "a"] <- 1
#' x[x %s!{}% "a"] <- 1
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' x %s{}% "1"
#' x %s!{}% "1"
#' which(x %s{}% "1")
#' which(x %s!{}% "1")
#' x[x %s{}% "1"]
#' x[x %s!{}% "1"]
#' x[x %s{}% "1"] <- "a"
#' x[x %s!{}% "1"] <- "a"
#' print(x)
#' 
#' #############################################################################
#' 
#'
#' # Example of %s{}% and %s!{}% with "at" argument ====
#'
#' x <- c(paste0(letters, collapse = ""),
#'        paste0(rev(letters), collapse = ""), NA)
#' p <- s_fixed("abc", at = "start")
#' x %s{}% p
#' stringi::stri_startswith(x, fixed = "abc") # same as above
#' 
#' p <- s_fixed("xyz", at = "end")
#' x %s{}% p
#' stringi::stri_endswith(x, fixed = "xyz") # same as above
#' 
#' p <- s_fixed("cba", at = "end")
#' x %s{}% p
#' stringi::stri_endswith(x, fixed = "cba") # same as above
#' 
#' p <- s_fixed("zyx", at = "start")
#' x %s{}% p
#' stringi::stri_startswith(x, fixed = "zyx") # same as above
#' 
#' 
#' #############################################################################
#' 
#' 
#' # Example of strfind for replace-all ====
#' 
#' x <- rep('The quick brown fox jumped over the lazy dog.', 3)
#' print(x)
#' p <- c('quick', 'brown', 'fox')
#' rp <- c('SLOW',  'BLACK', 'BEAR')
#' x %s{}% p
#' strfind(x, p)
#' strfind(x, p) <- rp
#' print(x)
#' 
#' 
#' #############################################################################
#' 
#' # Example of strfind for replace ith ====
#' 
#' # new character vector:
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#'
#' # report ith (second and second-last) vowel locations:
#' p <- s_regex( # vowels
#'   rep("A|E|I|O|U", 2),
#'   case_insensitive = TRUE
#' )
#' loc <- strfind(x, p, i = c(2, -2))
#' print(loc)
#'
#' # extract ith vowels:
#' extr <- stringi::stri_sub(x, from = loc)
#' print(extr)
#'
#' # replace ith vowels with numbers:
#' repl <- chartr("aeiou", "12345", extr)
#' stringi::stri_sub(x, loc) <- repl
#' print(x)
#' 
#' 
#'

#' @name str_search
NULL

#' @rdname str_search
#' @export
`%s{}%` <- function(x, p) {
  if(is.list(p)){
    return(.str_inop_search_lst(x, p, negate = FALSE, sys.call()))
  }
  if(is.character(p)) {
    return(stringi::stri_detect(x, regex = p, negate = FALSE))
  } else {
    stop("right hand side must be a character vector or list")
  }
}


#' @rdname str_search
#' @export
`%s!{}%` <- function(x, p) {
  if(is.list(p)){
    return(.str_inop_search_lst(x, p, negate = TRUE, sys.call()))
  }
  if(is.character(p)) {
    return(stringi::stri_detect(x, regex = p, negate = TRUE))
  } else {
    stop("right hand side must be a character vector or list")
  }
}


#' @rdname str_search
#' @export
strfind <- function(x, p, i = NULL, ...) {
  if(!is.null(i) && is.numeric(i)) {
    return(.strfind_locate_ith(x, p, i, ..., abortcall = sys.call()))
  }
  else if(!is.null(i) && isTRUE(i == "all")) {
    return(.strfind_locate_all(x, p, ..., abortcall = sys.call()))
  }
  else if(is.null(i)) {
    return(.strfind_extract_all(x, p, ..., abortcall = sys.call()))
  }
  else {
    stop("improper `i` given")
  }
}


#' @rdname str_search
#' @export
`strfind<-` <- function(x, p, ..., value) {
  if(is.list(p)){
    
    args <- list(str = x,replacement = value)
    return(do.call(stringi::stri_replace_all, c(args, p, list(...))))
    
  } else if(is.character(p)) {
    
    return(stringi::stri_replace_all(
      str = x, replacement = value, regex = p, ...
    ))
    
  } else {
    stop("`p` must be a character vector or list")
  }
}


#' @keywords internal
#' @noRd
.str_inop_search_lst <- function(x, lst, negate, abortcall) {
  if(isTRUE(lst[["at"]] == "start"))
  {
    lst[["at"]] <- NULL
    return(.str_inop_search_start(x, lst, negate = negate))
  }
  else if(isTRUE(lst[["at"]] == "end"))
  {
    lst[["at"]] <- NULL
    return(.str_inop_search_end(x, lst, negate = negate))
  }
  else if(!is.null(lst[["at"]]))
  {
    stop(simpleError("improper `at` argument given", call = abortcall))
  }
  else
  {
    return(do.call(stringi::stri_detect, c(list(str = x, negate = negate), lst)))
  }
}


#' @keywords internal
#' @noRd
.str_inop_search_start <- function(x, lst, negate) {
  if(!is.null(lst[["regex"]])) {
    pattern <- lst[["regex"]]
    pattern[pattern == ""] <- NA
    pattern <- stringi::stri_c("^(", pattern, ")")
    lst[["regex"]] <- NULL
    args <- list(str = x, pattern = pattern, negate = negate, max_count = -1)
    return(do.call(stringi::stri_detect_regex, c(args, lst)))
  } else {
    args <- list(str = x, negate = negate, from = 1L)
    return(do.call(stringi::stri_startswith, c(args, lst)))
  }
}


#' @keywords internal
#' @noRd
.str_inop_search_end <- function(x, lst, negate) {
  if(!is.null(lst[["regex"]])) {
    pattern <- lst[["regex"]]
    pattern[pattern == ""] <- NA
    pattern <- stringi::stri_c("(", pattern, ")$")
    lst[["regex"]] <- NULL
    args <- list(str = x, pattern = pattern, negate = negate, max_count = -1)
    return(do.call(stringi::stri_detect_regex, c(args, lst)))
  } else {
    args <- list(str = x, negate = negate, to = -1L)
    return(do.call(stringi::stri_endswith, c(args, lst)))
  }
}


#' @keywords internal
#' @noRd
.strfind_locate_ith <- function(x, p, i, ..., abortcall) {
  if(is.list(p)){
    
    args <- list(str = x, i = i)
    return(do.call(stri_locate_ith, c(args, p, list(...))))
    
  } else if(is.character(p)) {
    
    return(stri_locate_ith(
      str = x, i = i, regex = p, ...
    ))
    
  } else {
    stop(simpleError("`p` must be a character vector or list", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.strfind_locate_all <- function(x, p, ..., abortcall) {
  if(is.list(p)){
    return(do.call(stringi::stri_locate_all, c(list(str = x), p, list(...))))
  } else if(is.character(p)) {
    return(stringi::stri_locate_all(
      str = x, regex = p, ...
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
  } else if(is.character(p)) {
    return(stringi::stri_extract_all(
      str = x, regex = p, ...
    ))
  } else {
    stop(simpleError("`p` must be a character vector or list", call = abortcall))
  }
}

