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
#'  (like \link[stringi]{stri_endswith}). \cr
#'  
#' The \code{x %s!{}% p} operator is the same as \code{x %s{}% p},
#' except it checks for \bold{absence} of the pattern,
#' rather than presence. \cr
#' \cr
#' For string (in)equality operators,
#' see \link[stringi]{%s==%} from the 'stringi' package. \cr
#' \cr
#' \code{strfind()<-}
#' locates, extracts, or replaces found patterns. \cr
#' It complements the other string-related operators,
#' and uses the same \link{s_pattern} API. \cr
#' It functions as follows:
#'  * \code{strfind()} finds all pattern matches,
#' and returns the extractions of the findings in a list,
#' just like \link[stringi]{stri_extract_all}.
#'  * \code{strfind(..., i = "all")} finds all pattern matches,
#' and reports the locations of the findings in a list,
#' just like \link[stringi]{stri_locate_all}.
#'  * \code{strfind(..., i = i)}, where \code{i} is an integer vector,
#' locates the \eqn{i^{th}} occurrence of a pattern,
#' and reports the locations in a matrix,
#' just like \link{stri_locate_ith}.
#'  * \code{strfind() <- value} finds pattern matches in variable `x`,
#' replaces the pattern matches with the character vector specified in \code{value},
#' and assigns the transformed character vector back to `x`. \cr
#' This is similar to \link[stringi]{stri_replace_all},
#' except the replacement is done in-place. \cr \cr
#' 
#'
#'
#' @param x a string or character vector. \cr
#' For `strfind()<-`,
#' `x` must obviously be the variable containing the character vector/string,
#' since `strfind()<-` performs assignment in-place. \cr
#' @param p either a list with 'stringi' arguments (see \link{s_pattern}),
#' or else a character vector with regular expressions. \cr
#' See also the Details section. \cr
#' `r .mybadge_string("regex", "darkred")` \cr
#' `r .mybadge_string("fixed", "darkgreen")` \cr
#' `r .mybadge_string("coll", "pink")` \cr
#' `r .mybadge_string("charclass", "lightyellow")` \cr
#' @param value a character vector giving the replacement values.
#' @param i either one of the following can be given for `i`:
#'  * if \code{i} is not given or \code{NULL}, 
#'  \code{strfind()} extracts all found pattern occurrences.
#'  * if \code{i = "all"}, \code{strfind()} locates all found pattern occurrences.
#'  * if \code{i} is an integer,
#'  \code{strfind()} locates the \eqn{i^{th}} pattern occurrences. \cr
#'  See the `i` argument in \link{stri_locate_ith} for details.
#'
#' For \code{strfind() <- value}, `i` must not be specified.
#' @param rt use `rt` to specify the Replacement Type that `strfind()<-` should perform. \cr
#' Either one of the following can be given for `rt`: 
#'  * if `rt` is not given, `NULL` or `"vec"`,
#'  `strfind()<-` performs regular, vectorized replacement of \bold{all} occurrences.
#'  * if `rt = "dict"`,
#'  `strfind()<-` performs dictionary replacement of \bold{all} occurrences. \cr
#'  * if `rt = "first"`,
#'  `strfind()<-` replaces only the first occurrences.
#'  * if `rt = "last"`,
#'  `strfind()<-` replaces only the last occurrences.
#' 
#' Note: `rt = "first"` and `rt = "last"` only exist for convenience;
#' for more specific locational replacement,
#' use \link{stri_locate_ith} or `strfind(..., i)` with numeric `i`
#' (see the Examples section). \cr
#' For \code{strfind()}, `rt` must not be specified.
#' @param ... additional arguments to be specified.
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
#' \cr
#'
#' \bold{Vectorized Replacement vs Dictionary Replacement} \cr
#' - Vectorized replacement: \cr
#' `x`, `p`, and `value` are of the same length
#' (or recycled to become the same length). \cr
#' \bold{All} occurrences of pattern `p[j]` in `x[j]` is replaced with `value[j]`,
#' for every `j`.
#'  - Dictionary replacement: \cr
#'  `p` and `value` are of the same length,
#' and their length is independent of the length of `x`. \cr
#' For every single string in `x`,
#' all occurrences of pattern `p[1]` are replaced with `value[1]`, \cr
#' all occurrences of pattern `p[2]` are replaced with `value[2]`, 
#' etc. \cr
#' 
#' Notice that for single replacement, i.e. `rt = "first"` or `rt = "last"`,
#' it makes no sense to distinguish between vectorized or dictionary replacement,
#' since then only a single occurrence is being replaced per string. \cr
#' See examples below. \cr \cr
#' 
#' 
#' @section Warning:
#' `strfind()<-` performs in-place replacement. \cr
#' Therefore, the character vector or string to perform replacement on,
#' must already exist as a variable. \cr
#' So take for example the following code:
#' 
#' ```
#' strfind("hello", p = "e") <- "a" # this obviously does not work
#' 
#' y <- "hello"
#' strfind(y, p = "e") <- "a" # this works fine
#' 
#' ```
#' In the above code, the first `strfind()<-` call does not work,
#' because the string needs to exist as a variable. \cr
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
#' \code{strfind() <- value} returns nothing,
#' but performs in-place replacement
#' (but not by reference, technically speaking)
#' of the found patterns in variable `x`. \cr \cr
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
#' # Example of transforming ith occurrence ====
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
#' repl <- chartr("aeiou", "12345", extr) # transformation
#' stringi::stri_sub(x, loc) <- repl
#' print(x)
#' 
#' 
#' #############################################################################
#' 
#' 
#' # Example of strfind for regular vectorized replacement ====
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
#' #############################################################################
#' 
#' 
#' # Example of strfind for dictionary replacement ====
#' 
#' x <- rep('The quick brown fox jumped over the lazy dog.', 3)
#' print(x)
#' p <- c('quick', 'brown', 'fox')
#' rp <- c('SLOW',  'BLACK', 'BEAR')
#' # thus dictionary is:
#' # quick => SLOW; brown => BLACK; fox => BEAR
#' strfind(x, p, rt = "dict") <- rp
#' print(x)
#' 
#' 
#' #############################################################################
#' 
#' 
#' # Example of strfind for first and last replacement ====
#' 
#' x <- rep('The quick brown fox jumped over the lazy dog.', 3)
#' print(x)
#' p <- s_fixed("the", case_insensitive = TRUE)
#' rp <- "One"
#' strfind(x, p, rt = "first") <- rp
#' print(x)
#' 
#' x <- rep('The quick brown fox jumped over the lazy dog.', 3)
#' print(x)
#' p <- s_fixed("the", case_insensitive = TRUE)
#' rp <- "Some Other"
#' strfind(x, p, rt = "last") <- rp
#' print(x)
#' 
#' 
#' 
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
  else if(is.character(p)) {
    return(stringi::stri_detect(x, regex = p, negate = FALSE))
  }
  else {
    stop("right hand side must be a character vector or list")
  }
}


#' @rdname str_search
#' @export
`%s!{}%` <- function(x, p) {
  if(is.list(p)){
    return(.str_inop_search_lst(x, p, negate = TRUE, sys.call()))
  }
  else if(is.character(p)) {
    return(stringi::stri_detect(x, regex = p, negate = TRUE))
  }
  else {
    stop("right hand side must be a character vector or list")
  }
}


#' @rdname str_search
#' @export
strfind <- function(x, p, ..., i, rt) {
  
  if(missing(i)) i <- NULL
  if(!missing(rt)) warning("`rt` ignored in `strfind()`")
  
  if(is.null(i)) {
    return(.strfind_extract_all(x, p, ..., abortcall = sys.call()))
  }
  else if(is.numeric(i)) {
    return(.strfind_locate_ith(x, p, i, ..., abortcall = sys.call()))
  }
  else if(length(i==1) && i == "all") {
    return(.strfind_locate_all(x, p, ..., abortcall = sys.call()))
  }
  else {
    stop("improper `i` given")
  }
}


#' @rdname str_search
#' @export
`strfind<-` <- function(x, p, ..., i, rt, value) {
  
  if(!missing(i)) warning("`i` ignored in `strfind() <-`")
  if(missing(rt)) rt <- NULL
  
  if(!is.atomic(value)) {
    stop("right-hand side must be a vector")
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
    else {stop("unknown `rt` given")}
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
    else {stop("unknown `rt` given")}
  }
  else {
    stop("`p` must be a character vector or list")
  }
}


#' @keywords internal
#' @noRd
.str_inop_search_lst <- function(x, lst, negate, abortcall) {
  at <- lst[["at"]]
  lst[["at"]] <- NULL
  
  if(!is.null(at)) {
    if(at == "start") {
      return(.str_inop_search_start(x, lst, negate = negate))
    }
    else if(at == "end") {
      return(.str_inop_search_end(x, lst, negate = negate))
    }
    else {
      stop(simpleError("improper `at` argument given", call = abortcall))
    }
  }
  else {
    return(do.call(stringi::stri_detect, c(list(str = x, negate = negate), lst)))
  }
}


#' @keywords internal
#' @noRd
.str_inop_search_start <- function(x, lst, negate) {
  
  regexpattern <- lst[["regex"]]
  lst[["regex"]] <- NULL
  
  if(!is.null(regexpattern)) {
    regexpattern[regexpattern == ""] <- NA
    regexpattern <- stringi::stri_c("^(", regexpattern, ")")
    
    args <- list(str = x, pattern = regexpattern, negate = negate, max_count = -1)
    return(do.call(stringi::stri_detect_regex, c(args, lst)))
  }
  else {
    args <- list(str = x, negate = negate, from = 1L)
    return(do.call(stringi::stri_startswith, c(args, lst)))
  }
}


#' @keywords internal
#' @noRd
.str_inop_search_end <- function(x, lst, negate) {
  
  regexpattern <- lst[["regex"]]
  lst[["regex"]] <- NULL
  
  if(!is.null(regexpattern)) {
    regexpattern[regexpattern == ""] <- NA
    regexpattern <- stringi::stri_c("(", regexpattern, ")$")
    args <- list(str = x, pattern = regexpattern, negate = negate, max_count = -1)
    return(do.call(stringi::stri_detect_regex, c(args, lst)))
  }
  else {
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
  }
  else if(is.character(p)) {
    return(stringi::stri_locate_all(
      str = x, regex = p, ...
    ))
  }
  else {
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
    return(stringi::stri_extract_all(
      str = x, regex = p, ...
    ))
  }
  else {
    stop(simpleError("`p` must be a character vector or list", call = abortcall))
  }
}
