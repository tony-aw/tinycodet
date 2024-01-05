#' 'stringi' Pattern Search Operators
#'
#' @description
#'
#' The \code{x %s{}% p} operator
#' checks for every string in character vector \code{x} if
#' the pattern defined in \code{p} is present. \cr
#' \cr
#' The \code{x %s!{}% p} operator
#' checks for every string in character vector \code{x} if
#' the pattern defined in \code{p} is NOT present. \cr
#' \cr
#' For string (in)equality operators, see \link[stringi]{%s==%} from the 'stringi' package. \cr
#' \cr
#' The \code{strfind()<-} method,
#' although technically not an operator,
#' is meant to complement the string-related infix operators,
#' and therefore uses the same \link{s_pattern} API as (for example)
#' the \code{%s{}% and %s!{}%} operators. \cr
#' It functions as follows:
#'  * \code{strfind()} attempts to find all pattern matches,
#' and returns the extractions of the findings in a list,
#' just like \link[stringi]{stri_extract_all}.
#'  * \code{strfind(..., i = "all")} attempts to find all pattern matches,
#' and reports the locations of the findings in a list,
#' just like \link[stringi]{stri_locate_all}.
#'  * \code{strfind(..., i = i)}, where \code{i} is a non-zero integer,
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
#' with regular expressions. \cr
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
#' # simple example ====
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
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
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
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
#'
#' #############################################################################
#' 
#' # More complex example ====
#' 
#' x <- rep('The quick brown fox jumped over the lazy dog.', 3)
#' print(x)
#' p <- c('quick', 'brown', 'fox')
#' rp <- c('slow',  'black', 'bear')
#' x %s{}% p
#' strfind(x, p)
#' strfind(x, p) <- rp
#' print(x)
#' 
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#'
#' # report ith (second and second-last) vowel locations:
#' p <- s_regex( # vowels
#'   rep("A|E|I|O|U", 2),
#'   case_insensitive=TRUE
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

#' @name str_search
NULL

#' @rdname str_search
#' @export
`%s{}%` <- function(x, p) {
  if(is.list(p)){
    return(do.call(stringi::stri_detect, c(list(str = x), p)))
  }
  if(is.character(p)) {
    return(stringi::stri_detect(x, regex=p))
  } else {
    stop("right hand side must be a character vector or list")
  }
}


#' @rdname str_search
#' @export
`%s!{}%` <- function(x, p) {
  if(is.list(p)){
    return(do.call(stringi::stri_detect, c(list(str = x, negate = TRUE), p)))
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
    if(is.list(p)){
      
      args <- list(str = x, i = i)
      return(do.call(stri_locate_ith, c(args, p, list(...))))
      
    } else if(is.character(p)) {
      
      return(stri_locate_ith(
        str = x, i = i, regex = p, ...
      ))
      
    } else {
      stop("`p` must be a character vector or list")
    }
  }
  else if(!is.null(i) && i == "all") {
    if(is.list(p)){
      return(do.call(stringi::stri_locate_all, c(list(str = x), p, list(...))))
    } else if(is.character(p)) {
      return(stringi::stri_locate_all(
        str = x, regex = p, ...
      ))
    } else {
      stop("`p` must be a character vector or list")
    }
  }
  else if(is.null(i)) {
    if(is.list(p)){
      return(do.call(stringi::stri_extract_all, c(list(str = x), p, list(...))))
    } else if(is.character(p)) {
      return(stringi::stri_extract_all(
        str = x, regex = p, ...
      ))
    } else {
      stop("`p` must be a character vector or list")
    }
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
    return(do.call(stringi::stri_replace_all, c(args, p)))
    
  } else if(is.character(p)) {
    
    return(stringi::stri_replace_all(
      str = x, replacement = value, regex = p, ...
    ))
    
  } else {
    stop("`p` must be a character vector or list")
  }
}
