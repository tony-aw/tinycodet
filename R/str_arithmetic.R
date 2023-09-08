#' String arithmetic
#'
#'@description
#' String arithmetic operators. \cr
#' \cr
#' The \code{x %s+% y } operator is exported from \code{stringi},
#' and concatenates character vectors \code{x} and \code{y}. \cr
#' \cr
#' The \code{x %s-% p } operator removes character/pattern defined in \code{p} from \code{x}. \cr
#' \cr
#' The \code{x %s*% n } operator is exported from \code{stringi},
#' and duplicates each string in \code{x} \code{n} times, and concatenates the results. \cr
#' \cr
#' The \code{x %s/% p } operator counts how often regular expression or character pattern \code{p}
#' occurs in each element of \code{x}. \cr
#' \cr
#' The \code{e1 %s$% e2} operator is exported from \code{stringi},
#' and provides access to \link[=stringi]{stri_sprintf} in the form of an infix operator. \cr
#'
#' @param x a string or character vector.
#' @param p either a list with \code{stringi} arguments (see \link{stri_rgx}),
#' or else a character vector of the same length as \code{x} with regular expressions.
#'
#'
#'
#' @returns
#' The \code{%s+%}, \code{%s-%}, and \code{%s*%} operators
#' return a character vector of the same length as \code{x}. \cr
#' The \code{%s/%} returns a integer vector of the same length as \code{x}. \cr
#' The \code{%s$%} operator returns a character vector.
#'
#'
#' @seealso [tinyoperations_strings()]
#'
#'
#' @examples
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' y <- c("a", "b")
#' p <- rep("a|e|i|o|u", 2) # same as p <- list(regex=rep("a|e|i|o|u", 2))
#' n <- c(3, 2)
#'
#' x %s+% y # =paste0(x,y)
#' x %s-% p # remove all vowels from x
#' x %s*% n
#' x %s/% p # count how often vowels appear in each string of vector x.
#'
#'
#' #############################################################################
#'
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' y <- "a"
#' # pattern that ignores case:
#' p <- list(regex=rep("A|E|I|O|U", 2), case_insensitive = TRUE)
#' n <- c(2, 3)
#'
#' x %s+% y # =paste0(x,y)
#' x %s-% p # remove all vowels from x
#' x %s*% n
#' x %s/% p # count how often vowels appears in each string of vector x.
#'


#' @name str_arithmetic
NULL

#' @importFrom stringi %s+%
#' @export
stringi::`%s+%`


#' @importFrom stringi %s*%
#' @export
stringi::`%s*%`


#' @importFrom stringi %s$%
#' @export
stringi::`%s$%`


#' @rdname str_arithmetic
#' @export
`%s-%` <- function (x, p) {
  if(isTRUE(is.list(p))){
    return(do.call(stringi::stri_replace_all, c(list(str=x, replacement=""), p)))
  }
  if(isTRUE(is.character(p))) {
    return(stringi::stri_replace_all(x, "", regex=p))
  }
  if(!isTRUE(is.list(p)) && !isTRUE(is.character(p))) {
    stop("right hand side must be a character vector or list")
  }
}

#' @rdname str_arithmetic
#' @export
`%s/%` <- function(x, p) {
  if(isTRUE(is.list(p))){
    return(do.call(stringi::stri_count, c(list(str=x), p)))
  }
  if(isTRUE(is.character(p))){
    return(stringi::stri_count(x, regex=p))
  }
  if(!isTRUE(is.list(p)) && !isTRUE(is.character(p))) {
    stop("right hand side must be a character vector or list")
  }
}

