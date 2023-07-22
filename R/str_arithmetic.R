#' String arithmetic
#'
#'@description
#' String arithmetic operators. \cr
#' \cr
#' The \code{x %s+% y } operator is equivalent to \code{stringi::stri_c(x,y)}. \cr
#' \cr
#' The \code{x %s-% p } operator removes character/pattern defined in \code{p} from \code{x}. \cr
#' \cr
#' The \code{x %s*% n } operator repeats every element of \code{x} for \code{n} times,
#' and glues them together. \cr
#' \cr
#' The \code{x %s/% p } operator counts how often regular expression or character pattern \code{p}
#' occurs in each element of \code{x}. \cr
#' \cr
#'
#' @param x a string or character vector.
#' @param y a string, or a character vector of the same length as \code{x}.
#' @param p either a list as returned by \link{s_pattern},
#' or else a character vector of the same length as \code{x} with regular expressions.
#' @param n a number, or a numeric vector of the same length as \code{x}.
#'
#'
#' @details
#' Be aware of the precedence here! \cr
#' These are not regular arithmetic; these are functions.
#' Functions come before all arithmetic in R. \cr
#' For example, the following code: \cr
#' \code{"a" %s*% 3^2} \cr
#' is interpreted as:
#' \code{("a" %s*% 3)^2} \cr
#' which of course gives an error, since you cannot square a character. \cr
#' Therefore, put brackets around the right hand side expression
#' when using chaining arithmetic, like so: \cr
#' \code{"a" %s*% (3^2)} \cr
#' \cr
#'
#'
#' @returns
#' The \code{%s+%}, \code{%s-%}, and \code{%s*%} operators
#' return a character vector of the same length as \code{x}. \cr
#' The \code{%s/%} returns a integer vector of the same length as \code{x}.
#'
#'
#' @seealso [tinyoperations_stringi()]
#'
#'
#' @examples
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' y <- c("a", "b")
#' p <- rep("a|e|i|o|u", 2) # same as p <- s_pattern(regex=rep("a|e|i|o|u", 2))
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
#' p <- s_pattern(regex=rep("A|E|I|O|U", 2), ignore.case=TRUE)
#' n <- c(2, 3)
#'
#' x %s+% y # =paste0(x,y)
#' x %s-% p # remove all vowels from x
#' x %s*% n
#' x %s/% p # count how often vowels appears in each string of vector x.
#'


#' @name str_arithmetic
NULL

#' @rdname str_arithmetic
#' @export
`%s+%` <- function(x, y) {
  stringi::`%stri+%`(x, y)
}

#' @rdname str_arithmetic
#' @export
`%s-%` <- function (x, p) {
  if(isTRUE(is.list(p))){
    do.call(stringi::stri_replace_all, c(list(str=x, replacement=""), p))
  } else {
    stringi::stri_replace_all(x, "", regex=p)
  }
}

#' @rdname str_arithmetic
#' @export
`%s*%` <- function (x, n) {
  stringi::`%stri*%`(x, n)
}

#' @rdname str_arithmetic
#' @export
`%s/%` <- function(x, p) {
  if(isTRUE(is.list(p))){
    do.call(stringi::stri_count, c(list(str=x), p))
  } else {
    stringi::stri_count(x, regex=p)
  }
}

