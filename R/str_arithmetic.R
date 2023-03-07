#' String arithmetic
#'
#'@description
#' String arithmetic operators. \cr
#' \cr
#' The \code{x %s+% y } operator is equivalent to \code{paste0(x,y)}. \cr
#' \cr
#' The \code{x %s-% p } operator removes character/pattern defined in \code{p} from \code{x}. \cr
#' \cr
#' The \code{x %s*% n } operator repeats every element of \code{x} for \code{n} times,
#' and glues them together. \cr
#' \cr
#' The \code{x %s/% p } operator counts how often regular expression or character pattern \code{p}
#' occurs in each element of \code{x}. \cr
#' \cr
#' @param x a string or character vector.
#' @param y a string, or a character vector of the same length as \code{x}.
#' @param p the result from either \code{s_pattern_b} or \code{s_pattern_stri}.
#' See \code{\link{s_pattern_b}}.
#' @param n a number, or a numeric vector of the same length as \code{x}.
#'
#'
#' @details
#' These operators and functions serve as a way to provide straight-forward string arithmetic,
#' missing from base R. \cr
#' \cr
#'
#'
#' @returns
#' The \code{%s+%}, \code{%s-%}, and \code{%s*%} operators
#' return a character vector of the same length as \code{x}. \cr
#' The \code{%s/%} returns a integer vector of the same length as \code{x}.
#'
#'
#' @examples
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' y <- "a"
#' p <- "a|e|i|o|u" # same as p <- s_pattern_b("a|e|i|o|u", fixed=FALSE, ignore.case=FALSE, perl=FALSE)
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
#' # pattern with ignore.case=TRUE:
#' p <- s_pattern_b("A|E|I|O|U", fixed=FALSE, ignore.case=TRUE, perl=FALSE)
#' n <- c(2, 3)
#'
#' x %s+% y # =paste0(x,y)
#' x %s-% p # remove all vowels from x
#' x %s*% n
#' x %s/% p # count how often vowels appears in each string of vector x.
#'
#'
#' #############################################################################
#'
#'
#' p <- s_pattern_b("\\v+", perl=TRUE) # perl expression; only works with perl=TRUE
#' x <- "line1 \n line2"
#' print(x)
#' x %s-% p # remove vertical line
#'

#' @rdname str_arithmetic
#' @export
`%s+%` <- function(x, y) { paste0(x, y) }

#' @rdname str_arithmetic
#' @export
`%s-%` <- function (x, p) {
  if(isTRUE(attr(p, "engine")=="base") | is.null(attr(p, "engine"))){
    p_attr <- s_get_pattern_attr_internal(p)
    return(mapply(function(x, p){
      gsub(
        pattern = p, replacement = "",  x = x,
        fixed = p_attr$fxdd, ignore.case = p_attr$ic, perl = p_attr$prl, useBytes = p_attr$ub
      )
    }, x, p, USE.NAMES = FALSE))
  }
  if(isTRUE(attr(p, "engine")=="stringi")){
    do.call(stringi::stri_replace_all, c(list(str=x, replacement=""), p))
  }

}

#' @rdname str_arithmetic
#' @export
`%s*%` <- function (x, n) {
  strrep(x, n)
}

#' @rdname str_arithmetic
#' @export
`%s/%` <- function(x, p) {
  if(isTRUE(attr(p, "engine")=="base") | is.null(attr(p, "engine"))){
    p_attr <- s_get_pattern_attr_internal(p)
    return(mapply(function(x, p){
      lengths(regmatches(x, gregexpr(
        p, x, fixed = p_attr$fxd, ignore.case = p_attr$ic, perl = p_attr$prl, useBytes = p_attr$ub
      )))
    }, x, p, USE.NAMES = FALSE))
  }
  if(isTRUE(attr(p, "engine")=="stringi")){
    do.call(stringi::stri_count, c(list(str=x), p))
  }
}

