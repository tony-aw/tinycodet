#' Pattern attribute assignment
#'
#'@description
#' Some of the string arithmetic and string subsetting operators and functions,
#' and some of their in-place modifier equivalents,
#' in the tidyoperators R package,
#' use regular expression for pattern matching. \cr
#' The default interpretation for this is a regular expression using: \cr
#' \code{fixed=FALSE, ignore.case=FALSE, perl=FALSE, useBytes=FALSE} (see \link[base]{grep}). \cr
#' \cr
#' The \code{s_pattern_b()} function adds attributes to the pattern \code{p},
#' thereby allowing the user to change how the pattern is interpreted
#' by the string arithmetic functions in this package. \cr
#' To use this function, simply replace replace the pattern vector/string \code{p}
#' with \code{s_pattern_b(p, fixed, ignore.case, perl, useBytes)} appropriately. \cr
#' \cr
#' For example: \cr
#' \code{x %s/% p } counts how often regular expression p occurs in x, \cr
#' whereas \code{x %s/% s_pattern_b(p, ignore.case=TRUE) } will do the same,
#' except it does not distinguish between upper case and lower case characters.\cr
#'
#' The \code{s_pattern_b()} function uses base R for pattern matching.
#' Although the \code{tidyoperators} R package has zero dependencies,
#' it does allow using pattern matching with \code{stringi},
#' provided that the user has the \code{stringi (version 1.7.12+)} R package installed.
#' To use pattern matching with \code{stringi} via this R package,
#' use \code{s_pattern_stri()} instead of \code{s_pattern_b()}. \cr
#' \cr
#' The \code{s_pattern_stri()} function uses the exact same argument convention
#' as \code{stringi} itself; i.e.:\cr
#' \code{s_pattern_stri(regex=p, case_insensitive=FALSE, ...)} \cr
#' \code{s_pattern_stri(fixed=p, ...)} \cr
#' \code{s_pattern_stri(coll=p, ...)} \cr
#' \code{s_pattern_stri(boundary=p, ...)} \cr
#' \code{s_pattern_stri(charclass=p, ...)} \cr
#' \cr
#' All arguments in \code{s_pattern_stri()} are simply passed to the
#' appropriate functions in \code{stringi}.
#' (So even if \code{stringi}'s argument convention changes in the future,
#' the \code{s_pattern_stri()} function should still work.) \cr
#' For example: \cr
#' \code{x %s/% p } counts how often regular expression p occurs in x, \cr
#' whereas \code{x %s/% s_pattern_stri(regex=p, case_insensitive=TRUE) } will do the same,
#' except it will use \code{stringi::stri_count()} in the background,
#' and does not distinguish between upper case and lower case characters.\cr
#'
#'
#'
#'
#' @param p a pattern (regular expression),
#' or character vector of regular expressions of the same length as \code{x},
#' giving the pattern to find. \cr
#' @param fixed,perl,ignore.case,useBytes see \link[base]{grep}.
#' @param ... pass \code{stringi} arguments here.
#' I.e. \code{regex=p}, \code{boundary=p}, \code{coll=p}, \code{charclass=p}, \code{case_insensitive=FALSE}, etc.
#' See the documentation in the \code{stringi} R package.
#'
#'
#' @details
#' The \code{s_pattern_b()} function only works in combination with the functions and operators in this package.
#' It does not affect base R functions.
#'
#' Using \code{s_pattern_stri} will not only change the interpretation of pattern expressions,
#' it can actually improve the speed of the code also, as using this pattern
#' will actually make the \code{tidyoperators} functions and operators internally
#' call functions from \code{stringi}, whose functions are sometimes a bit
#' faster than base R functions.
#'
#' @returns
#' The \code{s_pattern_b(p, ...)} call returns the exact same input \code{p},
#' except attributes are set-up. \cr
#' \cr
#' The \code{s_pattern_str(...)} call returns a list with arguments that will be
#' passed to the appropriate functions in \code{stringi}. \cr
#'
#'
#' @examples
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- "a|e|i|o|u" # same as p <- s_pattern_b("a|e|i|o|u", fixed=FALSE, ignore.case=FALSE, perl=FALSE, useBytes=FALSE)
#' s_extract(x, -1, p) # extracts the last vowel in each element of x.
#' s_repl(x, -1, p, "?") # replace last vowel in each element of x with a question mark ("?").
#' x %s/% p # count how often vowels appear in each string of vector x.
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- s_pattern_b("A|E|I|O|U", fixed=FALSE, ignore.case=TRUE, perl=FALSE)
#' s_extract(x, -1, p) # extracts the last vowel in each element of x.
#' s_repl(x, -1, p, "?") # replace last vowel in each element of x with a question mark ("?").
#' x %s/% p # count how often vowels appear in each string of vector x.
#'
#' p <- s_pattern_b("\\v+", perl=TRUE) # perl expression; only works with perl=TRUE
#' x <- "line1 \n line2"
#' print(x)
#' s_repl(x, 1, p, " - ") # replace vertical line break with a minus line.
#'


#' @rdname s_pattern
#' @export
s_pattern_b <- function(p, fixed=FALSE, ignore.case=FALSE, perl=FALSE, useBytes=FALSE){
  attributes(p) <- list(engine="base", fixed=fixed, ignore.case=ignore.case, perl=perl, useBytes=useBytes)
  return(p)
}

#' @rdname s_pattern
#' @export
s_pattern_stri <- function(...) {
  out <- list(...)
  attr(out, "engine") <- "stringi"
  return(out)
}


