#' Pattern attribute assignment
#'
#'@description 
#' Some of the string arithmetic functions (\code{\link{str_arithmetic}}),
#' and some of their in-place modifier equivalents (\code{\link{inplace_str}}),
#' use regular expression for pattern matching. \cr
#' The default interpretation for this is a regular expression using: \cr
#' \code{fixed=FALSE, ignore.case=FALSE, perl=FALSE} (see \link[base]{grep}). \cr
#' \cr
#' The \code{s_pattern()} function adds attributes to the pattern \code{p},
#' thereby allowing the user to change how the pattern is interpreted by the string arithmetic functions in this package. \cr
#' To use this function simply replace replace the pattern vector/string \code{p} with \code{s_pattern(p, fixed, ignore.case, perl)} appropriately. \cr
#' \cr
#' For example: \cr
#' \code{x %s/% p } counts how often regular expression p occurs in x, \cr
#' whereas \code{x %s/% s_pattern(p, ignore.case=TRUE) } will do the same except it does not distinguish between capital/upper and lower characters.\cr
#' 
#' For examples on how to use \code{s_pattern}, see the documentation on \code{\link{str_arithmetic}} and on \code{\link{inplace_str}}.
#' More extensive examples on how to use this can be found in the vignette.
#' 
#' @param p a pattern (regular expression), or character vector of regular expressions of the same length as \code{x},
#' giving the pattern to find. \cr
#' @param fixed,perl,ignore.case see \link[base]{grep}.
#' 
#' 
#' @details
#' The \code{s_pattern()} function only works in combination with the functions and operators in this package.
#' It does not affect base R functions, nor does it affect functions from other packages such as \code{stringi}.
#' 
#' @returns
#' The \code{s_pattern(p, ...)} returns the exact same input \code{p}, except attributes are set-up. \cr
#' 
#' 
#' @examples
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- "a|e|i|o|u" # same as p <- s_pattern("a|e|i|o|u", fixed=FALSE, ignore.case=FALSE, perl=FALSE)
#' s_extract(x, -1, p) # extracts the last vowel in each element of x.
#' s_repl(x, -1, p, "?") # replace last vowel in each element of x with a question mark ("?").
#' x %s/% p # count how often vowels appear in each string of vector x.
#' 
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- s_pattern("A|E|I|O|U", fixed=FALSE, ignore.case=TRUE, perl=FALSE)
#' s_extract(x, -1, p) # extracts the last vowel in each element of x.
#' s_repl(x, -1, p, "?") # replace last vowel in each element of x with a question mark ("?").
#' x %s/% p # count how often vowels appear in each string of vector x.
#' 
#' p <- s_pattern("\\v+", perl=TRUE) # perl expression; only works with perl=TRUE
#' x <- "line1 \n line2"
#' print(x)
#' s_repl(x, 1, p, " - ") # replace vertical line break with a minus line.
#' 


#' @export
s_pattern <- function(p, fixed=FALSE, ignore.case=FALSE, perl=FALSE){
  attributes(p) <- list(fixed=fixed, ignore.case=ignore.case, perl=perl)
  return(p)
}
