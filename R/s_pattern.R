#' Pattern attribute assignment
#'
#'@description
#'
#'
#' The \code{%s-%} and \code{%s/%} operators, their in-place equivalents,
#' as well as the \code{%sgrep%} operator,
#' all perform pattern matching for some purpose.
#' By default the pattern matching is interpreted as case-sensitive
#' \code{regex} patterns from `stringi`.
#'
#' The \code{s_pattern} function allows the user to
#' specify exactly how the pattern should be interpreted.
#' To use more refined pattern definition,
#' simply replace the right-hand-side expression \code{p}
#' in the relevant operators with a call from
#' the \code{s_pattern()} function.
#'
#' The \code{s_pattern()} function uses the exact same argument convention as `stringi`. For example:
#'
#' * \code{s_pattern(regex=p, case_insensitive=FALSE, ...)}
#' * \code{s_pattern(fixed=p, ...)}
#' * \code{s_pattern(coll=p, ...)}
#' * \code{s_pattern(boundary=p, ...)}
#' * \code{s_pattern(charclass=p, ...)}
#'
#' All arguments in \code{s_pattern()} are simply passed to the
#' appropriate functions in \code{stringi}. \cr
#' For example: \cr
#' \code{x %s/% p } counts how often regular expression p occurs in x, \cr
#' whereas \code{x %s/% s_pattern(fixed=p, case_insensitive=TRUE) } will do the same,
#' except it uses fixed (i.e. literal) expression,
#' and it does not distinguish between upper case and lower case characters. \cr
#' For consistency with base R and with packages such as \code{stringr},
#' one can also fill in \code{ignore.case=TRUE} or \code{ignore_case=TRUE}
#' instead of \code{case_insensitive=TRUE},
#' and \code{s_pattern} will still understand that.
#'
#'
#'
#'
#' @param ... pass \code{stringi} arguments here.
#' I.e. \code{regex=p}, \code{boundary=p}, \code{coll=p}, \code{charclass=p}, \code{case_insensitive=FALSE}, etc.
#' See the documentation in the \code{stringi} R package.
#'
#'
#' @details
#' The \code{s_pattern()} function only works in combination with the functions and operators in this package.
#' It does not affect functions from base R or functions from other packages.
#'
#' @returns
#' The \code{s_pattern(...)} call returns a list with arguments that will be
#' passed to the appropriate functions in \code{stringi}. \cr
#'
#'
#' @examples
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- rep("a|e|i|o|u", 2) # same as p <- s_pattern(regex=rep("a|e|i|o|u", 2))
#' x %s/% p # count how often vowels appear in each string of vector x.
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- s_pattern(regex=rep("A|E|I|O|U", 2), ignore.case=TRUE)
#' x %s/% p # count how often vowels appear in each string of vector x.
#'


#' @rdname s_pattern
#' @export
s_pattern <- function(...) {
  out <- list(...)
  check.names <- c(
    "regex" %in% names(out),
    "fixed" %in% names(out),
    "coll" %in% names(out),
    "boundary" %in% names(out),
    "charclass" %in% names(out)
  )
  if(sum(check.names)!=1){
    stop("you have to specify either `regex`, `fixed`, `coll`, `boundary`, or `charclass`")
  }
  indx.ic <- which(names(out)=="ignore.case"|names(out)=="ignore_case")
  if(length(indx.ic)==1){
    names(out)[indx.ic] <- "case_insensitive"
  }
  if(length(indx.ic)>1){
    stop("multiple synonyms for argument `case_insensitive` given; only one can be given")
  }
  attr(out, "engine") <- "stringi"
  return(out)
}


