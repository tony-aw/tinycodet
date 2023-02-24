#' In place modifying string arithmetic
#'
#' @description
#' In-place modifier versions of string arithmetic: \cr
#'
#' \code{x %s+ <-% y } is the same as \code{ x <- x %s+% y} \cr
#' \cr
#' \code{x %s- <-% p } is the same as \code{ x <- x %s-% p} \cr
#' \cr
#' \code{x %s* <-% n } is the same as \code{ x <- x %s*% n} \cr
#' \cr
#' \code{x %s/ <-% p } is the same as \code{ x <- x %s/% p} \cr
#' \cr
#'
#' See also the documentation on string arithmetic (\code{\link{str_arithmetic}}). \cr
#' Note that there is no in-place modifier versions of \code{%ss%}, \code{s_extract()}, and \code{s_repl()}.\cr
#' \cr
#' The pattern \code{p} is by default understood to be characters or regular expressions evaluated with the default arguments: \cr
#' {fixed=FALSE, ignore.case=FALSE, perl=FALSE}. \cr
#' To change this, simply replace \code{p} with \code{s_pattern(p, fixed, ignore.case, perl)}. \cr
#' For example, \code{x %s/ <-% p } counts how often regular expression p occurs in x, \cr
#' whereas \code{x %s/ <-% s_pattern(p, ignore.case=TRUE) } will do the same except it does not distinguish between capital/upper and lower characters.\cr
#' See \code{\link{str_arithmetic}} and \code{\link{s_pattern}} for details.
#'
#' @param x,y,p,n see \code{\link{str_arithmetic}}.
#' @param fixed,perl,ignore.case see \link[base]{grep}.
#'
#' @return
#' These operators do not return any value: they are in-place modifiers, and thus modify \code{x} directly.
#'
#' @examples
#' y <- "a"
#' p <- "a|e|i|o|u" # same as p <- s_pattern("a|e|i|o|u", fixed=FALSE, ignore.case=FALSE, perl=FALSE)
#' n <- c(2, 3)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s+ <-% y # same as x <- x %s+% y
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s- <-% p # same as x <- x %s-% p
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s* <-% n # same as x <- x %s\*% n
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s/ <-% p # same as x <- x %s/% p
#' print(x)
#'
#'
#' #############################################################################
#'
#'
#' y <- "a"
#' # pattern with ignore.case=TRUE:
#' p <- s_pattern("A|E|I|O|U", fixed=FALSE, ignore.case=TRUE, perl=FALSE)
#' n <- c(3, 2)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s+ <-% y # same as x <- x %s+% y
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s- <-% p # same as x <- x %s-% p
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s* <-% n # same as x <- x %s\*% n
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s/ <-% p # same as x <- x %s/% p
#' print(x)
#'

#' @rdname inplace_str_arithmetic
#' @export
`%s+ <-%` <- function(x, y) {
  eval(call("<-", substitute(x), x %s+% y), envir = parent.frame(n = 1))
}

#' @rdname inplace_str_arithmetic
#' @export
`%s- <-%` <- function(x, p) {
  eval(call("<-", substitute(x), x %s-% p), envir = parent.frame(n = 1))
}

#' @rdname inplace_str_arithmetic
#' @export
`%s* <-%` <- function(x, n) {
  eval(call("<-", substitute(x), x %s*% n), envir = parent.frame(n = 1))
}

#' @rdname inplace_str_arithmetic
#' @export
`%s/ <-%` <- function(x, p) {
  eval(call("<-", substitute(x), x %s/% p), envir = parent.frame(n = 1))
}

