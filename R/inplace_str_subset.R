#' In place modifying string subsetting
#'
#' @description
#' In-place modifier versions of string subsetting: \cr
#' \cr
#' \code{x %sget <-% ss } is the same as \code{ x <- x %sget% ss} \cr
#' \cr
#' \code{x %strim <-% ss } is the same as \code{ x <- x %strim% ss} \cr
#' \cr
#'
#' See also the documentation on string subsetting (\code{\link{str_subset}}). \cr
#' Note that there is no in-place modifier versions of \code{%ss%}, \code{s_extract()}, and \code{s_repl()}.\cr
#' \cr
#'
#' @param x,ss,fun,i,rp see \code{\link{str_subset}}.
#' @param fixed,perl,ignore.case see \link[base]{grep}.
#'
#' @return
#' These operators do not return any value: they are in-place modifiers, and thus modify \code{x} directly.
#'
#' @examples
#' p <- "a|e|i|o|u" # same as p <- s_pattern("a|e|i|o|u", fixed=FALSE, ignore.case=FALSE, perl=FALSE)
#' ss <- c(2,2)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %sget <-% ss # same as x <- x %sget% ss
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %strim <-% ss # same as x <- x %strim% ss
#' print(x)
#'
#'
#' #############################################################################
#'
#'
#' # pattern with ignore.case=TRUE:
#' p <- s_pattern("A|E|I|O|U", fixed=FALSE, ignore.case=TRUE, perl=FALSE)
#' ss <- c(2,2)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %sget <-% ss # same as x <- x %sget% ss
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %strim <-% ss # same as x <- x %strim% ss
#' print(x)
#'

#' @rdname inplace_str_subset
#' @export
`%sget <-%` <- function(x, ss) {
  eval(call("<-", substitute(x), x %sget% ss), envir = parent.frame(n = 1))
}

#' @rdname inplace_str_subset
#' @export
`%strim <-%` <- function(x, ss) {
  eval(call("<-", substitute(x), x %strim% ss), envir = parent.frame(n = 1))
}


