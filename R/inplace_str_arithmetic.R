#' In place modifying string arithmetic
#'
#' @description
#' In-place modifier versions of string arithmetic: \cr
#'
#' \code{x %s+ =% y } is the same as \code{ x <- x %s+% y} \cr
#' \cr
#' \code{x %s- =% p } is the same as \code{ x <- x %s-% p} \cr
#' \cr
#' \code{x %s* =% n } is the same as \code{ x <- x %s*% n} \cr
#' \cr
#' \code{x %s/ =% p } is the same as \code{ x <- x %s/% p} \cr
#' \cr
#'
#' See also the documentation on string arithmetic: \link[=%s+%]{string arithmetic}. \cr
#'
#' Some of the internal code of these operators was inspired by the \code{roperators} R package.
#'
#'
#' @param x,y,p,n see \link[=%s+%]{string arithmetic} and \link{s_pattern}.
#'
#' @return
#' These operators do not return any value: they are in-place modifiers, and thus modify \code{x} directly.
#'
#' @seealso [tinyoperations_dry()] [tinyoperations_stringi()]
#'
#'
#' @references Wiseman B, Nydick S, Jones J (2022). roperators: Additional Operators to Help you Write Cleaner R Code. \url{https://CRAN.R-project.org/package=roperators}
#'
#' @examples
#' y <- "a"
#' p <- "a|e|i|o|u"
#' n <- c(2, 3)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s+ =% y # same as x <- x %s+% y
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s- =% p # same as x <- x %s-% p
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s* =% n # same as x <- x %s\*% n
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s/ =% p # same as x <- x %s/% p
#' print(x)
#'
#'
#' #############################################################################
#'
#'
#' y <- "a"
#' # pattern with ignore.case=TRUE:
#' p <- s_pattern(regex = "A|E|I|O|U", ignore.case=TRUE)
#' n <- c(3, 2)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s+ =% y # same as x <- x %s+% y
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s- =% p # same as x <- x %s-% p
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s* =% n # same as x <- x %s\*% n
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s/ =% p # same as x <- x %s/% p
#' print(x)
#'

#' @name inplace_str_arithmetic
NULL

#' @rdname inplace_str_arithmetic
#' @export
`%s+ =%` <- function(x, y) {
  eval(call("<-", substitute(x), x %s+% y), envir = parent.frame(n = 1))
}

#' @rdname inplace_str_arithmetic
#' @export
`%s- =%` <- function(x, p) {
  eval(call("<-", substitute(x), x %s-% p), envir = parent.frame(n = 1))
}

#' @rdname inplace_str_arithmetic
#' @export
`%s* =%` <- function(x, n) {
  eval(call("<-", substitute(x), x %s*% n), envir = parent.frame(n = 1))
}

#' @rdname inplace_str_arithmetic
#' @export
`%s/ =%` <- function(x, p) {
  eval(call("<-", substitute(x), x %s/% p), envir = parent.frame(n = 1))
}
