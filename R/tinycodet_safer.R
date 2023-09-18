#' The tinycodet "safer" functionality
#'
#' @description
#' To help make your code safer, the \code{tinycodet} R package
#' introduces a few functions:
#'
#'  * \link[=%d==%]{Safer decimal (in)equality testing}.
#'  * \link[=as_int]{Atomic type casting without stripping attributes}.
#'  * The \link{lock_TF} function to
#'  set and lock \code{T} and \code{F} to \code{TRUE} and \code{FALSE}.
#'  * The \link{%<-c%} operator to assign locked constants.
#'
#'
#'
#'
#' @seealso [tinycodet_help()]
#'


#' @rdname tinycodet_safer
#' @export
tinycodet_safer <- function() {
  utils::`?`(tinycodet_safer)
}
