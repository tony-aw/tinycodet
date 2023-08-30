#' The tinyoperations "safer" functionality
#'
#' @description
#' To help make your code safer, the \code{tinyoperations} R package
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
#' @seealso [tinyoperations_help()]
#'


#' @rdname tinyoperations_safer
#' @export
tinyoperations_safer <- function() {
  utils::`?`(tinyoperations_safer)
}
