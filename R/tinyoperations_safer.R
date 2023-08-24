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
#' Please refer to the Read-Me file on the GitHub main page of this page for more information. \cr
#' See: \url{https://github.com/tony-aw/tinyoperations}. \cr
#'
#'
#' @seealso [tinyoperations_help()]
#'


#' @rdname tinyoperations_safer
#' @export
tinyoperations_safer <- function() {
  utils::`?`(tinyoperations_safer)
}
