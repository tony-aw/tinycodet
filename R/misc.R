#' Miscellaneous functions to help your coding etiquette
#'
#' @description
#' The \code{stricter_TrueFalse} function re-assigns the \code{T} and \code{F} values to
#' \code{NULL}, forcing the user to use \code{TRUE} and \code{FALSE}.
#' Removing the created \code{T} and \code{F} objects will restore their default behaviour. \cr
#' 
#' @returns
#' Two objects: \code{T} and \code{F}, both set to \code{NULL}.
#' Removing the created \code{T} and \code{F} objects will restore their default behaviour. \cr
#' 
#' @examples
#'
#' stricter_TrueFalse()
#'
#'
#'

#' @name misc
NULL

#' @rdname misc
#' @export
stricter_TrueFalse <- function() {
  assign("T", NULL, envir = parent.frame(1))
  assign("F", NULL, envir = parent.frame(1))
}
