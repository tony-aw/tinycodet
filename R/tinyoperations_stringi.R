#' The tinyoperations expansion of the 'stringi' R package
#'
#'@description
#' The \code{tinyoperations} R package adds some functions and operators
#' to extend the functionality of the \code{stringi} R package:
#'
#' * Infix operators for \link[=%s-%]{string arithmetic}. \cr
#' * Infix operators for \link[=%ss%]{string sub-setting}. \cr
#' * Infix operators for \link[=%s+ =%]{In-place modifying string arithmetic}.
#' * Infix operators for \link[=%sget =%]{In-place modifying string sub-setting}.
#' * \link[=%row~%]{Infix operators for row- and column-wise re-ordering of matrices}. \cr
#' * The \code{tinyoperations} package adds additional
#' \code{stringi} functions, namely \link{stri_locate_ith}, and
#' \link{stri_join_mat} (and aliases). These functions use the same naming and argument convention as the rest of
#' the \code{stringi} functions, thus keeping your code consistent. \cr
#' * The \link[=strcut_loc]{strcut_-functions}.
#' * Most \code{stringi} pattern expressions options
#' are available for the string-pattern-related functions, when appropriate. \cr
#' * This R package has only one dependency: \code{stringi}.
#' No other dependencies, as to avoid \code{"dependency hell"}. \cr
#' * Although the functions are written in R,
#' they have been aggressively optimized to be in the same order of speed
#' as the other \code{stringi} functions.

#'
#'
#' Please also have a look at the Read-Me file on the GitHub main page before using this package: \cr
#' \url{https://github.com/tony-aw/tinyoperations}
#'
#'
#'
#' @seealso [tinyoperations_help()]
#'
#'

#' @rdname tinyoperations_stringi
#' @export
tinyoperations_stringi <- function() {
  utils::`?`(tinyoperations_stringi)
}
