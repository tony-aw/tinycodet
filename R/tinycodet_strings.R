#' The tinycodet expansion of the 'stringi' R package
#'
#' @description
#' The \code{tinycodet} R package adds some functions and operators
#' to extend the functionality of the \code{stringi} R package:
#'
#' * Infix operators for \link[=%s-%]{string arithmetic}. \cr
#' * Infix operators for \link[=%ss%]{string sub-setting}. \cr
#' * \link[=%row~%]{Infix operators for row- and column-wise re-ordering of matrices}. \cr
#' * The \code{tinycodet} package adds additional
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
#'
#'
#' @seealso [tinycodet_help()]
#'
#' @references Gagolewski M., stringi: Fast and portable character string processing in R, Journal of Statistical Software 103(2), 2022, 1–59, doi:10.18637/jss.v103.i02
#'

#' @rdname tinycodet_strings
#' @export
tinycodet_strings <- function() {
  utils::`?`(tinycodet_strings)
}
