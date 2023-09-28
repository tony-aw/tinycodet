#' The tinycodet extension of 'stringi'
#'
#' @description
#' The \code{tinycodet} R package adds some functions and operators
#' to extend the functionality of the \code{stringi} R package:
#'
#' * Infix operators for \link[=%s-%]{string arithmetic}. \cr
#' * Infix operators for \link[=%ss%]{string sub-setting}. \cr
#' * Infix operators for \link[=%s\{\}%]{detecting patterns}. \cr
#' * The \code{tinycodet} package adds additional
#' \code{stringi} functions, namely \link{stri_locate_ith}, and
#' \link{stri_join_mat} (and aliases). These functions use the same naming and argument convention as the rest of
#' the \code{stringi} functions, thus keeping your code consistent. \cr
#' * The \link[=strcut_loc]{strcut_-functions}.
#' * Most \code{stringi} pattern expressions options
#' are available for the string-pattern-related functions, when appropriate. \cr
#' * Although the functions are written in R,
#' they have been optimized to be in the same order of speed
#' as the other \code{stringi} functions.
#'
#'
#'
#' @seealso [tinycodet_help()], [stri_rgx()]
#'
#' @references Gagolewski M., \emph{stringi: Fast and portable character string processing in R}, Journal of Statistical Software 103(2), 2022, 1–59, doi:10.18637/jss.v103.i02
#'
#'
#' @examples
#'
#' # character vector:
#' x <- c("3rd 1st 2nd", "5th 4th 6th")
#' print(x)
#'
#' # detect if there are digits:
#' x %s{}% "[[:digits]]"
#'
#' # cut x into matrix of individual words:
#' x <- strcut_brk(x, "word")
#'
#' # re-order matrix using the fast %row~% operator:
#' mat <- stringi::stri_rank(as.vector(x)) |> matrix(ncol=ncol(x))
#' sorted <- x %row~% mat
#'
#' # join elements of every row into a single character vector:
#' stri_c_mat(sorted, margin=1, sep=" ")
#'

#' @rdname tinycodet_strings
#' @export
tinycodet_strings <- function() {
  utils::`?`(tinycodet_strings)
}
