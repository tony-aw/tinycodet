#' Overview of the tinycodet Extension of 'stringi'
#'
#' @description
#' The 'tinycodet' R-package adds some functions and operators
#' to extend the functionality of the 'stringi' R-package:
#'
#' * Infix operators for \link[=%s-%]{string arithmetic}. \cr
#' * Infix operators for \link[=%sget%]{string sub-setting}. \cr
#' * Infix operators for \link[=%s\{\}%]{detecting patterns}. \cr
#' * Find  \eqn{i^{th}} pattern occurrence (\link{stri_locate_ith}),
#' or \eqn{i^{th}} text boundary (\link{stri_locate_ith_boundaries}). \cr
#' * \link[=stri_join_mat]{Concatenate a character matrix row- or column-wise }. \cr
#' * The \link[=strcut_loc]{strcut_-functions}.
#' * Although the functions are written in R,
#' they have been optimized to be in somewhat the same order of speed
#' as the other 'stringi' functions.
#'
#'
#'
#' @seealso [tinycodet_help()], [stri_rgx()]
#'
#' @references Gagolewski M., \emph{stringi: Fast and portable character string processing in R}, Journal of Statistical Software 103(2), 2022, 1–59, \doi{doi:10.18637/jss.v103.i02}
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
#' @name tinycodet_strings
NULL
