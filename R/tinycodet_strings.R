#' Overview of the 'tinycodet' Extension of 'stringi'
#'
#' @description
#' Virtually every programming language, even those primarily focused on mathematics,
#' will at some point have to deal with strings.
#' R's atomic classes basically boil down to some form of either numbers or characters.
#' R's numerical functions are generally very fast.
#' But R's native string functions are somewhat slow,
#' do not have a unified naming scheme,
#' and are not as comprehensive as R's impressive numerical functions. \cr
#' \cr
#' The primary R-package that fixes this is 'stringi'.
#' 'stringi' is
#' the fastest and most comprehensive
#' string manipulation package available at the time of writing.
#' Many string related packages fully depend on 'stringi'.
#' The 'stringr' package, for example, is merely a thin wrapper around 'stringi'. \cr
#' \cr
#' As string manipulation is so important to programming languages,
#' 'tinycodet' adds a little bit new functionality to 'stringi':
#'
#' * Find  \eqn{i^{th}} pattern occurrence (\link{stri_locate_ith}),
#' or \eqn{i^{th}} text boundary (\link{stri_locate_ith_boundaries}).
#' * \link[=stri_join_mat]{Concatenate a character matrix row- or column-wise }.
#' * Cut strings with the \link[=strcut_loc]{strcut_-functions}.
#' * Infix operators for \link[=%s-%]{string arithmetic}.
#' * Infix operators for \link[=%sget%]{string sub-setting},
#' which get or remove the first and/or last \code{n} characters from strings.
#' * Infix operators for \link[=%s\{\}%]{detecting patterns}.
#'
#'
#'
#'
#' @seealso [tinycodet_help()], [s_regex()]
#'
#' @references Gagolewski M., \bold{stringi}: Fast and portable character string processing in R, \emph{Journal of Statistical Software} 103(2), 2022, 1–59, \doi{doi:10.18637/jss.v103.i02}
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
