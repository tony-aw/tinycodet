#' Overview of the 'tinycodet' Extension of 'stringi'
#'
#' @description
#' 
#' Virtually every programming language,
#' even those primarily focused on mathematics,
#' will at some point have to deal with strings.
#' R's atomic classes boil down to some form of either numbers or characters.
#' R's numerical functions are generally very fast.
#' But R's native string functions are somewhat slow,
#' do not have a unified naming scheme,
#' and are not as comprehensive as R's impressive numerical functions.
#' The primary R-package that fixes this is 'stringi'. \cr
#'  \cr
#' 'stringi' is arguably the fastest and most comprehensive
#' string manipulation package available at the time of writing.
#' Many string related packages fully depend on 'stringi'
#' (see its reverse-dependencies on CRAN). \cr
#' \cr
#' As string manipulation is so important to programming languages,
#' 'tinycodet' adds a little bit new functionality to 'stringi'. \cr
#' \cr
#' 'tinycodet' adds the following functions to extend 'stringi':
#'
#' * Find  \eqn{i^{th}} pattern occurrence (\link{stri_locate_ith}),
#' or \eqn{i^{th}} text boundary (\link{stri_locate_ith_boundaries}).
#' * \link[=stri_join_mat]{Concatenate a character matrix row- or column-wise}. \cr \cr
#' 
#' 'tinycodet' adds the following operators,
#' to complement the already existing 'stringi' operators:
#' 
#' * Infix operators for \link[=%s-%]{string arithmetic}.
#' * Infix operators for \link[=%sget%]{string sub-setting},
#' which get or remove the first and/or last \code{n} characters from strings.
#' * Infix operators for \link[=str_search]{detecting patterns},
#' and \link[=strfind]{strfind()<-} for locating/extracting/replacing found patterns. \cr \cr
#'
#' And finally, 'tinycodet' adds the somewhat separate
#' \link[=strcut_loc]{strcut_-functions},
#' to cut strings into pieces without removing the delimiters. \cr
#' \cr
#'
#' @section Regarding Vector Recycling in the 'stringi'-based Functions:
#' Generally speaking, vector recycling is supported as 'stringi' itself supports it also. \cr
#' There are, however, a few exceptions. \cr
#' First, matrix inputs
#' (like in \code{strcut_loc} and \link[=str_subset_ops]{string sub-setting operators}) 
#' will generally not be recycled. \cr
#' Second, the \code{i} argument in \link{stri_locate_ith} does not support vector recycling. \cr
#' Scalar recycling is virtually always supported.
#' 
#'
#' @seealso \link{tinycodet_help}, \link{s_pattern}
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
#' x %s{}% "\\d"
#'
#' # find second last digit:
#' loc <- stri_locate_ith(x, i = -2, regex = "\\d")
#' stringi::stri_sub(x, from = loc)
#'
#' # cut x into matrix of individual words:
#' mat <- strcut_brk(x, "word")
#'
#' # sort rows of matrix using the fast %row~% operator:
#' rank <- stringi::stri_rank(as.vector(mat)) |> matrix(ncol = ncol(mat))
#' sorted <- mat %row~% rank
#' sorted[is.na(sorted)] <- ""
#'
#' # join elements of every row into a single character vector:
#' stri_c_mat(sorted, margin = 1, sep = " ")
#'

#' @rdname aaa3_tinycodet_strings
#' @name aaa3_tinycodet_strings
#' @aliases tinycodet_strings
NULL
