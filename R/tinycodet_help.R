#' The tinycodet help page
#'
#'
#' @description
#'
#' Welcome to the \code{tinycodet} introduction help page! \cr
#' \cr
#' The 'tinycodet' R-package is a tiny little R package that adds some functions to help in your coding etiquette. \cr
#' It primarily focuses on 4 things: \cr
#' \cr
#' (1) Safer decimal (in)equality testing, safer atomic conversions, and other functions for safer coding; \cr
#' see \link{tinycodet_safer}. \cr
#' \cr
#' (2) A new package import system,
#' that combines the benefits of aliasing a package with the benefits of attaching a package; \cr
#' see \link{tinycodet_import} \cr
#' \cr
#' (3) Extending the string manipulation capabilities of the \code{stringi} R package; \cr
#' see \link{tinycodet_strings}. \cr
#' \cr
#' (4) Reducing repetitive code; \cr
#' see \link{tinycodet_dry}. \cr
#' \cr
#' And some miscellaneous functionality; see \link{tinycodet_misc}. \cr
#' \cr
#'
#' The \code{tinycodet} R-package has only one dependency,
#' namely \code{stringi}. No other dependencies, thus avoiding "dependency hell".
#' Most functions in this R-package are fully vectorized
#' and optimized, and have been well documented.
#' \cr
#'
#'
#' Please also have a look at the GitHub page before using this package: \cr
#' \url{https://github.com/tony-aw/tinycodet}
#'
#'
#'
#' @references The badges shown in the documentation of this R package were made using the services of: \url{https://shields.io/}
#'

#' @rdname tinycodet_help
#' @export
tinycodet_help <- function() {
  utils::`?`(tinycodet_help)
}
