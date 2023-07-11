#' The tinyoperations help page
#'
#'@description
#' Welcome to the \code{tinyoperations} help page! \cr
#' \cr
#' The \code{tinyoperations} R-package adds some infix operators,
#' and a few functions. \cr
#' It primarily focuses on 4 things: \cr
#' \cr
#' (1) Safer decimal numbers ("double") truth testing; see \link{tinyoperations_decimal_truth}. \cr
#' (2) Reducing repetitive code; see \link{tinyoperations_dry}. \cr
#' (3) Extending the string manipulation capabilities of the \code{stringi} R package,
#' see \link{tinyoperations_stringi}. \cr
#' (4) A new package and module import system,
#' that combines the benefits of aliasing a package with the benefits of attaching a package,
#' see \link{tinyoperations_import} \cr
#' \cr
#'
#' And some miscellaneous functionality; see \link{tinyoperations_misc}.
#'
#' The \code{tinyoperations} R-package has only one dependency,
#' namely \code{stringi},
#' though it does allows multi-threading of some of the string-related functions
#' (when appropriate)
#' via the suggested \code{stringfish} R-package. \cr
#' Most functions in this R-package are fully vectorized
#' and have been optimized for optimal speed and performance.
#' \cr
#'
#'
#' Please also have a look at the Read-Me file on the GitHub main page before using this package: \cr
#' \url{https://github.com/tony-aw/tinyoperations}
#'
#'
#'

#' @rdname tinyoperations_help
#' @export
tinyoperations_help <- function() {
  utils::`?`(tinyoperations_help)
}
