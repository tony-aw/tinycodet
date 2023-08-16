#' The tinyoperations help page
#'
#'@description
#' Welcome to the \code{tinyoperations} introduction help page! \cr
#' \cr
#' The \code{tinyoperations} R-package
#' adds adds some functions and infix operators to help in your programming etiquette. \cr
#' It primarily focuses on 4 things: \cr
#' \cr
#' (1) Safer decimal numbers ("double") truth testing (see \link{%d==%}). \cr
#' (2) Extending the string manipulation capabilities of the \code{stringi} R package,
#' see \link{tinyoperations_strings}. \cr
#' (3) Reducing repetitive code; see \link{tinyoperations_dry}. \cr
#' (4) A new package and module import system,
#' that combines the benefits of aliasing a package with the benefits of attaching a package,
#' see \link{tinyoperations_import} \cr
#' \cr
#'
#' And some miscellaneous functionality; see \link{tinyoperations_misc}.
#'
#' The \code{tinyoperations} R-package has only one dependency,
#' namely \code{stringi}.
#' Most functions in this R-package are fully vectorized
#' and optimized.
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
