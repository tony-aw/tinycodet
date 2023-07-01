#' The tinyoperators help page
#'
#'@description
#' Welcome to the \code{tinyoperators} help page! \cr
#' \cr
#' The \code{tinyoperators} R-package adds some infix operators,
#' and a few functions. \cr
#' It primarily focuses on 4 things: \cr
#' \cr
#' (1) Float truth testing. \cr
#' (2) Reducing repetitive code. \cr
#' (3) Extending the string manipulation capabilities of the \code{stringi} R package. \cr
#' (4) A new package and module import system,
#' that combines the benefits of aliasing a package with the benefits of attaching a package. \cr
#' \cr
#' The \code{tinyoperators} R-package has only one dependency,
#' namely \code{stringi},
#' though it does allows multi-threading of some of the string-related functions
#' (when appropriate)
#' via the suggested \code{stringfish} R-package. \cr
#' Most functions in this R-package are fully vectorized
#' and have been optimized for optimal speed and performance.
#' \cr
#'
#' The \code{tinyoperators} R package adds the following functionality: \cr
#' \cr
#' * \link[=%xor%]{Infix logical operators} for exclusive-or, not-and, not-in, number-type, and string-type. \cr
#' * \link[=%f==%]{Safer (in)equality operators for floating numbers}. \cr
#' * Infix operators for \link[=%s+%]{string arithmetic}. \cr
#' * Infix operators for \link[=%ss%]{string sub-setting}. \cr
#' * Several operators for the "Don't Repeat Yourself" coding principle (\code{DRY}).
#' This includes the \link[=%:=%]{generalized in-place (mathematical) modification operator},
#' infix operators for \link[=%s+ =%]{In-place modifying string arithmetic},
#' and infix operators for \link[=%sget =%]{In-place modifying string sub-setting}. \cr
#' * \link[=%row~%]{Infix operators for row- and column-wise re-ordering of matrices}. \cr
#' * The \code{tinyoperators} package adds additional
#' \code{stringi} functions, namely \link{stri_locate_ith} and
#' \link{stri_join_mat} (and aliases).
#' These functions use the same naming and argument convention as the rest of
#' the \code{stringi} functions, thus keeping your code consistent. \cr
#' * The fully vectorized \link[=substr_repl]{sub-string functions},
#' that extract, replace, add-in, transform, or re-arrange,
#' the \eqn{i^{th}} pattern occurrence or location. \cr
#' * The \link{s_pattern} helper function for string operators. \cr
#' * \link[=import_data]{New package import functions},
#' and \link[=source_inops]{new module sourcing functions}. \cr
#' * Most \code{stringi} pattern expressions options
#' are available for the string-pattern-related functions, when appropriate. \cr
#' * This R package has only one dependency: \code{stringi}.
#' No other dependencies, as to avoid \code{"dependency hell"}. \cr
#' * Although this package has no other dependencies,
#' it allows multi-threading of the sub-string functions
#' through the \code{stringfish} R package. \cr
#' \cr
#'
#' Please also have a look at the Read-Me file on the GitHub main page before using this package: \cr
#' \url{https://github.com/tony-aw/tinyoperators}
#'
#'
#'

#' @rdname tinyoperators_help
#' @export
tinyoperators_help <- function() {
  utils::`?`(tinyoperators_help)
}
