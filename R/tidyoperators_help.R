#' The tidyoperators help page
#'
#'@description
#' Welcome to the \code{tidyoperators} help page! \cr
#' \cr
#' The 'tidyoperators' R-package adds some much needed infix operators,
#' and a few functions, to make your R code much more tidy.
#' It includes infix operators for additional logic operators,
#' safer float (in)equality operators,
#' and infix operators for custom row- and column-wise ordering of matrices.
#' It also adds some stringi-based string related functions and operators.
#' It also adds operators and a few functions to help reduce unnecessary repetitive code.
#' And finally, it also adds some functions and an operator for easier package/library management.
#' The 'tidyoperators' R-package has only one dependency, namely 'stringi',
#' though it does allows multi-threading of some of the string-related functions
#' (when appropriate)
#' via the suggested 'stringfish' R-package. \cr
#' \cr
#'
#' The \code{tidyoperators} R package adds the following functionality: \cr
#' \cr
#' * \link[=%xor%]{Infix logical operators} for exclusive-or, not-and, not-in, number-type, and string-type. \cr
#' * \link[=%f==%]{Safer (in)equality operators for floating numbers}. \cr
#' * Infix operators for \link[=%+ =%]{In-place modifiers for mathematical arithmetic}. \cr
#' * Infix operators for \link[=%s+%]{string arithmetic}. \cr
#' * Infix operators for \link[=%ss%]{string sub-setting}. \cr
#' * Infix operators for \link[=%s+ =%]{In-place modifying string arithmetic}. \cr
#' * Infix operators for \link[=%sget =%]{In-place modifying string sub-setting}. \cr
#' * \link[=%row~%]{Infix operators for row- and column-wise rank-based re-ordering of matrices}. \cr
#' * The \code{tidyoperators} package adds additional
#' \code{stringi} functions, namely \link{stri_locate_ith} and
#' \link{stri_join_mat} (and aliases).
#' These functions use the same naming and argument convention as the rest of
#' the \code{stringi} functions, thus keeping your code consistent. \cr
#' * The fully vectorized \link[=substr_repl]{sub-string functions},
#' that extract, replace, add-in, transform, or re-arrange,
#' the ith pattern occurrence or location. \cr
#' * The \link{s_pattern} helper function for string operators. \cr
#' * The \link{transform_if} function, and some related infix operators. \cr
#' * \link[=import_data]{New package import functions}. \cr
#' * Most \code{stringi} pattern expressions options
#' are available for the string-pattern-related functions, when appropriate. \cr
#' * This R package has only one dependency: \code{stringi}.
#' No other dependencies, as to avoid "dependency hell". \cr
#' * Although this package has no other dependencies,
#' it allows multi-threading of functions (when appropriate)
#' through third-party packages (like \code{stringfish}). \cr
#' \cr
#'
#' Please also have a look at the Read-Me file on the Github main page before using this package: \cr
#' \url{https://github.com/tony-aw/tidyoperators}
#'
#'
#'

#' @rdname tidyoperators_help
#' @export
tidyoperators_help <- function() {
  utils::`?`(tidyoperators_help)
}
