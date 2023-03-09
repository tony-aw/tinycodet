#' The tidyoperators help page
#'
#'@description
#' Welcome to the \code{tidyoperators} help page! \cr
#' \cr
#' The 'tidyoperators' R-package adds some much needed infix operators,
#' and a few functions,
#' to make your R code much more tidy.
#' It includes infix operators for the negation of logical operators (exclusive-or, not-and, not-in),
#' safer float (in)equality operators,
#' in-place modifying mathematical arithmetic,
#' string arithmetic,
#' string sub-setting,
#' in-place modifying string arithmetic,
#' in-place modifying string sub-setting,
#' "which"-operators,
#' and in-place modifying unreal replacers.
#' Moreover, it includes some helper functions for more complex string arithmetic,
#' some of which are missing from popular R packages like stringi.
#' All base R expressions options (regex, fixed, coll, boundary, charclass) are available for all string-pattern-related functions, as well as all options from stringi (regex, fixed, boundary, charclass, coll).
#' This package also allows integrating third-party parallel computing packages
#' for some of its functions. \cr
#' \cr
#'
#' The \code{tidyoperators} R package adds the following functionality: \cr
#' \cr
#' * \link[=%xor%]{Infix logical operators} for exclusive-or, not-and, not-in, number-type, and string-type. \cr
#' * \link[=%f==%]{Safer (in)equality operators for floating numbers}. \cr
#' * Infix operators for \link[=%+ <-%]{In-place modifiers for mathematical arithmetic}. \cr
#' * Infix operators for \link[=%s+%]{string arithmetic}. \cr
#' * Infix operators (and a few functions) for \link[=%ss%]{string sub-setting}. \cr
#' * Infix operators for \link[=%s+ <-%]{In-place modifying string arithmetic}. \cr
#' * Infix operators for \link[=%sget <-%]{In-place modifying string sub-setting}. \cr
#' * There are also some string helper functions:
#' \link{s_pattern}, \link{s_locate_ith}, and \link{s_strapply}. \cr
#' * Infix operators for general sub-setting (\code{\link[=which_ops]{"which"-operators}}). \cr
#' * All \code{stringi} expressions options (Regex, fixed, coll, boundary, charclass)
#' are available for all string-pattern-related functions (see \link{s_pattern}). \cr
#' * This R package has only one dependency: \code{stringi}.
#' No other dependencies, as to avoid "dependency hell". \cr
#' * Although this package has no other dependencies,
#' it allows multi-threading of functions (when appropriate)
#' through third-party packages to improve efficiency. \cr
#'\cr

#' @rdname help
#' @export
tidyoperators_help <- function() {
  utils::`?`(tidyoperators_help)
}
