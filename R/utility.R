#' Utility operators and functions
#'
#'@description
#' The \code{env_c} function concatenates multiple environments into a single environment. \cr
#' This very handy, for example,
#' when one wants to load multiple R packages under a single alias.
#' I.e.: \cr
#' \code{fv <- env_c(loadNamespace("tidytable"), loadNamespace("data.table"), loadNamespace("collapse"))} \cr
#' \cr
#'
#'
#' @param ... environments to be concatenated.
#'
#' @returns
#' A merged environment
#'
#'



#' @rdname utility
#' @export
env_c <- function(...) {
  lst <- list(...)
  lst <- lapply(lst, as.list)
  lst <- do.call(c, lst)
  lst <- as.environment(lst)
  return(lst)
}
