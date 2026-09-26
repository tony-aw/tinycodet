#' Legacy import_ functions
#'
#' @description
#' These functions are deprecated, and will be removed in a future update. \cr \cr
#'
#' @param expose,package a single string, giving the name of the R-package.
#' @param selection a character vector of function names
#' (both regular functions and infix operators). \cr
#' Internal functions or re-exported functions are not supported.
#' @param lib.loc a character vector describing the location of R library trees to search through.
#' @param ... further arguments passed to \link{import_from} or \link{import_ls}
#'
#'
#'
#'
#' @returns
#' See \link{import_from}.
#'
#' @seealso \link{tinycodet_import}, [import_from()], [import_ls()]
#'
#'
#' @examples
#'
#' import_inops("stringi")
#' import_LL("stringi", "stri_c")
#'
#'
#'
#' @name import_legacy
NULL
#'

#' @rdname import_legacy
#' @export
import_inops <- function(expose, lib.loc = .libPaths(), ...) {
  
  warning(
  "`import_inops()` is deprecated and will be removed;
  please use `import_from(..., ls = import_ls(..., type = \"infix\"))` instead"
  )
  msg <- c("calling:",
          "`import_from(..., ls = import_ls(..., type = \"infix\"))`")
  message(paste0(msg, collapse = "\n"))
  
  exports <- import_ls(
    package = expose,
    types = "infix",
    re_exports = FALSE,
    lib.loc = lib.loc,
    print = FALSE
  )
  env <- parent.frame()
  import_from(
    package = expose,
    ls = exports,
    re_exports = FALSE,
    lock = FALSE,
    lib.loc = lib.loc,
    env = env,
    ...
  )
  
  return(invisible(NULL))
}


#' @rdname import_legacy
#' @export
import_LL <- function(package, selection, lib.loc = .libPaths()) {
  
  warning("`import_LL()` is deprecated and will be removed; please use `import_from()` instead")
  message("calling `import_from(...)`")
  
  env <- parent.frame()
  import_from(
    package = package,
    ls = selection,
    re_exports = FALSE,
    lock = TRUE,
    lib.loc = lib.loc,
    env = env
  )
  
  return(invisible(NULL))
}

