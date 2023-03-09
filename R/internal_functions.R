#' Internal functions
#'
#'
#'

#' @keywords internal
#' @noRd
s_get_pattern_attr_internal <- function(p) {
  fxd <- ic <- prl <- ub <- FALSE
  if(!is.null(attr(p, "fixed"))){fxd <- attr(p, "fixed")}
  if(!is.null(attr(p, "ignore.case"))){ic <- attr(p, "ignore.case")}
  if(!is.null(attr(p, "perl"))){prl <- attr(p, "perl")}
  if(!is.null(attr(p, "useBytes"))) {ub <- attr(p, "useBytes")}
  return(list(fxd=fxd, ic=ic, prl=prl, ub=ub))
}


f_function_exists <- function(package, funcname) {
  tryCatch({
    utils::getFromNamespace(funcname, package)
    TRUE
  }, error = function(...) { FALSE })
}
