#' Internal functions
#'
#'
#'

#' @keywords internal
#' @noRd
s_locate_internal <- function(x, i, p, fixed, ignore.case, perl, useBytes) {
  tempgreg <- gregexpr(p, x, fixed=fixed, ignore.case = ignore.case, perl = perl, useBytes = useBytes)
  tempgreg <- tempgreg[[1]]
  n.matches <- length(tempgreg)
  i[i<0] <- pmax(n.matches[i<0] - abs(i[i<0]+1), 1)
  i[i>0] <- pmin(i[i>0], n.matches[i>0])
  if(all(tempgreg == -1)) {
    out <- cbind(start=NA, end=NA, length=NA)
  }
  if(all(tempgreg != -1)){
    out <- cbind(
      start = tempgreg[i],
      end = tempgreg[i] + attr(tempgreg, which="match.length")[i] -1,
      length = attr(tempgreg, which="match.length")[i]
    )
  }
  return(out)
}

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
