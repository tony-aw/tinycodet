#' Internal functions
#'
#'
#'
#' @keywords internal
#' @noRd
s_repl_internal <- function(x, i, p, rp, fixed, ignore.case, perl, useBytes) {
  tempgreg <- gregexpr(p, x, fixed=fixed, ignore.case = ignore.case, perl = perl, useBytes = useBytes)
  tempmatch <- regmatches(x, tempgreg)
  n <- length(tempmatch[[1]])
  i[i<0] <- pmax(n - abs(i+1), 1)
  i[i>0] <- pmin(i, n)
  tempmatch[[1]][i] <- rp
  regmatches(x, tempgreg) <- tempmatch
  return(x)
}

#' @keywords internal
#' @noRd
s_extract_internal <- function(x, i, p, fixed, ignore.case, perl, useBytes) {
  tempgreg <- gregexpr(p, x, fixed=fixed, ignore.case = ignore.case, perl = perl, useBytes = useBytes)
  tempmatch <- regmatches(x, tempgreg)
  n <- length(tempmatch[[1]])
  i[i<0] <- pmax(n - abs(i+1), 1)
  i[i>0] <- pmin(i, n)
  out <- tempmatch[[1]][i]
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
