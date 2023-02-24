#' Internal functions
#'
#'
#'
#' @export
s_repl_internal <- function(x, i, p, rp, fixed, ignore.case, perl) {
  tempgreg <- gregexpr(p, x, fixed=fixed, ignore.case = ignore.case, perl = perl)
  tempmatch <- regmatches(x, tempgreg)
  n <- length(tempmatch[[1]])
  i[i<0] <- pmax(n - abs(i+1), 1)
  i[i>0] <- pmin(i, n)
  tempmatch[[1]][i] <- rp
  regmatches(x, tempgreg) <- tempmatch
  return(x)
}

#' @export
s_extract_internal <- function(x, i, p, fixed, ignore.case, perl) {
  tempgreg <- gregexpr(p, x, fixed=fixed, ignore.case = ignore.case, perl = perl)
  tempmatch <- regmatches(x, tempgreg)
  n <- length(tempmatch[[1]])
  i[i<0] <- pmax(n - abs(i+1), 1)
  i[i>0] <- pmin(i, n)
  out <- tempmatch[[1]][i]
  return(out)
}