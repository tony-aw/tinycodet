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

#' @keywords internal
#' @noRd
.internal_prep_Namespace <- function(package, lib.loc) {
  ns <- loadNamespace(package, lib.loc = lib.loc) |> as.list(all.names=TRUE, sorted=TRUE)
  names_exported <- names(ns[[".__NAMESPACE__."]][["exports"]])
  names_inops <- grep(":=|%", names_exported, value = TRUE)
  ns <- ns[names_exported]
  for (i in names_inops){
    if(isFALSE(is.null(ns[[i]]))){
      attr(ns[[i]], which = "package") <- package
    }
  }
  return(ns)
}

#' @keywords internal
#' @noRd
.internal_get_deps <- function(package, lib.loc, deps_type=c("Depends", "Imports")) {
  # based of https://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran
  dcf <- read.dcf(file.path(system.file("DESCRIPTION", package = package, lib.loc = lib.loc)))
  jj <- intersect(deps_type, colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names=FALSE)
  val <- gsub("\\s.*", "", trimws(val))
  return(val[val != "R"])
}

#' @keywords internal
#' @noRd
.internal_check_deps_overlap_any <- function(pkgs, lib.loc) {
  all_deps <- sapply(
    pkgs, .internal_get_deps, lib.loc=lib.loc, deps_type=c("Depends", "Imports")
  ) |> unlist() |> unname()
  check <- any(pkgs %in% all_deps)
  return(check)
}

#' @keywords internal
#' @noRd
.internal_f_function_exists <- function(package, funcname) {
  tryCatch({
    utils::getFromNamespace(funcname, package)
    TRUE
  }, error = function(...) { FALSE })
}
