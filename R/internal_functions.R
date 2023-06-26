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
  ns <- ns[names_exported]
  ns <- ns[!is.na(names(ns))]
  names_exported <- names(ns)
  names_inops <- grep(":=|%", names_exported, value = TRUE)
  if(length(names_inops)>0) {
    for (i in names_inops){
      if(isFALSE(is.null(ns[[i]]))){
      attr(ns[[i]], which = "package") <- package
      }
    }
  }
  return(ns)
}

#' @keywords internal
#' @noRd
.internal_check_deps_overlap_any <- function(
    pkgs, lib.loc, deps_type=c("Depends", "Imports", "LinkingTo")
) {
  all_deps <- sapply(
    pkgs, pkgs_get_deps, lib.loc=lib.loc, deps_type=deps_type
  ) |> unlist() |> unname()
  check <- any(pkgs %in% all_deps)
  return(check)
}

.internal_import_namespaces <- function(pkgs, lib.loc){
  export_names_all <- character()
  export_names_allconflicts <- character()
  namespaces <- list()
  for (i in 1:length(pkgs)) {
    message(paste0("Importing package: ", pkgs[i], "..."))
    namespace_current <- .internal_prep_Namespace(pkgs[i], lib.loc)
    export_names_current <- names(namespace_current)
    
    prop.infix <- mean(grepl("%|:=", export_names_current))
    if(prop.infix >= 0.5) {
      message(paste0(
        "NOTE: Most functions in this package are infix operators;",
        "\n",
        "consider using library(", pkgs[i], ") instead."
      ))
    }
    
    export_names_intersection <- intersect(export_names_current, export_names_all)
    if(length(export_names_intersection)==0 & i>1) {
      message("no conflicts")
    }
    if(length(export_names_intersection)>0) {
      message(
        "The following conflicting objects detected:",
        "\n \n",
        paste0(export_names_intersection, collapse = ", "),
        "\n \n",
        pkgs[i], " will overwrite conflicting objects from previous imported packages..."
      )
    }
    export_names_allconflicts <- c(export_names_intersection, export_names_allconflicts)
    export_names_all <- c(export_names_current, export_names_all)
    namespaces <- utils::modifyList(namespaces, namespace_current)
    message("\n")
  }
  return(namespaces)
}

#' @keywords internal
#' @noRd
.internal_f_function_exists <- function(package, funcname) {
  tryCatch({
    utils::getFromNamespace(funcname, package)
    TRUE
  }, error = function(...) { FALSE })
}

