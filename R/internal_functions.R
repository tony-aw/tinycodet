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
    pkgs, function(p)pkgs_get_deps(p, lib.loc=lib.loc, deps_type=deps_type,
    base=FALSE, recom=FALSE)
  ) |> unlist() |> unname()
  check <- any(pkgs %in% all_deps)
  return(check)
}


# .internal_import_namespaces <- function(pkgs, lib.loc){
#   
#   conflicts.df <- data.frame(
#     package = character(length(pkgs)),
#     overwrites = character(length(pkgs))
#   )
#   
#   export_names_all <- character()
#   namespaces <- list()
#   for (i in 1:length(pkgs)) {
#     message(paste0("Importing package: ", pkgs[i], "..."))
#     namespace_current <- .internal_prep_Namespace(pkgs[i], lib.loc)
#     export_names_current <- names(namespace_current)
#     
#     export_names_intersection <- intersect(export_names_current, export_names_all)
#     conflicts.df$package[i] <- pkgs[i]
#     conflicts.df$overwrites[i] <-  paste0(export_names_intersection, collapse = ", ")
#     
#     export_names_all <- c(export_names_current, export_names_all)
#     namespaces <- utils::modifyList(namespaces, namespace_current)
#   }
#   message(paste0(capture.output(conflicts.df), collapse = "\n"))
#   return(namespaces)
# }


.internal_import_namespaces <- function(pkgs, lib.loc){
  export_names_all <- character()
  export_names_allconflicts <- character()
  namespaces <- list()
  for (i in 1:length(pkgs)) {
    namespace_current <- .internal_prep_Namespace(pkgs[i], lib.loc)
    export_names_current <- names(namespace_current)

    export_names_intersection <- intersect(export_names_current, export_names_all)
    if(i==1){
      message("Importing package: ", pkgs[i], "...")
    }
    if(length(export_names_intersection)==0 & i>1) {
      message("Importing package: ", pkgs[i], "... no conflicts")
    }
    if(length(export_names_intersection)>0) {
      message(
        "Importing package: ", pkgs[i], "... The following conflicting objects detected:",
        "\n",
        paste0(export_names_intersection, collapse = ", "),
        "\n",
        pkgs[i], " will overwrite conflicting objects from previous imported packages..."
      )
    }
    export_names_allconflicts <- c(export_names_intersection, export_names_allconflicts)
    export_names_all <- c(export_names_current, export_names_all)
    namespaces <- utils::modifyList(namespaces, namespace_current)
    message("")
  }
  return(namespaces)
}

#' @keywords internal
#' @noRd
.internal_import_as_check_depends <- function(package, depends, lib.loc) {
  
  pkgs_core <- utils::installed.packages(priority = "base") |> rownames()
  pkgs_preinst <- utils::installed.packages(priority = "recommended") |> rownames()
  
  actual_depends <- pkgs_get_deps(
    package, lib.loc=lib.loc, deps_type=c("Depends", "Imports", "LinkingTo"),
    base=TRUE, recom=TRUE
  ) |> unique()
  
  if(isFALSE(depends)) {
    depends <- NULL
  }
  
  if(isTRUE(depends)){
    depends <- setdiff(actual_depends, c(pkgs_core, pkgs_preinst))
  }
  
  if(is.character(depends) & length(depends)>0) {
    if(length(depends)!=length(unique(depends))) {
      stop("one or more duplicate dependent packages given")
    }
    
    wrong_depends <- depends[!depends %installed in% lib.loc]
    if(length(wrong_depends)>0) {
      error.txt <- paste0(
        "The following dependent packages are not installed:",
        "\n",
        paste0(wrong_depends, collapse = ", ")
      )
      stop(error.txt)
    }
    
    if(is.character(depends)) {
      wrong_depends <- depends[!depends %in% actual_depends]
      if(length(wrong_depends)>0) {
        error.txt <- paste0(
          "The following dependent packages are not actual dependencies:",
          "\n",
          paste0(wrong_depends, collapse = ", ")
        )
        stop(error.txt)
      }
    }
  }
  
  return(depends)
}

#' @keywords internal
#' @noRd
.internal_import_as_check_enhances <- function(package, enhances, lib.loc) {
  actual_enhances <- pkgs_get_deps(
    package, lib.loc=lib.loc, deps_type=c("Enhances"),
    base=TRUE, recom=TRUE
  ) |> unique()
  
  if(is.character(enhances) & length(enhances)>0) {
    if(length(enhances)!=length(unique(enhances))) {
      stop("one or more duplicate enhances given")
    }
    
    wrong_enhances <- enhances[!enhances %installed in% lib.loc]
    if(length(wrong_enhances)>0) {
      error.txt <- paste0(
        "The following enhances are not installed:",
        "\n",
        paste0(wrong_enhances, collapse = ", ")
      )
      stop(error.txt)
    }
    
    wrong_enhances <- enhances[!enhances %in% actual_enhances]
    if(length(wrong_enhances)>0) {
      error.txt <- paste0(
        "The following dependent packages are not in LinkingTo:",
        "\n",
        paste0(wrong_enhances, collapse = ", ")
      )
      stop(error.txt)
    }
  }
  return(enhances)
}

#' @keywords internal
#' @noRd
.internal_import_as_check_extends <- function(package, extends, lib.loc) {
  if(!is.null(extends) & is.character(extends) & length(extends)>0) {
    if(length(extends)!=length(unique(extends))) {
      stop("one or more duplicate extension packages given")
    }
    
    wrong_extends <- extends[!extends %installed in% lib.loc]
    if(length(wrong_extends)>0) {
      error.txt <- paste0(
        "The following extensions are not installed:",
        "\n",
        paste0(wrong_extends, collapse = ", ")
      )
      stop(error.txt)
    }
    tempfun <- function(x){
      depends <- pkgs_get_deps(
        x, lib.loc=lib.loc, deps_type=c("Depends", "Imports"),
        base=FALSE, recom=FALSE
      )
      return(package %in% depends)
    }
    check_extends <- sapply(
      extends, tempfun
    )
    wrong_extends <- extends[!check_extends]
    if(length(wrong_extends)>0) {
      error.txt <- paste0(
        "The following extensions were not found to be actual reverse dependencies:",
        "\n",
        paste0(wrong_extends, collapse = ", ")
      )
      stop(error.txt)
    }
  }
  return(extends)
}



#' @keywords internal
#' @noRd
.internal_f_function_exists <- function(package, funcname) {
  tryCatch({
    utils::getFromNamespace(funcname, package)
    TRUE
  }, error = function(...) { FALSE })
}

