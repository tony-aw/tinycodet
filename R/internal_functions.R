#' Internal functions
#'
#'
#'
#'
#'
#'

#' @keywords internal
#' @noRd
.internal_list_coreR <- function() {
  out <- c(
    "base", "compiler", "datasets", "grDevices", "graphics", "grid"," methods",
    "parallel", "splines", "stats", "stats4", "tcltk", "tools",
    "translations", "utils"
  )
  return(out)
}

#' @keywords internal
#' @noRd
.internal_list_preinst <- function() {
  out <- c(
    "boot", "class", "cluster", "codetools", "foreign", "KernSmooth",
    "lattice", "MASS", "Matrix",  "mgcv", "nlme", "nnet",
    "rpart", "spatial", "survival"
  )
}

#' @keywords internal
#' @noRd
.internal_check_lib.loc <- function(lib.loc, abortcall) {
  if(length(lib.loc) < 1 || !is.character(lib.loc) || any(!nzchar(lib.loc))) {
    stop(simpleError(
      "`lib.loc` must be a character vector with at least one library path",
      call = abortcall
    ))
  }
}

#' @keywords internal
#' @noRd
.internal_prep_Namespace <- function(package, lib.loc, abortcall) {

  pkgs_required <- pkg_get_deps(package, lib.loc = lib.loc, deps_type=c("LinkingTo", "Depends", "Imports"),
               base=FALSE, recom=TRUE, rstudioapi=TRUE)
  pkgs_total <- c(package, pkgs_required)
  pkgs_missing <- pkgs_total[!pkgs_total %installed in% lib.loc]
  if(length(pkgs_missing)>0) {
    error.txt <- paste0(
      "to load the namespace of package `",
      package,
      "`, the following packages are required but not installed:",
      "\n",
      paste0(pkgs_missing, collapse = ", ")
    )
    stop(simpleError(error.txt, call = abortcall))
  }

  ns <- loadNamespace(package, lib.loc = lib.loc) |> as.list(all.names=TRUE, sorted=TRUE)
  names_exported <- names(ns[[".__NAMESPACE__."]][["exports"]])
  ns <- ns[names_exported]
  ns <- ns[!is.na(names(ns))]
  names_exported <- names(ns)
  names_functions <- names(ns)[sapply(ns, is.function)]
  if(length(names_functions)>0) {
    for (i in names_functions){
      if(isFALSE(is.null(ns[[i]]))){
      attr(ns[[i]], which = "package") <- package
      attr(ns[[i]], which = "function_name") <- i
      attr(ns[[i]], which = "tinyimport") <- "tinyimport"
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
    pkgs, function(p)pkg_get_deps(p, lib.loc=lib.loc, deps_type=deps_type,
    base=FALSE, recom=FALSE, rstudioapi=FALSE)
  ) |> unlist() |> unname()
  check <- any(pkgs %in% all_deps)
  return(check)
}

#' @keywords internal
#' @noRd
.internal_get_foreignexports_ns <- function(main_package, lib.loc, abortcall) {
  ns <- loadNamespace(main_package, lib.loc = lib.loc) |> as.list(all.names=TRUE, sorted=TRUE)
  names_exports <- names(ns[[".__NAMESPACE__."]][["exports"]])
  lst_imports <- ns[[".__NAMESPACE__."]][["imports"]]
  pkgs_core <- .internal_list_coreR()

  lst_imports <- lst_imports[!names(lst_imports) %in% pkgs_core]
  pkgs <- names(lst_imports) |> unique()

  uninstalled_pkgs <- pkgs[!pkgs %installed in% lib.loc]
  if(length(uninstalled_pkgs)>0) {
    error.txt <- simpleError(paste0(
      "The following dependent packages (for the re-exports) are not installed:",
      "\n",
      paste0(uninstalled_pkgs, collapse = ", ")
    ), call = abortcall)
    stop(error.txt)
  }

  ns_foreign <- list()
  for (i in pkgs) {
    names_funs <- lst_imports[names(lst_imports) %in% i] |> unlist()
    names_funs <- intersect(names_exports, names_funs)
    ns_i <- .internal_prep_Namespace(i, lib.loc = lib.loc, abortcall) |> as.environment()
    names_funs <- intersect(names_funs, names(ns_i))
    ns_temp <- mget(
      names_funs, envir = ns_i,
      inherits = FALSE
    )
    ns_foreign <- utils::modifyList(
      ns_foreign, ns_temp
    )
  }
  return(ns_foreign)
}

#' @keywords internal
#' @noRd
.internal_check_pkgs <- function(pkgs, lib.loc, pkgs_txt="packages", correct_pkgs=NULL, abortcall) {

  pkgs_core <- .internal_list_coreR()

  misspelled_pkgs <- pkgs[pkgs != make.names(pkgs)]
  if(isTRUE(length(misspelled_pkgs)>0)) {
    error.txt <- simpleError(paste0(
      "You have misspelled the following ", pkgs_txt, ":",
      "\n",
      paste0(misspelled_pkgs, collapse = ", ")
    ), call=abortcall)
    stop(error.txt)
  }

  duplicate_pkgs <- pkgs[duplicated(pkgs)]
  if(isTRUE(length(duplicate_pkgs)>0)) {
    error.txt <- simpleError(paste0(
      "The following duplicate ", pkgs_txt, " given:",
      "\n",
      paste0(duplicate_pkgs, collapse = ", ")
    ), call=abortcall)
    stop(error.txt)
  }

  uninstalled_pkgs <- pkgs[!pkgs %installed in% lib.loc]
  if(isTRUE(length(uninstalled_pkgs)>0)) {
    error.txt <- simpleError(paste0(
      "The following ", pkgs_txt, " are not installed:",
      "\n",
      paste0(uninstalled_pkgs, collapse = ", ")
    ), call=abortcall)
    stop(error.txt)
  }

  forbidden_pkgs <- pkgs[pkgs %in% pkgs_core]
  if(isTRUE(length(forbidden_pkgs)>0)) {
    error.txt <- simpleError(paste0(
      'The following "packages" are base/core R, which is not allowed:',
      "\n",
      paste0(forbidden_pkgs)
    ), call=abortcall)
    stop(error.txt)
  }

  if(!is.null(correct_pkgs)) {
    wrong_pkgs <- pkgs[!pkgs %in% correct_pkgs]
    if(length(wrong_pkgs)>0) {
      error.txt <- simpleError(paste0(
        "The following given ", pkgs_txt, " were not found to be actual ", pkgs_txt, ":",
        "\n",
        paste0(wrong_pkgs, collapse = ", ")
      ), call=abortcall)
      stop(error.txt)
    }
  }

}

#' @keywords internal
#' @noRd
.internal_check_dependencies <- function(package, dependencies, lib.loc, abortcall) {

  actual_dependencies <- pkg_get_deps(
    package, lib.loc=lib.loc, deps_type=c("Depends", "Imports", "LinkingTo"),
    base=TRUE, recom=TRUE, rstudioapi = TRUE
  ) |> unique()

  if(is.character(dependencies) & length(dependencies)>0) {
    .internal_check_pkgs(
      pkgs=dependencies, lib.loc=lib.loc, pkgs_txt = "dependencies",
      correct_pkgs=actual_dependencies, abortcall=abortcall
    )
  }

  return(dependencies)
}

#' @keywords internal
#' @noRd
.internal_check_enhances <- function(package, enhances, lib.loc, abortcall) {
  actual_enhances <- pkg_get_deps(
    package, lib.loc=lib.loc, deps_type=c("Enhances"),
    base=TRUE, recom=TRUE, rstudioapi = TRUE
  ) |> unique()

  if(is.character(enhances) & length(enhances)>0) {
    .internal_check_pkgs(
      pkgs=enhances, lib.loc=lib.loc, pkgs_txt = "enhances",
      correct_pkgs = actual_enhances, abortcall=abortcall
    )
  }
  return(enhances)
}

#' @keywords internal
#' @noRd
.internal_check_extends <- function(package, extends, lib.loc, abortcall) {

  if(!is.null(extends) & is.character(extends) & length(extends)>0) {
    .internal_check_pkgs(
      pkgs=extends, lib.loc=lib.loc, pkgs_txt = "extensions",
      abortcall = abortcall
    )
    # checking extensions AFTER basic package checks,
    # because these packages need to be actually installed and correctly specified
    # before I can check them
    # (dependencies and enhances, on the other hand, can be checked from the main package itself)
    tempfun <- function(x){
      depends <- pkg_get_deps(
        x, lib.loc=lib.loc, deps_type=c("Depends", "Imports"),
        base=FALSE, recom=FALSE, rstudioapi = FALSE
      )
      return(package %in% depends)
    }
    check_extends <- sapply(
      extends, tempfun
    )
    wrong_extends <- extends[!check_extends]
    if(length(wrong_extends)>0) {
      error.txt <- simpleError(paste0(
        "The following given extensions were not found to be actual reverse dependencies:",
        "\n",
        paste0(wrong_extends, collapse = ", ")
      ), call=abortcall)
      stop(error.txt)
    }
  }
  return(extends)
}

#' @keywords internal
#' @noRd
.internal_get_packagename <- function(f) {
  f.env <- environment(f)
  if(isNamespace(f.env)) { package <- getNamespaceName(f.env) }
  if(!isNamespace(f.env)) { package <- attr(f, "package") }
  return(package)
}


