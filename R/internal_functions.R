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
    "base", "compiler", "datasets", "grDevices", "graphics", "grid", "methods",
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
  return(out)
}


#' @keywords internal
#' @noRd
.internal_list_knownmeta <- function() {
  out <- c(
    "tidyverse", "fastverse", "tinyverse"
  )
  return(out)
}


#' @keywords internal
#' @noRd
.internal_list_tidyshared <- function() {
  out <- c(
    "rlang", "lifecycle", "cli", "glue", "withr"
  )
  return(out)
}


#' @keywords internal
#' @noRd
.internal_package_found <- function(pkgs, lib.loc) {
  temp.fun <- function(pkg, lib.loc) {
    if(pkg %in% .internal_list_coreR()) {
      return(find.package(pkg, lib.loc = NULL, quiet = TRUE) |> length() |> as.logical())
    }
    else {
      return(find.package(pkg, lib.loc = lib.loc, quiet = TRUE) |> length() |> as.logical())
    }
  }
  out <- vapply(
    pkgs,
    \(x)temp.fun(x, lib.loc),
    FUN.VALUE = logical(1L)
  )
  return(out)
}

#' @keywords internal
#' @noRd
.internal_is_formula <- function(form) {
  check <- inherits(form, "formula") && is.call(form) && isTRUE(form[[1]] == "~")
  return(check)
}


#' @keywords internal
#' @noRd
.internal_grep_inops <- function(nms, type, invert = FALSE) {
  if(type == 0) {
    return(stringi::stri_detect_regex(nms, "%|:=", negate = invert))
  }
  else if(type == 1) {
    return(which(stringi::stri_detect_regex(nms, "%|:=", negate = invert)))
  }
  else if(type == 2) {
    return(stringi::stri_subset_regex(nms, "%|:=", negate = invert))
  }
  else {
    stop("unknown type given")
  }
}


#' @keywords internal
#' @noRd
.internal_check_lib.loc <- function(lib.loc, abortcall) {
  if(length(lib.loc) < 1L || !is.character(lib.loc) || any(!nzchar(lib.loc))) {
    stop(simpleError(
      "`lib.loc` must be a character vector with at least one library path",
      call = abortcall
    ))
  }
}


#' @keywords internal
#' @noRd
.internal_check_pkgs <- function(
    pkgs, lib.loc, pkgs_txt = "packages", correct_pkgs = NULL, abortcall
) {
  
  
  misspelled_pkgs <- pkgs[pkgs != make.names(pkgs)]
  if(length(misspelled_pkgs) > 0) {
    error.txt <- simpleError(paste0(
      "You have misspelled the following ", pkgs_txt, ":",
      "\n",
      paste0(misspelled_pkgs, collapse = ", ")
    ), call=abortcall)
    stop(error.txt)
  }
  
  duplicate_pkgs <- pkgs[duplicated(pkgs)]
  if(length(duplicate_pkgs) > 0) {
    error.txt <- simpleError(paste0(
      "The following duplicate ", pkgs_txt, " given:",
      "\n",
      paste0(duplicate_pkgs, collapse = ", ")
    ), call=abortcall)
    stop(error.txt)
  }
  
  
  uninstalled_pkgs <- pkgs[!.internal_package_found(pkgs, lib.loc)]
  if(length(uninstalled_pkgs) > 0) {
    error.txt <- simpleError(paste0(
      "The following ", pkgs_txt, " are not installed:",
      "\n",
      paste0(uninstalled_pkgs, collapse = ", ")
    ), call=abortcall)
    stop(error.txt)
  }
  
  
  if(!is.null(correct_pkgs)) {
    wrong_pkgs <- pkgs[!(pkgs %in% correct_pkgs)]
    if(length(wrong_pkgs) > 0) {
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
.internal_check_forbidden_pkgs <- function(
    pkgs, lib.loc, pkgs_txt = "packages", abortcall
) {
  
  meta_pkgs <- pkgs[pkgs %in% .internal_list_knownmeta()]
  if(length(meta_pkgs) > 0) {
    error.txt <- paste0(
      "The following packages are known meta-verse packages, which is not allowed:",
      "\n",
      paste0(meta_pkgs, collapse = ", ")
    )
    stop(simpleError(error.txt, call = abortcall))
  }
  
  forbidden_pkgs <- pkgs[pkgs %in% .internal_list_coreR()]
  if(length(forbidden_pkgs) > 0) {
    error.txt <- paste0(
      'The following "packages" are base/core R, which is not allowed:',
      "\n",
      paste0(forbidden_pkgs, collapse = ", ")
    )
    stop(simpleError(error.txt, call = abortcall))
  }
  
}

#' @keywords internal
#' @noRd
.internal_check_ns_requirements <- function(package, lib.loc, abortcall) {
  
  pkgs_required <- pkg_get_deps(
    package, lib.loc = lib.loc,
    deps_type=c("LinkingTo", "Depends", "Imports"),
    base = FALSE, recom = TRUE, rstudioapi = TRUE, shared_tidy = TRUE)
  pkgs_total <- c(package, pkgs_required)
  pkgs_missing <- pkgs_total[!.internal_package_found(pkgs_total, lib.loc)]
  if(length(pkgs_missing) > 0) {
    error.txt <- paste0(
      "to load the namespace of package `",
      package,
      "`, the following packages are required but not installed:",
      "\n",
      paste0(pkgs_missing, collapse = ", ")
    )
    stop(simpleError(error.txt, call = abortcall))
  }
  
}


#' @keywords internal
#' @noRd
.internal_prep_Namespace <- function(package, lib.loc, abortcall) {
  
  .internal_check_ns_requirements(package, lib.loc, abortcall)
  
  ns <- loadNamespace(package, lib.loc = lib.loc) |> as.list(all.names=TRUE, sorted=TRUE)
  names_exported <- names(ns[[".__NAMESPACE__."]][["exports"]])
  ns <- ns[names_exported]
  ns <- ns[!is.na(names(ns))]
  names_exported <- names(ns)
  ind <- vapply( # get indices of `ns` that are functions but NOT primitive functions
    ns, \(x)is.function(x) && !is.primitive(x), FUN.VALUE = logical(1)
  ) |> unlist(use.names = FALSE)
  names_functions <- names(ns)[ind]
  if(length(names_functions) > 0) {
    ns <- .rcpp_prep_ns(ns, names_functions, package)
    return(ns)
  }
  if(length(names_functions) == 0) {
    warn.txt <- paste0(
      "the package `", package, "` has no exported (non-primitive) functions"
    )
    warning(simpleWarning(warn.txt, call = abortcall))
    return(list())
  }
  return(ns)
}



#' @keywords internal
#' @noRd
.internal_get_foreignexports_ns <- function(main_package, lib.loc, abortcall) {
  ns <- loadNamespace(main_package, lib.loc = lib.loc)
  names_exports <- names(ns[[".__NAMESPACE__."]][["exports"]])
  lst_imports <- ns[[".__NAMESPACE__."]][["imports"]]

  lst_imports <- lst_imports[vapply(lst_imports, is.character, logical(1))]
  pkgs <- names(lst_imports) |> unique()
  pkgs <- pkgs[!pkgs %in% "base"]
  
  if(length(pkgs) == 0) return(list())
  

  uninstalled_pkgs <- pkgs[!.internal_package_found(pkgs, lib.loc)]
  if(length(uninstalled_pkgs) > 0) {
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
    ns_i <- .internal_prep_Namespace(i, lib.loc = lib.loc, abortcall)
    names_funs <- intersect(names_funs, names(ns_i))
    ns_temp <- ns_i[names_funs]
    ns_foreign <- utils::modifyList(
      ns_foreign, ns_temp
    )
  }
  return(ns_foreign)
  
  
}


#' @keywords internal
#' @noRd
.internal_check_dependencies <- function(package, dependencies, lib.loc, abortcall) {

  actual_dependencies <- pkg_get_deps(
    package, lib.loc=lib.loc, deps_type=c("Depends", "Imports", "LinkingTo"),
    base=TRUE, recom=TRUE, rstudioapi = TRUE, shared_tidy = TRUE
  ) |> unique()

  

  .internal_check_pkgs(
    pkgs=dependencies, lib.loc=lib.loc, pkgs_txt = "dependencies",
    correct_pkgs=actual_dependencies, abortcall=abortcall
  )

}

#' @keywords internal
#' @noRd
.internal_check_extends <- function(package, extends, lib.loc, abortcall) {

  .internal_check_pkgs(
    pkgs=extends, lib.loc=lib.loc, pkgs_txt = "extensions",
    abortcall = abortcall
  )

  wrong_extends <- extends[extends %in% c(.internal_list_preinst(), .internal_list_tidyshared(), "rstudioapi")]
  if(length(wrong_extends) > 0) {
    error.txt <- simpleError(paste0(
      "The following given extensions were not found to be actual extensions:",
      "\n",
      paste0(wrong_extends, collapse = ", ")
    ), call=abortcall)
    stop(error.txt)
  }
  # checking extensions AFTER basic package checks,
  # because these packages need to be actually installed and correctly specified
  # before I can check them
  # (dependencies and enhances, on the other hand, can be checked from the main package itself)
  tempfun <- function(x){
    return(package %in% pkg_get_deps_minimal(x, lib.loc = lib.loc))
  }
  check_extends <- vapply(
    extends, FUN = tempfun, FUN.VALUE = logical(1)
  ) |> unlist(use.names = FALSE)
  wrong_extends <- extends[!check_extends]
  if(length(wrong_extends) > 0) {
    error.txt <- simpleError(paste0(
      "The following given extensions were not found to be actual extensions:",
      "\n",
      paste0(wrong_extends, collapse = ", ")
    ), call=abortcall)
    stop(error.txt)
  }
}


#' @keywords internal
#' @noRd
.internal_help.import.tempfun <- function(f, ..., abortcall) {
  fun_name <- attr(f, "function_name")
  if(is.null(fun_name)) {
    error.txt <- paste0(
      "no function name attribute found",
      "\n",
      "are you sure the function comes from `tinycodet::import_as()` or `tinycodet::import_inops()`?"
    )
    stop(simpleError(error.txt, call = abortcall))
  }
  package <- .internal_get_packagename(f)
  return(utils::help(topic = (fun_name), package = (package), ...))
}


#' @keywords internal
#' @noRd
.internal_get_packagename <- function(f) {
  f.env <- environment(f)
  package <- NULL
  if(isNamespace(f.env)) { package <- getNamespaceName(f.env) }
  if(!isNamespace(f.env)) { package <- attr(f, "package") }
  return(package)
}


