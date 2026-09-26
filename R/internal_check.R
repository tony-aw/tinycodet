

#' @keywords internal
#' @noRd
.check_pkgenv <- function(env, abortcall) {
  env <- topenv(env)
  if(isNamespace(env)) {
    caller <- methods::getPackageName(env, create = FALSE)
    if(any(!caller %in% "tinycodet")) {
      txt <- "`import` functions should not be used inside R-packages!"
      stop(simpleError(txt, call = abortcall))
    }
  }
  
}


#' @keywords internal
#' @noRd
.check_pkgs <- function(
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
      "The following ", pkgs_txt, " are not installed in the specified `lib.loc`:",
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
.check_forbidden_pkgs <- function(
    pkgs, lib.loc, pkgs_txt = "packages", abortcall
) {
  
  meta_pkgs <- pkgs[pkgs %in% .list_knownmeta()]
  if(length(meta_pkgs) > 0) {
    error.txt <- paste0(
      "The following packages are known meta-verse packages, which is not allowed:",
      "\n",
      paste0(meta_pkgs, collapse = ", ")
    )
    stop(simpleError(error.txt, call = abortcall))
  }
  
  forbidden_pkgs <- pkgs[pkgs %in% .list_coreR()]
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
.check_dependencies <- function(package, dependencies, lib.loc, abortcall) {

  actual_dependencies <- pkg_get_deps(
    package, lib.loc=lib.loc, deps_type=c("Depends", "Imports", "LinkingTo"),
    base=TRUE, recom=TRUE, semi = TRUE, shared_tidy = TRUE
  ) |> unique()

  

  .check_pkgs(
    pkgs=dependencies, lib.loc=lib.loc, pkgs_txt = "dependencies",
    correct_pkgs=actual_dependencies, abortcall=abortcall
  )

}



#' @keywords internal
#' @noRd
.check_import_post <- function(pkgs, lib.loc, abortcall) {
  loaded_pkgs <- .import_ls_loaded_deps(pkgs)
  names(loaded_pkgs) <- loaded_pkgs
  
  check <- .import_diagnose(loaded_pkgs, lib.loc)
  
  # check for packages not even installed in lib.loc at all:
  missing_pkgs <- check$package[!check$installed_in_lib.loc]
  if(length(missing_pkgs)) {
    txt <- c(
      "The following packages/dependencies do NOT exist in `lib.loc`, but were already loaded BEFORE import started:",
      paste0(missing_pkgs, collapse = ", ")
    )
    warning(simpleWarning(paste0(txt, collapse = " "), call = abortcall))
  }
  
  # check for non-missing packages that have a different version:
  mismatched_pkgs <- check$package[which(!check$versions_equal)]
  if(length(mismatched_pkgs)) {
    txt <- c(
      "The following packages/dependencies have a different loaded version than the version installed in `lib.loc`:",
      paste0(mismatched_pkgs, collapse = ", ")
    )
    warning(simpleWarning(paste0(txt, collapse = " "), call = abortcall))
  }
  
  
  if(length(missing_pkgs) || length(mismatched_pkgs)) {
    txt <- c("one or more potential `lib.loc` issues found;",
             "it is recommended you run `import_diagnose()` to investigate!")
    warning(simpleWarning(paste0(txt, collapse = " "), call = abortcall))
  }
  
}


#' @keywords internal
#' @noRd
.check_searchenv <- function(name = NULL, pos = NULL, create, abortcall) {
  
  if(!is.null(name)) {
    if(!.internal_is_string(name)) {
      stop(simpleError("search name must be a single string", call = abortcall))
    }
    .check_searchenv_protectednames(name, abortcall)
    
    if(!create) {
      if(!name %in% search()) {
        stop(simpleError(
          sprintf("search name \"%s\" not found", name), call = abortcall
        ))
      }
    }
    if(create) {
      if(name %in% search()) {
        stop(simpleError(
          sprintf("search name \"%s\" already exists", name), call = abortcall
        ))
      }
    }
  }
  
  if(!is.null(pos)) {
    if(!.internal_is_wholenum(pos)) {
      stop(simpleError("search position must be a single positive integer", call = abortcall))
    }
    if(pos >= length(search()) || pos <= 1L) {
      stop(simpleError("search position out of bounds", call = abortcall))
    }
    if(!create) .check_searchenv_protectednames(search()[pos], abortcall)
  }
}


#' @keywords internal
#' @noRd
.check_searchenv_protectednames <- function(name, abortcall) {
  name <- trimws(name, "both")
  if(startsWith(name, "package:")) {
    stop(simpleError(
      "search names with prefix \"package:\" are reserved for `loadNamespace()`",
      call = abortcall
    ))
  }
  if(startsWith(name, "tools:")) {
    stop(simpleError(
      "search names with prefix \"tools:\" are reserved for tools",
      call = abortcall
    ))
  }
  if(name ==".GlobalEnv") {
    stop(simpleError(
      "search name \".GlobalEnv\" is reserved for `globalenv()`",
      call = abortcall
    ))
  }
  if(name == "Autoloads") {
    stop(simpleError(
      "search name \"Autoloads\" is reserved for `.AutoloadEnv`",
      call = abortcall
    ))
  }
}


#' @keywords internal
#' @noRd
.check_exists <- function(selection, nms, typename, abortcall) {
  
  non_existing <- selection[!selection %in% nms]
  if(length(non_existing)) {
    txt <- paste0(c(
      "The following object names do not exist in the given ", typename, ": \n",
      paste0(non_existing, collapse = ", ")
    ), collapse = "")
    stop(simpleError(txt, call = abortcall))
  }
  
}



#' @keywords internal
#' @noRd
.check_conflicts <- function(objects, envir, abortcall) {
  
  nms <- names(envir)
  check_existing <- objects %in% nms
  all_conflicting <- sum(check_existing) == length(objects)
  
  if(sum(check_existing) > 0 && !all_conflicting) {
    conflict.txt <- paste0(
      "The following objects already exist in the given environment:",
      "\n\n",
      paste0(objects[check_existing], collapse = ", "),
      "\n"
    )
    warning(simpleWarning(conflict.txt, call = abortcall))
  }
  
  if(sum(check_existing) > 0 && all_conflicting) {
    conflict.txt <- paste0(
      "ALL objects already exist in the given environment",
      "\n"
    )
    warning(simpleWarning(conflict.txt, call = abortcall))
  }
  
  
  
}


