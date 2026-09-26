#' Internal functions
#'
#'
#'
#'
#'
#'
#'




#' @keywords internal
#' @noRd
.internal_package_found <- function(pkgs, lib.loc) {
  temp.fun <- function(pkg, lib.loc) {
    if(pkg %in% .list_coreR()) {
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
.import_lib.loc <- function(lib.loc, abortcall) {
  if(length(lib.loc) < 1L || !is.character(lib.loc) || any(!nzchar(lib.loc))) {
    stop(simpleError(
      "`lib.loc` must be a character vector with at least one library path",
      call = abortcall
    ))
  }
  lib.loc <- normalizePath(lib.loc)
  return(lib.loc)
}


#' @keywords internal
#' @noRd
.import_ls_loaded_deps <- function(pkgs) {
  # make internal cache for quick lookup:
  loaded_pkgs <- loadedNamespaces()
  direct_deps <- lapply(loaded_pkgs, function(p) {
    intersect(names(getNamespaceImports(p)), loaded_pkgs)
  })
  names(direct_deps) <- loaded_pkgs
  
  # recursive function:
  resolve_deps <- function(p_list, visited = character()) {
    next_pkgs <- setdiff(p_list, visited)
    if (length(next_pkgs) == 0) return(visited)
    children <- unique(unlist(direct_deps[next_pkgs]))
    resolve_deps(children, unique(c(visited, next_pkgs)))
  }
  
  all_found <- resolve_deps(pkgs)
  out <- setdiff(all_found, .list_coreR())
  
  return(out)
}


#' @keywords internal
#' @noRd
.internal_importenv <- function(env, parent_frame, abortcall) {
  
  if(is.null(env)) return(parent_frame)
  if(is.environment(env)) return(env)
  
  
  if(is.character(env)) {
    if(sum(env == search()) > 1L) {
      stop(simpleError(
        "duplicate search names found; fix this first",
        call = abortcall
      ))
    }
    .check_searchenv(env, NULL, FALSE, abortcall)
    return(as.environment(env))
  }
  else if(is.numeric(env)) {
    .check_searchenv(NULL, env, FALSE, abortcall)
    return(as.environment(env))
  }
  else {
    stop(simpleError(
      "`env` must be `NULL``, an environment, single string, or numeric scalar",
      call = abortcall
    ))
  }
  
}



#' @keywords internal
#' @noRd
.internal_prep_Namespace <- function(package, lib.loc, abortcall) {
  
  ns <- loadNamespace(package, lib.loc = lib.loc) |> as.list(all.names=TRUE, sorted=TRUE)
  names_exported <- names(ns[[".__NAMESPACE__."]][["exports"]])
  names_exported <- names_exported[!startsWith(names_exported, ".__")]
  ns <- ns[names_exported]
  
  ns <- ns[!is.na(names(ns))]
  names_exported <- names(ns)
  return(ns)
}



#' @keywords internal
#' @noRd
.internal_get_reexports_ns <- function(main_package, lib.loc, abortcall) {
  ns <- loadNamespace(main_package, lib.loc = lib.loc)
  names_exports <- names(ns[[".__NAMESPACE__."]][["exports"]])
  lst_imports <- ns[[".__NAMESPACE__."]][["imports"]]
  
  lst_imports <- lst_imports[vapply(lst_imports, is.character, logical(1))]
  pkgs <- names(lst_imports) |> unique()
  pkgs <- pkgs[!pkgs %in% "base"]
  
  if(length(pkgs) == 0) return(list())
  
  # 
  #   uninstalled_pkgs <- pkgs[!.internal_package_found(pkgs, lib.loc)]
  #   if(length(uninstalled_pkgs) > 0) {
  #     error.txt <- simpleError(paste0(
  #       "The following dependent packages (for the re-exports) are not installed:",
  #       "\n",
  #       paste0(uninstalled_pkgs, collapse = ", ")
  #     ), call = abortcall)
  #     stop(error.txt)
  #   }
  #   
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
.internal_help.import.tempfun <- function(f, ..., abortcall) {
  
  if(is.primitive(f)) {
    fun_name <- deparse(f)
    fun_name <- stringi::stri_replace_first_fixed(fun_name, ".Primitive(\"", "")
    fun_name <- stringi::stri_replace_last_fixed(fun_name, "\")", "")
    if(fun_name %in% names(baseenv())) {
      return(utils::help(topic = (fun_name), package = "base", ...))
    }
    else {
      stop(simpleError(
        "cannot determine package and original function name; please use `?` instead",
        call = abortcall
      ))
    }
  }
  
  f.env <- environment(f)
  if(is.null(f.env)) {
    stop(simpleError(
      "cannot determine package and original function name; please use `?` instead",
      call = abortcall
    ))
  }
  
  package <- utils::packageName(f.env)
  if(!isNamespaceLoaded(package)) {
    stop(simpleError(
      sprintf("function comes from package %s, which is not loaded", package),
      call = abortcall
    ))
  }
  pkg.env <- loadNamespace(package)
  if(isBaseNamespace(pkg.env)) {
    stop(simpleError(
      "function comes from package core 'R', not an imported package",
      call = abortcall
    ))
  }
  fun_name <- .rcpp_get_function_name(f, pkg.env, names(pkg.env))
  return(utils::help(topic = (fun_name), package = (package), ...))
}




#' @keywords internal
#' @noRd
.import_prefix_names <- function(ns, prefix, abortcall) {
  if(!.internal_is_string(prefix)) {
    stop(simpleError("`prefix` must be a single string", call = abortcall))
  }
  if(endsWith(prefix, ".") && !endsWith(prefix, "..")) {
    stop(simpleError("`prefix` cannot end with a single dot", call = abortcall))
  }
  
  # NOT to prefix:
  # -> infix
  # -> nonfun
  # -> primitives
  nms <- names(ns)
  is_fun <- vapply(ns, is.function, logical(1L))
  is_prim <- vapply(ns, is.primitive, logical(1L))
  is_infix <-  stringi::stri_detect_regex(nms, "%|:=")
  ind <- which(is_fun & !is_prim & !is_infix)
  nms[ind] <- stringi::stri_c(prefix, nms[ind])
  return(nms)
  
}

