#' Check for Package Versions Mismatch
#'
#' @description
#' The \code{pversion_check4mismatch()} function
#' checks if there is any mismatch between
#' the currently loaded packages and the packages in the specified library path. \cr
#' This is useful as 'R' (silently) looks for all known libraries
#' (see \link[base]{.libPaths})
#' when it cannot find package dependencies that need to be loaded,
#' which may result in loading packages of a different version than the user expected. \cr
#' \cr
#' The \code{pversion_report()} function
#' gives a table of all specified packages,
#' with their loaded and installed versions,
#' regardless if there is a mismatch or not.
#'
#' @param pkgs a character vector with the package name(s). \cr
#' Packages that are not actually loaded will be ignored. \cr
#' If \code{NULL}, all loaded packages
#' (see \link[base]{loadedNamespaces})
#' excluding core/base R will be checked.
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through). \cr
#' The \code{lib.loc} argument would usually be \code{.libPaths()}. \cr
#' See also \link[base]{loadNamespace}.
#' 
#'
#' @returns
#' For  \code{pversion_check4mismatch()}: \cr
#' If no mismatch between loaded versions and those in \code{lib.loc} were found,
#' returns \code{NULL}. \cr
#' Otherwise it returns a \code{data.frame},
#' with the loaded version and library version of the specified packages. \cr
#' \cr
#' For \code{pversion_report()}: \cr
#' Returns a \code{data.frame},
#' with the loaded version and library version of the specified packages. \cr
#' \cr
#' 
#' In both functions, the data.frame consists of the following columns:
#' 
#'  * "package": specifying the packages.
#'  * "version_loaded": the version already loaded.
#'  * "version_lib.loc": the version of each package found in the library location. \cr \cr
#' 
#'
#' @seealso [tinycodet_import()]
#'
#'
#' @examplesIf "dplyr" %installed in%  .libPaths()
#' "dplyr" %installed in%  .libPaths()
#' 
#' import_as(~dpr., "dplyr")
#' pversion_check4mismatch()
#' pversion_report()
#' 
#'
#'
#'
#'

#' @name pversion
NULL



#' @rdname pversion
#' @export
pversion_check4mismatch <- function(pkgs = NULL, lib.loc = .libPaths()) {
  
  # check lib.loc:
  .internal_check_lib.loc(lib.loc, sys.call())
  
  # check pkgs:
  if(is.null(pkgs)) pkgs <- setdiff(loadedNamespaces(), .internal_list_coreR())
  if(!is.character(pkgs) || length(pkgs) == 0) {
    stop("`pkgs` must a non-empty character vector")
  }
  pkgs <- pkgs[!pkgs %in% .internal_list_coreR()]
  .internal_check_pkgs(pkgs, lib.loc, abortcall = sys.call())
  
  
  pkgs <- pkgs[pkgs %in% loadedNamespaces()]
  
  
  if(length(pkgs) > 0) {
    versions_loaded <- lapply(pkgs, getNamespaceVersion) |> unlist()
    versions_lib <- lapply(pkgs, \(x)as.character(utils::packageVersion(x, lib.loc = lib.loc))) |>
      unlist()
    versions_compare <- mapply(utils::compareVersion, versions_loaded, versions_lib)
    ind <- which(versions_compare != 0)
    if(length(ind) > 0) {
      pkgs <- pkgs[ind]
      versions_lib <- versions_lib[ind]
      versions_loaded <- versions_loaded[ind]
      tab <- data.frame(
        package = pkgs,
        version_loaded = versions_loaded,
        version_lib.loc = versions_lib
      )
      rownames(tab) <- 1:nrow(tab)
      return(tab)
    }
  }
  
  return(NULL)
}

#' @rdname pversion
#' @export
pversion_report <- function(pkgs = NULL, lib.loc = .libPaths()) {
  
  # check lib.loc:
  .internal_check_lib.loc(lib.loc, sys.call())
  
  # check pkgs:
  if(is.null(pkgs)) pkgs <- setdiff(loadedNamespaces(), .internal_list_coreR())
  if(!is.character(pkgs) || length(pkgs) == 0) {
    stop("`pkgs` must a non-empty character vector")
  }
  pkgs <- pkgs[!pkgs %in% .internal_list_coreR()]
  .internal_check_pkgs(pkgs, lib.loc, abortcall = sys.call())
  
  
  pkgs <- pkgs[pkgs %in% loadedNamespaces()]
  
  
  if(length(pkgs) > 0) {
    versions_loaded <- lapply(pkgs, getNamespaceVersion) |> unlist()
    versions_lib <- lapply(pkgs, \(x)as.character(utils::packageVersion(x, lib.loc = lib.loc))) |>
      unlist()
    tab <- data.frame(
      package = pkgs,
      version_loaded = versions_loaded,
      version_lib.loc = versions_lib
    )
    rownames(tab) <- 1:nrow(tab)
    return(tab)
  }
  
  tab <- data.frame(
    package = character(0),
    version_loaded = character(0),
    version_lib.loc = character(0)
  )
  return(tab)
  
}



