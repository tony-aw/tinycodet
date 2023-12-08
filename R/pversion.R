#' Check for Package Versions Mismatch
#'
#' @description
#' The \link[base]{loadNamespace} function
#' has the unfortunate property that when dependencies of the specified package
#' cannot be found in the specified library,
#' it searches other known libraries to load the dependencies. \cr
#' \cr
#' The \code{pversion_check4mismatch()} function
#' checks if there is any mismatch between
#' the currently loaded packages and the packages in the specified library. \cr
#' So one could, for example,
#' load and import or attach all packages at the start of a script,
#' and check for version mismatches like so:
#' 
#' ```{r eval = FALSE}
#' lib.loc <- ...some library path...
#' import_as(~ dpr., "dplyr", lib.loc = lib.loc)
#' import_inops("magrittr", lib.loc = lib.loc)
#' import_data("dplyr", starwars", lib.loc = lib.loc)
#' library(ggplot2)
#' pkgs <- tools::package_dependencies(
#'    c("dplyr", "magrtittr", "ggplot2"), recursive = TRUE
#' )
#' pversion_check4mismatch(pkgs, lib.loc)
#' 
#' ```
#' 
#' The \code{pversion_report()} function
#' gives a table of all specified packages,
#' with their loaded and installed versions,
#' regardless if there is a mismatch or not.
#'
#' @param pkgs a character vector with the package name(s). \cr
#' Packages that are not actually loaded will be ignored. \cr
#' If \code{NULL}, ALL loaded packages (excluding core R) will be checked.
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
#' pversion_report(loadedNamespaces(), .libPaths())
#' pversion_check4mismatch(loadedNamespaces(), .libPaths())
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
    ind <- which(versions_loaded != versions_lib)
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



