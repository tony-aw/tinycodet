#' Check for Mismatches between Loaded and Installed Packages
#'
#' @description
#' The `import_diagnose()` function
#' compares the loaded packages
#' with those installed in the specified `lib.loc`,
#' and checks for version and library path mismatches. \cr
#' Any differences found will be reported in the form of a simple `data.frame`. \cr
#' \cr
#' 
#'
#' @param lib.loc a character vector describing the location of R library trees to search through. \cr \cr
#'
#'
#'
#'
#' @returns
#' If no issues are found, returns `NULL`. \cr
#' Otherwise, a data.frame giving the packages where issues have been found. \cr
#' This data.frame will have the following columns: 
#' 
#'  - "package": Character vector of package names
#'  - "installed_in_lib.loc": logical vector indicating if the package is actually installed in the given `lib.loc` paths (`TRUE`) or not (`FALSE`).
#'  - "version_loaded": character vector giving the version of the packages as loaded in \link[base]{loadedNamespaces}.
#'  - "version_installed": character vector giving the version of the packages as installed in `lib.loc`; \cr
#'  It will be `NA` if the package is not installed in `lib.loc`.
#'  - "versions_equal": logical vector indicating if the loaded and installed versions match. \cr
#'  Gives `NA` if the package is not installed in `lib.loc`. \cr \cr
#'  
#'  
#'
#' @seealso \link{tinycodet_import}
#'
#'
#' @examples
#'
#' import_diagnose()
#'
#'
#'

#' @rdname import_diagnose
#' @export
import_diagnose <- function(
    lib.loc = .libPaths()
) {

  .check_pkgenv(parent.frame(), sys.call())
  lib.loc <- .import_lib.loc(lib.loc, sys.call())
  
  loaded_pkgs <- loadedNamespaces()
  loaded_pkgs <- loaded_pkgs[!loaded_pkgs %in% .list_coreR()]
  names(loaded_pkgs) <- loaded_pkgs
  if(length(loaded_pkgs) == 0L) {
    return(NULL)
  }
  
  out <- .import_diagnose(loaded_pkgs, lib.loc)
  
  if(nrow(out) == 0L) return(NULL)
  return(out)
  
  
}


.import_diagnose <- function(loaded_pkgs, lib.loc) {
  
  # get versions of loaded packages:
  loaded_versions <- lapply(
    loaded_pkgs, utils::packageVersion
  )
  
  # get version of installed packages, provided they exist:
  installed_pkgs <- loaded_pkgs
  installed_ind <- installed_pkgs %installed in% lib.loc
  installed_versions <- rep(NA_character_, length(installed_pkgs))
  names(installed_versions) <- names(installed_pkgs)
  if(sum(installed_ind) > 0L) {
    installed_versions[installed_ind] <- lapply(
      installed_pkgs[installed_ind], \(x) utils::packageVersion(x, lib.loc)
    )
  }
  
  # compare versions:
  versions_equal <- rep(NA, length(loaded_pkgs))
  if(sum(installed_ind) > 0L) {
    versions_equal[installed_ind] <- vapply(
      which(installed_ind), \(i) installed_versions[[i]] == loaded_versions[[i]],
      FUN.VALUE = logical(1L)
    )
  }
  
  # make output:
  out <- data.frame(
    package = loaded_pkgs,
    installed_in_lib.loc = installed_ind,
    version_loaded = vapply(loaded_versions, as.character, FUN.VALUE = character(1L)),
    version_installed = vapply(installed_versions, as.character, FUN.VALUE = character(1L)),
    versions_equal = versions_equal
  )
  
  ind <- ifelse(is.na(versions_equal), TRUE, !versions_equal)
  out <- out[ind, ]
  row.names(out) <- NULL
  
  return(out)
}