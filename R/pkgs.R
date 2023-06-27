#' Miscellaneous package functions
#'
#' @description
#' The \code{pkgs %installed in% lib.loc} operator
#' checks if one or more package(s) \code{pkgs} exist(s) in library location \code{lib.loc}. \cr
#' Now you no longer have to attach a package with \code{require()} simply to check if it exists. \cr
#' Moreover, this operator makes it syntactically explicit in your code
#' where you are looking for your R package(s). \cr
#' \cr
#' The \code{pkgs_get_deps()} function gets the dependencies of a package
#' from the Description file. It works on non-CRAN packages also.
#'
#' @param pkgs a single string, or character vector, with the package name(s).
#' @param package a single string giving the package name
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through).
#' This is usually \code{.libPaths()}.
#' See also \link[base]{loadNamespace}.
#' @param deps_type a character vector, giving the dependency types to be used. \cr
#' Defaults to \code{c("Depends", "Imports", "LinkingTo")}.
#' @param base logical,
#' indicating whether base/core R packages should be included (\code{TRUE}),
#' or not included (\code{FALSE}; the default).
#' @param recom logical,
#' indicating whether the pre-installed "recommended" R packages should be included (\code{TRUE}),
#' or not included (\code{FALSE}; the default).
#' Note that only the recommended R packages actually installed in your system are taken into consideration.
#'
#' @returns
#' For  \code{pkgs %installed in% lib.loc}:
#' Returns a logical vector, where \code{TRUE} indicates a package is installed,
#' and \code{FALSE} indicates a package is not installed. \cr
#' \cr
#' For \code{pkgs_get_deps()}:
#' A character vector of dependencies.
#' 
#' 
#' @references https://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran
#'
#'
#' @examples
#'
#' \dontrun{
#' pkgs <- c(unlist(tools::package_dependencies("devtools")), "devtools")
#' pkgs %installed in% .libPaths()
#' }
#'
#'
#'

#' @rdname pkgs
#' @export
pkgs_get_deps <- function(
    package, lib.loc=.libPaths(), deps_type=c("Depends", "Imports", "LinkingTo"),
    base=FALSE, recom=FALSE
) {
  if(length(package)>1){
    stop("Only one package can be given")
  }
  # based of https://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran
  dcf <- read.dcf(file.path(system.file("DESCRIPTION", package = package, lib.loc = lib.loc)))
  jj <- intersect(deps_type, colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names=FALSE)
  val <- gsub("\\s.*", "", trimws(val))
  depends <- val[val != "R"]
  if(!base){
    pkgs_core <- c(
      utils::installed.packages(priority = "base") |> rownames(),
      utils::installed.packages(lib.loc=lib.loc, priority = "base") |> rownames()
    ) |> unique()
    depends <- setdiff(depends, pkgs_core)
  }
  if(!recom) {
    pkgs_preinst <- c(
      utils::installed.packages(priority = "recommended") |> rownames(),
      utils::installed.packages(lib.loc=lib.loc, priority = "recommended") |> rownames()
    ) |> unique()
    depends <- setdiff(depends, pkgs_preinst)
  }
  return(depends)
}

#' @rdname pkgs
#' @export
`%installed in%` <- function(pkgs, lib.loc) {
  temp.fun <- function(x)nzchar(system.file(package=x, lib.loc=lib.loc))
  out <- sapply(pkgs, temp.fun)
  return(out)
}

