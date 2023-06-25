#' Check if one or more packages are installed in the specified library
#'
#' @description
#' The \code{pkgs %installed in% lib.loc} operator
#' checks if one or more package(s) \code{pkgs} exist(s) in library location \code{lib.loc}. \cr
#' Now you no longer have to attach a package with \code{require()} simply to check if it exists. \cr
#' Moreover, this operator makes it syntactically explicit in your code
#' where you are looking for your R package(s). \cr
#' \cr
#'
#' @param pkgs a single string, or character vector, with the package name(s).
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through).
#' This is usually \code{.libPaths()}.
#' See also \link[base]{loadNamespace}.
#'
#' @returns
#' Returns a logical vector, where \code{TRUE} indicates a package is installed,
#' and \code{FALSE} indicates a package is not installed.
#' \cr
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

#' @rdname installed_in
#' @export
`%installed in%` <- function(pkgs, lib.loc) {
  temp.fun <- function(x)nzchar(system.file(package=x, lib.loc=lib.loc))
  out <- sapply(pkgs, temp.fun)
  return(out)
}