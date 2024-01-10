#' Directly Return a Data-set From a Package
#'
#' @description
#' The \code{import_data()} function gets a specified data set from a package. \cr
#' Unlike \code{utils::data()}, the \code{import_data()} function returns the data set directly,
#' and allows assigning the data set like so: \cr
#' \code{mydata <- import_data(...)}. \cr
#'
#'
#' @param package a single string, giving the name of the R-package.
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through). \cr
#' The \code{lib.loc} argument would usually be \code{.libPaths()}. \cr
#' See also \link[base]{loadNamespace}.
#' @param dataname a single string, giving the name of the data set.
#'
#'
#'
#'
#' @returns
#' Returns the data directly.
#' Thus, one can assign the data like so: \code{mydata <- import_data(...)}.
#'
#' @seealso \link{tinycodet_import}
#'
#'
#' @examples
#'
#' d <- import_data("datasets", "cars")
#' head(d)
#'
#'
#'
#' @rdname import_data
#' @export
import_data <- function(package, dataname, lib.loc = .libPaths()) {

  # check library:
  .internal_check_lib.loc(lib.loc, sys.call())

  if(length(dataname) > 1 || length(package) > 1) {
    stop("only a single dataset and a single package can be given")
  }
  out <- get(
    utils::data(list=dataname, package = package, lib.loc=lib.loc, envir = environment())
  )
  
  return(out)
}


