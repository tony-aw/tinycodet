#' Directly return a data-set from a package
#'
#' @description
#' The \code{import_data()} function gets a specified data set from a package. \cr
#' Unlike \code{utils::data()}, the \code{import_data()} function returns the data set directly,
#' and allows assigning the data set like so: \cr
#' \code{mydata <- import_data(...)}.
#'
#'
#' @param package the quoted package name.
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through). \cr
#' This is usually \code{.libPaths()}. \cr
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
#' @seealso [tinyoperations_import()]
#'
#'
#' @examples
#'
#' d <- import_data("chicago", "gamair")
#' head(d)
#'
#'
#' @rdname import_data
#' @export
import_data <- function(dataname, package, lib.loc=.libPaths()) {
  if(length(dataname)>1 | length(package)>1) {
    stop("only a single dataset and a single package can be given")
  }
  return(get(
    utils::data(list=dataname, package = package, lib.loc=lib.loc, envir = environment())
  ))
}


