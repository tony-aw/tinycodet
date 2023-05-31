#' Package import management operator and functions
#'
#' @description
#' The \code{alias %m import <-% pkgs} operator
#' imports the namespaces of an R package
#' (or a small set of R packages that "belong" to each other)
#' under the same alias. \cr
#' \cr
#' The \code{import_lsf(package, ...)} function gets a list of exported functions from a package. \cr
#' \cr
#' The \code{import_data(dataname, package)} function gets a specified data set from a package. \cr
#' Unlike \code{utils::data()}, the \code{import_data()} function returns the dataset directly,
#' and allows assigning the dataset like so: \cr
#' \code{mydata <- import_data(...)}. \cr
#' \cr
#'
#' @param alias a variable name (unquoted),
#' giving the (not yet existing) object
#' where the package(s) are to be assigned to.
#' @param pkgs a character vector with the package name(s). \cr
#' NOTE: The order matters! If 2 packages share objects with the same name,
#' the package named last will overwrite the earlier named package.
#' @param dataname a single string, giving the name of the dataset.
#' @param package a single string, giving the name of the package.
#' @param type The type of functions to list. Possibilities: \cr
#' \code{"inops"} or \code{"operators"}: Only infix operators (functions surrounded by percentage signs). \cr
#' \code{"regfuns"}: Only regular functions (thus excluding infix operators). \cr
#' \code{"all"}: All functions, both regular and infix operators. \cr
#'
#'
#' @details
#' The \code{alias %m import <-% pkgs} command is essentially the same as \cr
#' \code{alias <- loadNamespace("packagename")} \cr
#' except the \code{alias %m import <-% pkgs} operator
#' allows assigning multiple packages to the same alias,
#' and this operator does not import internal functions
#' (i.e. internal functions are kept internal, as they should). \cr
#' \cr
#' The \code{alias %m import <-% pkgs} operator will tell the user
#' about conflicting objects. It will also inform the user when importing
#' a package that consists mostly of infix operators. \cr
#' \cr
#' Note: the user should not use the \code{alias %m import <-% pkgs} operator
#' unless the user knows what he/she is doing. \cr
#' The operator will give a warning when more than 3 packages being imported into the same alias. \cr
#' \cr
#'
#' @returns
#' For \code{%m import <-%}: \cr
#' The variable named in the \code{alias} argument will be created
#' (if it did not already exist),
#' and it will contain the (merged) package environment. \cr
#' \cr
#' For \code{import_data()}: \cr
#' Returns the data directly.
#' Thus, one can assign the data like so: \code{mydata <- import_data(...)}. \cr
#' \cr
#' For \code{import_lsf()}: \cr
#' A vector of function and/or operator names. \cr
#' This vector can be directly used in the \code{include.only} argument of the
#' \code{library()} function. \cr
#'
#'
#' @examples
#'
#' \dontrun{
#' fv %m import <-% c("data.table", "collapse", "tidytable")
#' library(data.table, include.only = import_lsf("data.table", type="inops"))
#' library(collapse, include.only = import_lsf("collapse", type="inops"))
#' d <- import_data("chicago", "gamair")
#' }
#'
#'
#'

#' @name import
NULL

#' @rdname import
#' @export
`%m import <-%` <- function(alias, pkgs) {
  if(length(pkgs)!=length(unique(pkgs))) {
    stop("one or more duplicate packages given")
  }
  if(length(pkgs)>3) {
    warning("More than 3 packages are being imported into the same alias...")
  }

  if(length(pkgs)==1){
    message(paste0("Importing package: ", pkgs, "..."))
    export_names <- getNamespaceExports(pkgs)

    prop.infix <- mean(grepl("%", export_names))
    if(prop.infix >= 0.5) {
      message(paste0(
        "Most functions in this package are infix operators;",
        "\n",
        "consider using library(", pkgs, ") instead."
      ))
    }

    namespace <- loadNamespace(pkgs) |> as.list()
    lst.exported <- namespace[export_names]
    out <- as.environment(lst.exported)
    message("Done")
  }
  if(length(pkgs)>1) {
    export_names_all <- character()
    export_names_allconflicts <- character()
    namespaces <- list()
    for (i in 1:length(pkgs)) {
      message(paste0("Importing package: ", pkgs[i], "..."))
      export_names_current <- getNamespaceExports(pkgs[i])

      prop.infix <- mean(grepl("%", export_names_current))
      if(prop.infix >= 0.5) {
        message(paste0(
          "NOTE: Most functions in this package are infix operators;",
          "\n",
          "consider using library(", pkgs[i], ") instead."
        ))
      }

      export_names_intersection <- intersect(export_names_current, export_names_all)
      if(length(export_names_intersection)==0 & i>1) {
        message("no conflicts")
      }
      if(length(export_names_intersection)>0) {
        message(
          "The following conflicting objects detected:",
          "\n \n",
          paste0(export_names_intersection, collapse = ", "),
          "\n \n",
          pkgs[i], " will overwrite conflicting objects from previous imported packages..."
        )
      }
      export_names_allconflicts <- c(export_names_intersection, export_names_allconflicts)
      export_names_all <- c(export_names_current, export_names_all)
      namespace_current <- loadNamespace(pkgs[i]) |> as.list()
      lst.exported <- namespace_current[export_names_current]
      namespaces <- utils::modifyList(namespaces, lst.exported)
      message("\n")
    }
    out <- as.environment(namespaces)
    message("Done")
  }
  message(paste0(
    "You can now access the functions using ", substitute(alias), "$...",
    "\n",
    "methods will work like normally. \n"
  ))
  eval(call("<-", substitute(alias), out), envir = parent.frame(n = 1))
}

#' @rdname import
#' @export
import_data <- function(dataname, package) {
  if(length(dataname)>1 | length(package)>1) {
    stop("only a single dataset and a single package can be given")
  }
  return(get(
    utils::data(list=dataname, package = package, envir = environment())
  ))
}

#' @rdname import
#' @export
import_lsf <- function(package, type) {
  if(length(package)>1){
    stop("only a single package can be given")
  }
  ns <- getNamespaceExports(package)
  if(type=="inops" | type=="operators") {
    out <- grep("%|:=", ns, value = TRUE)
  }
  if(type=="regfuns") {
    out <- grep("%|:=", ns, value = TRUE, invert = TRUE)
  }
  if(type=="all") {
    out <- ns
  }
  return(out)
}
