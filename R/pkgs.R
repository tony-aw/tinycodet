#' Package import management operator and functions
#'
#'@description
#' The \code{alias %m import <-% pkgs} operator
#' imports the namespaces of an R package
#' (or a small set of R packages that "belong" to each other)
#' under the same alias. \cr
#' \cr
#'
#' The \code{import_data(dataname, package)} function gets a specified data set from a package. \cr
#' Unlike \code{utils::data()}, \code{import_data()} returns the dataset directly,
#' and allows assigning the dataset like so: \cr
#' \code{mydata <- import_data(...)}. \cr
#' \cr
#' The \code{force_libPaths()} function allows the user to force R to specific libraries.
#' This was needed since base R's \code{.libPaths()} function
#' only allows adding new library paths, not overwrite existing ones.
#' The library paths are of course re-set again every time R restarts.
#'
#' @param alias a variable name (unquoted),
#' giving the (not yet existing) object
#' where the package(s) are to be assigned to.
#' @param pkgs a character vector with the package name(s). \cr
#' NOTE: The order matters! If 2 packages share objects with the same name,
#' the package named last will overwrite the earlier named package.
#' @param dataname a single string, giving the name of the dataset.
#' @param package a single string, giving the name of the package.
#' @param lib_vec a character vector giving the new library path(s). \cr
#' Just like in \code{.libPaths()}, the order matters: \cr
#' R will first look for packages in the first path in \code{.libPaths()}, \cr
#' and if it cannot find the package(s),
#' it will look for the packages in the second path in \code{.libPaths()},
#' etc.
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
#' For \code{force_libPaths()}: \cr
#' Adjusts the R library paths as defined in \code{.libPaths()} directly.
#'
#' @references McBain (2019, June 20). Before I Sleep: Hacking R's library paths. Retrieved from https://milesmcbain.com/posts/hacking-r-library-paths/
#'
#' @examples
#'
#' \dontrun{
#' force_libPaths("/mylibrary")
#' fv %m import <-% c("data.table", "collapse", "tidytable")
#' d <- import_data("chicago", "gamair")
#' }
#'
#'
#'

#' @name pkgs
NULL

#' @rdname pkgs
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

#' @rdname pkgs
#' @export
import_data <- function(dataname, package) {
  if(length(dataname)>1 | length(package)>1) {
    stop("only a single dataset and a single package can be given")
  }
  return(get(
    utils::data(list=dataname, package = package, envir = environment())
  ))
}

#' @rdname pkgs
#' @export
force_libPaths <- function(lib_vec) {
  lib_vec <- normalizePath(lib_vec, mustWork = TRUE)
  shim_fun <- .libPaths
  shim_env <- new.env(parent = environment(shim_fun))
  shim_env$.Library <- character()
  shim_env$.Library.site <- character()
  environment(shim_fun) <- shim_env
  shim_fun(lib_vec)
}
