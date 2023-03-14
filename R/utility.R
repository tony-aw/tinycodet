#' Utility operator
#'
#'@description
#' The \code{alias %m import <-% pkgs} operator
#' imports multiple R package under the same alias. \cr
#' \cr
#' The \code{alias %m import <-% pkgs} command is essentially the same as \cr
#' \code{alias <- loadNamespace("packagename")} \cr
#' except the \code{alias %m import <-% pkgs} operator
#' allows assigning multiple packages to the same alias,
#' and this operator does not import internal functions
#' (i.e. internal functions are kept internal, as they should). \cr
#' \cr
#' For example: \cr
#' \code{fv %m import <-% c("data.table", "collapse", "tidytable")} \cr
#' \cr
#' The \code{alias %m import <-% pkgs} operator will tell the user
#' about conflicting objects. It will also inform the user when importing
#' a package that consists mostly of infix operators.
#'
#' @param alias a variable name (unquoted),
#' giving the (not yet existing) object
#' where the package(s) are to be assigned to.
#' @param pkgs a character vector with the package name(s). \cr
#' NOTE: The order matters! If 2 packages share objects with the same name,
#' the package named last will overwrite the earlier named package.
#'
#' @returns
#' The variable named in the \code{alias} argument will be created
#' (if it did not already exist),
#' and it will contain the (merged) package environment.
#'
#'
#' @examples
#'
#' \dontrun{
#' fv %m import <-% c("data.table", "collapse", "tidytable")
#' }
#'
#'
#'



#' @rdname utility
#' @export
`%m import <-%` <- function(alias, pkgs) {
  if(length(pkgs)!=length(unique(pkgs))) {
    stop("one or more duplicate packages given")
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

