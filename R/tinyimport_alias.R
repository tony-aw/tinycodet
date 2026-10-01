#' Class tinyimport_alias
#'
#' @description
#' The \link{import_as} function
#' creates an object of class `"tinyimport_alias"`. \cr
#' This help page documents its usage. \cr
#' \cr
#' To get a function from a `tinyimport_alias`, once can use the `$` operator. \cr
#' To use, for example, function "some_function()" from alias ".alias", use: \cr
#' `.alias$some_function()`. \cr
#' To "unimport" the package alias object, simply remove it from the environment it was placed in. \cr
#' \cr
#' `is.tinyimport_alias()` checks if an object truly is a package alias as created by the \link{import_as} function. \cr
#' \cr
#' The `attr.import()` function
#' gets one or all special attribute(s)
#' from an alias object returned by \link{import_as}. \cr
#' \cr
#'
#'
#' @param alias the alias object as created by the \link{import_as} function.
#' @param which The attributes to list. If \code{NULL}, all attributes will be returned. \cr
#' Possibilities: "pkgs", "conflicts", and "ordered_object_names". \cr \cr
#' 
#' @returns
#' 
#' For \code{is.tinyimport_alias()}: \cr
#' `TRUE` or `FALSE`, indicating if the object is a tiny package alias. \cr
#' \cr
#' 
#' For \code{attr.import(alias, which = NULL)}: \cr
#' All special attributes of the given alias object are returned as a list. \cr
#' \cr
#' For \code{attr.import(alias, which = "pkgs")}: \cr
#' Returns a list with 3 elements:
#'
#' * packages_order: a character vector of package names,
#' giving the packages in the order they were imported in the alias object.
#' * main_package: a string giving the name of the main package.
#' Re-exported functions, if present, are taken together with the main package.
#' * re_exports.pkgs: a character vector of package names,
#' giving the packages from which the re-exported functions in the main package were taken. \cr \cr
#'
#' For \code{attr.import(alias, which = "conflicts")}: \cr
#' The order in which packages are imported in the alias object
#' (see attribute \code{pkgs$packages_order})
#' matters:
#' Functions from later named packages overwrite those from earlier named packages,
#' in case of conflicts. \cr
#' The "conflicts" attribute returns a data.frame showing exactly which functions overwrite
#' functions from earlier named packages, and as such "win" the conflicts. \cr
#' \cr
#' For \code{attr.import(alias, which = "ordered_object_names")}: \cr
#' Gives the names of the objects in the alias, in the order as they were imported. \cr
#' For conflicting objects, the last imported ones are used for the ordering. \cr
#' Note that if argument \code{re_exports} is \code{TRUE},
#' re-exported functions are imported when the main package is imported,
#' thus changing this order slightly. \cr \cr
#' 
#' @seealso \link{tinycodet_import}
#' 
#' @example inst/examples/import.R
#' 
#' 

#' @name tinyimport_alias
NULL


#' @rdname tinyimport_alias
#' @export
is.tinyimport_alias <- function(alias) {
  
  if(!is.environment(alias)) return(FALSE)
  if(!"tinyimport_alias" %in% class(alias)) return(FALSE)
  if(!environmentIsLocked(alias)) return(FALSE)
  
  check_attr <- all(
    names(alias$.__attributes__.) == c("pkgs", "conflicts", "ordered_object_names", "tinyimport")
  )
  if(!check_attr) return(FALSE)
  if(alias$.__attributes__.$tinyimport != "tinyimport") return(FALSE)
  return(TRUE)
}


#' @rdname tinyimport_alias
#' @export
attr.import <- function(alias, which = NULL) {
  if(!is.tinyimport_alias(alias)) {
    stop("`alias` must be a locked environment as returned by `import_as()`")
  }
  
  if(is.null(which)) {
    return(alias$.__attributes__.)
  }
  
  allowed_which <- c("pkgs", "conflicts", "ordered_object_names")
  if(!isTRUE(which %in% allowed_which)) {
    stop("unknown `which` given")
  }
  
  if(isTRUE(which %in% allowed_which)){
    return(alias$.__attributes__.[[which]])
  }
  
}



#' @keywords internal
#' @noRd
.get.tinyimport_alias <- function(e1, e2) {
  alias_name <- as.character(substitute(e1))
  nms <- names(e1)
  if(!e2 %in% nms) {
    stop(sprintf("'%s' is not an exported object in alias '%s'", e2, alias_name))
  }
  return(get(e2, envir = e1))
}

#' @export
`$.tinyimport_alias` <- .get.tinyimport_alias


#' @export
`[[.tinyimport_alias` <- .get.tinyimport_alias



#' @export
`as.list.tinyimport_alias` <- function(x, all.names = TRUE, sorted = FALSE, ...) {
  class(x) <- NULL
  return(as.list(x, all.names = all.names, sorted = sorted))
}


#' @export
#' @importFrom utils .DollarNames
.DollarNames.tinyimport_alias <- function(x, pattern = "") {
  nms <- x$.__attributes__.$ordered_object_names
  utils::findMatches(pattern, nms)
}

