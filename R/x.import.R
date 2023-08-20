#' Helper functions for the tinyoperations package import system
#'
#' @description
#' The \code{help.import()} function
#' finds the help file for functions in an alias object or exposed infix operators. \cr
#' \cr
#' The \code{attr.import()} function
#' gets one specific special attributes or all special attributes
#' from an alias object returned by \link{import_as}. \cr
#'
#'
#' @param i either one of the following: \cr
#'  * a function (use back-ticks when the function is an infix operator).
#'  Examples:  \code{myfun} , \code{`\%operator\%`} , \code{myalias.$some_function} .
#'  If a function, the \code{alias} argument is ignored.
#'  * a string giving the function name or topic (i.e. \code{"myfun"}, \code{"thistopic"}).
#'  If a string, argument \code{alias} must be specified also.
#' @param alias the alias object as created by the \link{import_as} function. \cr
#' @param ... further arguments to be passed to \link[utils]{help}.
#' @param which The attributes to list. If \code{NULL}, all attributes will be returned. \cr
#' Possibilities: "pkgs", "conflicts", "versions", "args", and "ordered_object_names".
#'
#' @details
#' For \code{help.import(...)}: \cr
#' Do not use the \code{topic} / \code{package} and
#' \code{i} / \code{alias} arguments together. It's either one set or the other. \cr
#' \cr
#'
#'
#' @returns
#' For \code{help.import()}: \cr
#' Opens the appropriate help page. \cr
#' \cr
#' For \code{attr.import(alias, which = NULL)}: \cr
#' ALL special attributes of the given alias object are returned as a list. \cr
#' \cr
#' For \code{attr.import(alias, which = "pkgs")}: \cr
#' Returns a list with 3 elements:
#'
#' * packages_order: a character vector of package names,
#' giving the packages in the order they were loaded in the alias object.
#' * main_package: a string giving the name of the main package.
#' Re-exported functions, if present, are loaded together with the main package.
#' * re_exports.pkgs: a character vector of package names,
#' giving the packages from which the re-exported functions in the main package were taken. \cr \cr
#'
#' For \code{attr.import(alias, which = "conflicts")}: \cr
#' The order in which packages are loaded in the alias object
#' (see attribute \code{pkgs$packages_order})
#' matters:
#' Functions from later named packages overwrite those from earlier named packages,
#' in case of conflicts. \cr
#' The "conflicts" attribute returns a data.frame showing exactly which functions overwrite
#' functions from earlier named packages, and as such "win" the conflicts. \cr
#' \cr
#' For \code{attr.import(alias, which = "versions")}: \cr
#' A data.frame, giving the version of every package loaded in the alias,
#' ignoring re-exports. \cr
#' \cr
#' For \code{attr.import(alias, which = "args")}: \cr
#' Returns a list of input arguments.
#' These were the arguments supplied to \link{import_as} when
#' the alias object in question was created. \cr
#' \cr
#' For \code{attr.import(alias, which = "ordered_object_names")}: \cr
#' Gives the names of the objects in the alias, in the order as they were loaded. \cr
#' Only unique names are given, thus conflicting objects only appear once. \cr
#' (Note that if argument \code{re_exports} is \code{TRUE},
#' re-exported functions are loaded when the main package is loaded, thus changing this order slightly.) \cr
#'
#'
#' @seealso [tinyoperations_import()]
#'
#'
#'
#' @examples
#'
#' \dontrun{
#' import_as(mr., "magrittr")
#' import_inops("magrittr")
#'
#' attr.import(mr.)
#' attr.import(mr., which="conflicts")
#'
#' help.import(i=mr.$add)
#' help.import(i=`%>%`)
#' help.import(i="add", alias=mr.)
#' }
#'
#'
#'

#' @name x.import
NULL


#' @rdname x.import
#' @export
help.import <- function(..., i, alias) {

  # check arguments:
  lst <- list(...)
  args_base <- any(names(lst) %in% c("package", "topic"))
  args_import <- !missing(i) | !missing(alias)
  if(args_base & args_import) {
    stop("you cannot provide both `package`/`topic` AND `i`/`alias`")
  }
  if(!missing(alias)) {
    if(!is.environment(alias)) {
      stop("`alias` must be an alias object")
    }
  }
  if(!missing(i)) {
    if(isFALSE(is.character(i)) & isFALSE(is.function(i))) {
      stop("`i` must be a function or string")
    }
  }

  if(args_base) {
    return(utils::help(...))
  }


  temp.fun <- function(f) {
    fun_name <- attr(f, "function_name")
    if(is.null(fun_name)) {
      stop(
        "no function name attribute found",
        "\n",
        "are you sure the function comes from `tinyoperations::import_as()` or `tinyoperations::import_inops()`?")
    }
    package <- .internal_get_packagename(f)
    return(utils::help(fun_name, package = (package), ...))
  }

  if(!missing(i)) { # start i

    if(is.function(i)) { # start i is a function
      return(temp.fun(i))
    } # end i is a function


    if(is.character(i)) { # start i is a character
      if(missing(alias)) {
         stop("if `i` is specified as a string, `alias` must also be supplied")
      }

      if(i %in% names(alias)) {
        i <- alias[[i]]
        return(temp.fun(i))
      }

      if(!i %in% names(alias)) {
        pkgs <- c(
          eapply(alias, FUN=\(x)getNamespaceName(environment(x))) |> unlist(),
          eapply(alias, FUN=\(x)attr(x, "package")) |> unlist()
        ) |> unique()
        return(utils::help(topic = (i), package = (pkgs), ...))
      }

    } # end i is a character

  } # end i
}


#' @rdname x.import
#' @export
attr.import <- function(alias, which = NULL) {
  if(!.is.tinyalias(as.character(substitute(alias)), env = parent.frame(n = 1))) {
    stop("`alias` must be a locked environment as returned by `import_as()`")
  }

  if(is.null(which)) {
    return(alias$.__attributes__.)
  }

  allowed_which <- c("pkgs", "conflicts", "versions", "args", "ordered_object_names")
  if(!isTRUE(which %in% allowed_which)) {
    stop("unknown which given")
  }

  if(isTRUE(which %in% allowed_which)){
    return(alias$.__attributes__.[[which]])
  }

}
