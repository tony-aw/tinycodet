#' Helper Functions for the 'tinycodet' Package Import System
#'
#' @description
#' The \code{help.import()} function
#' finds the help file for functions or topics,
#' including exposed functions/operators as well as functions in a package alias object. \cr
#' \cr
#' The \code{is.tinyimport()} function
#' checks if an alias object or an exposed function is of class \code{tinyimport};
#' i.e. if it is an object produced by the
#' \link{import_as}, \link{import_inops}, or \link{import_LL} function. \cr
#' \cr
#' The \code{attr.import()} function
#' gets one or all special attribute(s)
#' from an alias object returned by \link{import_as}. \cr
#' \cr
#'
#'
#'
#' @param i either one of the following:
#'  * a function (use back-ticks when the function is an infix operator).
#'  Examples:  \code{myfun} , \code{`\%operator\%`} , \code{myalias.$some_function} .
#'  If a function, the \code{alias} argument is ignored.
#'  * a string giving the function name or topic (i.e. \code{"myfun"}, \code{"thistopic"}).
#'  If a string, argument \code{alias} must be specified also.
#' @param alias the alias object as created by the \link{import_as} function. \cr
#' @param ... further arguments to be passed to \link[utils]{help}.
#' @param which The attributes to list. If \code{NULL}, all attributes will be returned. \cr
#' Possibilities: "pkgs", "conflicts", "args", and "ordered_object_names".
#' @param x an existing object (i.e. an assigned variable or a locked constant) to be tested.
#'
#' @details
#' For \code{help.import(...)}: \cr
#' Do not use the \code{topic} / \code{package} and
#' \code{i} / \code{alias} argument sets together.
#' It's either one set or the other. \cr
#' For example:
#'
#' ```{r eval = FALSE}
#'
#' import_as(~ mr., "magrittr")
#' import_inops(mr.)
#' help.import(i = mr.$add)
#' help.import(i = `%>%`)
#' help.import(i = "add", alias = mr.)
#' help.import(topic = "%>%", package = "magrittr")
#' help.import("%>%", package = "magrittr") # same as previous line
#'
#' ```
#'
#'
#'
#' @returns
#' For \code{help.import()}: \cr
#' Opens the appropriate help page. \cr
#' \cr
#' For \code{is.tinyimport()}: \cr
#' Returns \code{TRUE} if the function is produced by
#' \link{import_as}, \link{import_inops}, or \link{import_LL},
#' and returns \code{FALSE} if it is not. \cr
#' \cr
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
#' For \code{attr.import(alias, which = "args")}: \cr
#' Returns a list of input arguments.
#' These were the arguments supplied to \link{import_as} when
#' the alias object in question was created. \cr
#' \cr
#' For \code{attr.import(alias, which = "ordered_object_names")}: \cr
#' Gives the names of the objects in the alias, in the order as they were imported. \cr
#' For conflicting objects, the last imported ones are used for the ordering. \cr
#' Note that if argument \code{re_exports} is \code{TRUE},
#' re-exported functions are imported when the main package is imported,
#' thus changing this order slightly. \cr \cr
#'
#'
#' @seealso \link{tinycodet_import}
#'
#'
#'
#' @examples
#'
#' import_as(~ to., "tinycodet")
#' import_inops(to.)
#' `%s==%` <- stringi::`%s==%`
#'
#' is.tinyimport(to.) # returns TRUE
#' is.tinyimport(`%:=%`) # returns TRUE
#' is.tinyimport(`%s==%`) # returns FALSE: not imported by tinycodet import system
#'
#' attr.import(to., which = "conflicts")
#'
#'
#'
#'

#' @name x.import
NULL


#' @rdname x.import
#' @export
help.import <- function(..., i, alias) {

  # directly go to base help if applicable:
  if(missing(i) && missing(alias)) {
    return(utils::help(...))
  }
  
  # check arguments for help.import:
  lst <- list(...)
  lst_has_base_args <- any(c(
    names(lst) == character(0),
    any(names(lst) %in% c("topic", "package")),
    sum(nzchar(names(lst))) < length(lst)
  ))
  args_base <- length(lst) > 0 && lst_has_base_args
  args_import <- !missing(i) || !missing(alias)
  if(args_base && args_import) {
    stop("you cannot provide both `package`/`topic` AND `i`/`alias`")
  }
  if(!missing(alias)) {
    if(!is.environment(alias)) {
      stop("`alias` must be a package alias object")
    }
    if(!.is.tinyalias(as.character(substitute(alias)), parent.frame(n = 1))) {
      stop("`alias` must be a package alias object")
    } 
  }
  i_is_string <- is.character(i) && length(i) == 1
  if(!i_is_string && !is.function(i)) {
    stop("`i` must be a function or a single string")
  }
 
  
  # help.import:
  if(is.function(i)) { # start i is a function
    return(.internal_help.import.tempfun(i, ..., abortcall = sys.call()))
  } # end i is a function
  
  
  if(i_is_string) { # start i is a character
    if(missing(alias)) {
      stop("if `i` is specified as a string, `alias` must also be supplied")
    }
    
    if(i %in% names(alias)) {
      i <- alias[[i]]
      return(.internal_help.import.tempfun(i, ..., abortcall = sys.call()))
    } else {
      pkgs <- unlist(alias$.__attributes__.$pkgs) |> unique()
      return(utils::help(topic = (i), package = (pkgs), ...))
    }
    
  } # end i is a character
}



#' @rdname x.import
#' @export
is.tinyimport <- function(x) {
  
  if(!is.symbol(substitute(x))) {
    stop("only assigned objects (variables/constants) can be checked")
  }
  
  x_chr <- as.character(substitute(x))
  myenv <- parent.frame(n = 1)


  out <- .is.tinyalias(x_chr, myenv) | .is.tinyinop(x_chr, myenv) | .is.tinyLL(x_chr, myenv)
  return(out)
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

  allowed_which <- c("pkgs", "conflicts", "args", "ordered_object_names")
  if(!isTRUE(which %in% allowed_which)) {
    stop("unknown `which` given")
  }

  if(isTRUE(which %in% allowed_which)){
    return(alias$.__attributes__.[[which]])
  }

}

