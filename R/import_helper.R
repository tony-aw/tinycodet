#' Helper Functions for the 'tinycodet' Package Import System
#'
#' @description
#' The \code{help.import()} function
#' finds the help file for functions or topics,
#' including exposed functions/operators as well as functions in a package alias object. \cr
#' \cr
#'
#'
#' @param i either one of the following:
#'  * a function (use back-ticks when the function is an infix/replacement operator). \cr
#'  Examples: \cr
#'  \code{myfun} , \code{`\%operator\%`} , \code{`fun<-`}, \code{myalias.$some_function}. \cr
#'  If a function, the \code{alias} argument is ignored.
#'  * a string giving the function name or topic (i.e. \code{"myfun"}, \code{"thistopic"}). \cr
#'  If a string, argument \code{alias} must be specified also.
#' @param alias an object of class \link{tinyimport_alias} as returned by \link{import_as}.
#' @param ... further arguments to be passed to \link[utils]{help}.
#'
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
#' import_as(.str ~ stringi)
#' import_from("magrittr", import_ls("magrittr", "infix"))
#' help.import(i = .str$stri_sub)
#' help.import(i = `%>%`)
#' help.import(i = "stri_sub", alias = .str)
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
#'
#'
#' @seealso \link{tinycodet_import}
#'
#'
#'
#' @example inst/examples/import.R
#'
#'
#'
#'

#' @name import_helper
NULL


#' @rdname import_helper
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
  args_base <- length(lst) > 0L && lst_has_base_args
  args_import <- !missing(i) || !missing(alias)
  if(args_base && args_import) {
    stop("you cannot provide both `package`/`topic` AND `i`/`alias`")
  }
  if(!missing(alias)) {
    if(!is.tinyimport_alias(alias)) {
      stop("`alias` must be a package alias object")
    } 
  }
  
  i_is_string <- .internal_is_string(i)
  
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
    }
    else {
      pkgs <- unlist(alias$.__attributes__.$pkgs) |> unique()
      return(utils::help(topic = (i), package = (pkgs), ...))
    }
    
  } # end i is a character
}


