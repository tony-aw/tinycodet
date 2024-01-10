#' Source Specific Objects from Script
#'
#' @description
#' The \code{source_selection()} function is the same as
#' base R's \link[base]{source} function, except that it allows only placing
#' the selected objects and functions into the current environment,
#' instead of all objects. \cr
#' \cr
#' The objects to be selected can be specified using any combination of the following:
#' \itemize{
#'  \item by supplying a character vector of exact object names to the \code{select} argument.
#'  \item by supplying a character vector of \code{regex} patterns to the \code{regex} argument.
#'  \item by supplying a character vector of \code{fixed} patterns to the \code{fixed} argument.
#' }
#'
#' Note that the \code{source_selection()} function does NOT suppress output
#' (i.e. plots, prints, messages)
#' from the sourced script file. \cr
#'
#' @param lst a named list, giving the arguments to be passed to the
#' \link[base]{source} function. \cr
#' The \code{local} argument should not be included in the list.
#' @param select a character vector,
#' giving the exact names of the functions or objects appearing in the script,
#' to expose to the current environment.
#' @param regex a character vector of \code{regex} patterns
#' (see \link[stringi]{about_search_regex}). \cr
#' These should give regular expressions that match to
#' the names of the functions or objects appearing in the script,
#' to expose to the current environment. \cr
#' For example, to expose the following methods to the current environment, \cr
#' \code{mymethod.numeric()} and \code{mymethod.character()} from generic \code{mymethod()}, \cr
#' one could specify \code{regex = "^mymethod"}. \cr
#' `r .mybadge_string("regex", "darkred")` \cr
#' @param fixed a character vector of \code{fixed} patterns
#' (see \link[stringi]{about_search_fixed}). \cr
#' These should give fixed expressions that match to
#' the names of the functions or objects appearing in the script,
#' to expose to the current environment. \cr
#' For example, to expose the following methods to the current environment, \cr
#' \code{mymethod.numeric()} and \code{mymethod.character()} from generic \code{mymethod()}, \cr
#' one could specify \code{fixed = "mymethod"}. \cr
#' `r .mybadge_string("fixed", "darkgreen")` \cr
#'
#'
#' @details
#' One can specify which objects to expose using arguments
#' \code{select}, \code{regex}, or \code{fixed}. \cr
#' The user can specify all 3 of them, but at least one of the 3 must be specified. \cr
#' It is not a problem if the specifications overlap.
#'
#'
#' @returns
#' Any specified objects will be placed
#' in the current environment
#' (like the Global environment, or the environment within a function). \cr
#' \cr
#'
#' @seealso \link{tinycodet_misc}, [base::source()]
#'
#'
#' @examples
#'
#' exprs <- expression({
#' helloworld = function()print("helloworld")
#' goodbyeworld <- function() print("goodbye world")
#' `%s+test%` <- function(x,y) stringi::`%s+%`(x,y)
#' `%s*test%` <- function(x,y) stringi::`%s*%`(x,y)
#'  mymethod <- function(x) UseMethod("mymethod", x)
#'  mymethod.numeric <- function(x)x * 2
#'  mymethod.character <- function(x)chartr(x, old = "a-zA-Z", new = "A-Za-z")
#' })
#'
#' source_selection(list(exprs=exprs), regex = "^mymethod")
#' mymethod(1)
#' mymethod("a")
#'
#'
#' temp.fun <- function(){
#'   source_selection(list(exprs=exprs), regex = "^mymethod", fixed = c("%", ":="))
#'   ls() # list all objects residing within the function definition
#' }
#' temp.fun()
#'
#' temp.fun <- function(){
#'   source_selection(list(exprs=exprs), select = c("helloworld", "goodbyeworld"))
#'   ls() # list all objects residing within the function definition
#' }
#' temp.fun()
#'
#'

#' @name source_selection
#' @export
source_selection <- function(
    lst, select=NULL, regex = NULL, fixed = NULL
) {

  # check lst:
  if(!is.list(lst)) {
    stop("`lst` must be a list")
  }
  if(length(lst) == 0) {
    stop("`lst` is of length 0")
  }
  if(any(names(lst) %in% c("local"))) {
    stop(paste0(
      "Do not supply the `local` argument:",
      "\n",
      "Environment is already specified to be the current environment"
    ))
  }

  # check args:
  if(missing(select) && missing(regex) && missing(fixed)) {
    stop("You must specified at least one of `select`, `regex`, or `fixed`")
  }

  # check selections:
  temp.fun <- function(x, abortcall) {
    error.txt <- paste0(
      "invalid `", substitute(x), "` argument given"
    )
    if(!is.null(x)) {
      if(!is.character(x) || length(x) == 0) {
        stop(simpleError(error.txt, call = abortcall))
      }
      if(any(!nzchar(x))) {
        stop(simpleError(error.txt, call = abortcall))
      }
    }
  }
  temp.fun(select, sys.call())
  temp.fun(regex, sys.call())
  temp.fun(fixed, sys.call())

  # get source:
  message("Sourcing script ... \n")
  tempenv <- new.env(parent=parent.frame())
  do.call(source, c(lst, local=tempenv))
  tempenv <- as.list(tempenv, all.names = TRUE, sorted = TRUE)

  # get selections:
  if(!is.null(select)) {
    wrong.select <- select[!select %in% names(tempenv)]
    if(length(wrong.select) > 0) {
      stop(
        "The following selections not in the source:",
        "\n",
        paste(wrong.select, collapse = ", ")
      )
    }
    for (i in seq_along(select)) {
      attr(tempenv[[select[i]]], "env") <- tempenv
      assign(select[[i]], tempenv[[select[i]]], envir = parent.frame(n = 1))
    }
  }

  # get patterns:
  if(!is.null(regex) || !is.null(fixed)) {
    fun_names <- names(tempenv)[unlist(lapply(tempenv, is.function))]
    methodnames <- .source_getmethodnames(fun_names, regex, fixed)

    if(length(methodnames) == 0) {
      warning("no appropriate matches found in sourced script")
    }
    if(length(methodnames) > 0) {
      # methods_used <- TRUE
      for(meth in methodnames) {
        attr(tempenv[[meth]], "env") <- tempenv
        assign(meth, tempenv[[meth]], envir = parent.frame(n = 1))
      }
    }
  }
  message("Done")
}

.source_getmethodnames <- function(fun_names, regex, fixed) {
  if(!is.null(regex)) {
    regex_names <- lapply(
      regex,
      FUN = \(x)stringi::stri_subset(fun_names, regex=x)
    ) |> unlist() |> as.character() |> unique()
    methodnames <- regex_names
  }
  if(!is.null(fixed)) {
    fixed_names <- lapply(
      fixed,
      FUN = \(x)stringi::stri_subset(fun_names, fixed=x)
    ) |> unlist() |> as.character() |> unique()
    methodnames <- fixed_names
  }
  if(!is.null(regex) && !is.null(fixed)) {
    methodnames <- c(regex_names, fixed_names) |> unique()
  }

  return(methodnames)

}
