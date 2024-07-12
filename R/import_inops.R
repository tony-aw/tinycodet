#' (Un)Expose Infix Operators From Package Namespace in the Current Environment
#'
#' @description
#' \code{import_inops(expose = ...)}
#' exposes infix operators specified
#' in a package or an alias object to the current environment. \cr
#' \cr
#' \code{import_inops(unexpose = ...)}
#' "unexposes" (i.e. removes) the infix operators specified
#' in a package or an alias object
#' from the current environment. \cr
#' Note that in this case only infix operators exposed by
#' the 'tinycodet' import system
#' will be removed from the current environment;
#' "regular" (i.e. user-defined) infix operators will not be touched. \cr
#' \cr
#' To attach all infix operators from a package to the global namespace,
#' one can use the \link{pkg_lsf} function like so: 
#' 
#' ```{r echo = TRUE, eval = FALSE}
#' y <- pkg_lsf("packagename", type = "inops")
#' library(packagename, include.only = y)
#' ```
#'
#' @param expose,unexpose either one of the following:
#'  * an alias object as produced by the \link{import_as} function.
#'  * a string giving the package name. Core R (i.e. "base", "stats", etc.) is not allowed.
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through). \cr
#' Only used when supplying a string to
#' \code{expose} / \code{unexpose},
#' and ignored when supplying an alias object to
#' \code{expose} / \code{unexpose}
#' (the library is path already stored inside the alias object). \cr
#' The \code{lib.loc} argument would usually be \code{.libPaths()}. \cr
#' See also \link[base]{loadNamespace}.
#' @param ... additional arguments,
#' only relevant if the \code{expose} argument is used. \cr
#' See \link{import_inops.control}. \cr \cr
#'
#'
#' @details
#' \bold{Why Exposing Infix Operators Is Useful} \cr
#' To use a function from an R-package,
#' while avoiding the disadvantages of attaching a package
#' (see \link{tinycodet_import}),
#' one would traditionally use the \link[base]{::} operator like so:
#'
#' ```{r eval = FALSE}
#' packagename::function_name()
#' ```
#'
#' This is, however, cumbersome with infix operators,
#' as it forces one to code like this:
#'
#' ```{r eval = FALSE}
#' packagename::`%op%`(x,y)
#' ```
#'
#' Exposing infix operators to the current environment,
#' using the \code{import_inops()} function,
#' allows one to use infix operators without using cumbersome code,
#' and without having to attach the infix operators globally. \cr
#' \cr
#' \cr
#' \bold{Other Details} \cr
#' The \code{import_inops()} function does not support overloading base/core R operators. \cr
#' \cr
#' When using \code{import_inops()} to remove infix operators from the current environment,
#' it will use the attributes of those operators to determine if the infix operator came from
#' the 'tinycodet' import system or not.
#' Only infix operators exposed by the 'tinycodet' import system will be removed. \cr \cr
#'
#'
#' @returns
#' If using argument \code{expose}: \cr
#' The infix operators specified in the given package or alias will be placed
#' in the current environment. \cr
#' \cr
#' If using argument \code{unexpose}: \cr
#' The infix operators specified in the given package or alias,
#' exposed by \code{import_inops()}, will be removed from the current environment. \cr
#' If such infix operators could not be found, this function simply returns \code{NULL}. \cr \cr
#' 
#'
#' @seealso \link{tinycodet_import}, [import_inops.control()], [report_inops()]
#'
#'
#' @examples
#'
#' import_inops(expose = "stringi") # expose infix operators from package
#' import_inops(unexpose = "stringi") # remove the exposed infix operators from environment
#'
#' import_as(~ stri., "stringi")
#' import_inops(expose = stri.) # expose infix operators from alias
#' import_inops(unexpose = stri.) # unexposed infix operators from current environment
#'
#'
#' # additional arguments (only used when exposing, not unexposing):
#' import_inops(expose = "stringi", exclude = "%s==%")
#' import_inops(unexpose = "stringi")
#' import_inops(expose = "stringi", overwrite = FALSE)
#' import_inops(unexpose = "stringi")
#'
#' import_as(~ stri., "stringi")
#' import_inops(expose = stri., include.only = "%s==%")
#' import_inops(unexpose = stri.)
#' import_inops(expose = stri., overwrite = FALSE)
#' import_inops(unexpose = stri.)
#'
#'
#'
#'

#' @rdname import_inops
#' @export
import_inops <- function(
    expose=NULL, unexpose=NULL, lib.loc=.libPaths(), ...
) {

  # check expose/unexpose:
  if(!is.null(expose) && !is.null(unexpose)) {
    stop("Can only specify either `expose` or `unexpose`, not both")
  }
  if(!is.null(expose)) {
    if(!is.character(expose) && !is.environment(expose)) {
      stop("`expose` must be a package name (string) or an alias from `import_as()`")
    }
  }
  if(!is.null(unexpose)) {
    if(!is.character(unexpose) && !is.environment(unexpose)) {
      stop("`unexpose` must be a package name (string) or an alias from `import_as()`")
    }
  }

  # check library:
  if(is.character(expose) || is.character(unexpose)){
    .internal_check_lib.loc(lib.loc, sys.call())
  }


  # expose:
  if(!is.null(expose)) {
    # check additional arguments:
    lst_opts <- import_inops.control(...) # checks are done in here
    
    if(is.environment(expose)) {
      if(!.is.tinyalias(as.character(substitute(expose)), parent.frame(n = 1))) {
        stop("The given environment is not an alias from `import_as()`")
      }
      .import_inops_expose_alias(
        expose, parent.frame(n = 1), lst_opts, abortcall = sys.call()
      )
    }
    
    if(is.character(expose)) {
      if(length(expose)>1) {
        stop("`expose` must be a package name (string) or an alias from `import_as()`")
      }
      
      .import_inops_expose_package(
        expose, lib.loc, parent.frame(n = 1), lst_opts, abortcall = sys.call()
      )
    }
    
  }

  # unexpose:
  if(!is.null(unexpose)) {
    if(is.environment(unexpose)) {
      if(!.is.tinyalias(as.character(substitute(unexpose)), parent.frame(n = 1))) {
        stop("The given environment is not an alias from `import_as()`")
      }
      .import_inops_unexpose_alias(unexpose, parent.frame(n = 1), sys.call())
    }
    if(is.character(unexpose)) {
      if(length(unexpose)>1) {
        stop("`unexpose` must be a package name (string) or an alias from `import_as()`")
      }
      .import_inops_unexpose_package(unexpose, lib.loc, parent.frame(n = 1), sys.call())
    }
  }
}




#' @keywords internal
#' @noRd
.internal_check_conflicting_inops <- function(operators, overwrite, inherits, envir, abortcall) {
  message("Checking for conflicting infix operators in the current environment...")
  check_existing <- sapply(
    operators, \(x)exists(x, envir = envir, inherits = inherits, mode = "function")
  )
  all_conflicting <- sum(check_existing) == length(operators)

  if(sum(check_existing) > 0 && !all_conflicting) {
    conflict.txt <- paste0(
      "The following infix operators already exist in the current environment:",
      "\n",
      paste0(operators[check_existing], collapse = ", "),
      "\n"
    )
  }
  if(sum(check_existing) > 0 && all_conflicting) {
    conflict.txt <- paste0(
      "ALL infix operators already exist in the current environment",
      "\n"
    )
  }
  if(isTRUE(overwrite) && sum(check_existing) > 0) {
    message(simpleMessage(
      paste0(conflict.txt, "Overwriting existing infix operators...", "\n"),
      call = abortcall
    ))
    warning(simpleWarning(
      paste0(conflict.txt, "Attempted overwrite existing infix operators", "\n"),
      call = abortcall
    ))
  }
  if(isFALSE(overwrite) && sum(check_existing) > 0) {
    stop(simpleError(
      paste0(conflict.txt, "Function halted"), call = abortcall
    ))
  }
}


#' @keywords internal
#' @noRd
.is.tinyinops <- function(nms, pkgs, env) {
  if(!is.character(nms)){
    stop("`nms` must be a character vector")
  }

  if(missing(nms) || missing(pkgs) || missing(env)) {
    stop("not all arguments given in `.is.tinyinops()`")
  }
  
  checks <- rep_len(FALSE, length(nms))
  for (i in seq_along(nms)) {
    nms.current <- nms[i]
    package_name <- attr(env[[nms.current]], "package")
    if(is.null(package_name) || !is.character(package_name)) {
      checks[i] <- FALSE
    } else {
      checks[i] <- .is.tinyinop(nms.current, env) && package_name %in% pkgs
    }
  }
  return(checks)
}


#' @keywords internal
#' @noRd
.is.tinyinop <- function(nm, env) {
  if(!exists(as.character(nm), envir = env, inherits = FALSE)) {
    return(FALSE)
  }
  obj <- get(as.character(nm), envir = env)
  checks <- c(
    isTRUE(is.function(obj)),
    isTRUE(.internal_grep_inops(nm, type = 0))
  )
  if(any(!checks)) {
    return(FALSE)
  }
  check_class <- isTRUE(all( c("function", "tinyimport") %in% class(obj)))
  if(!check_class) {
    return(FALSE)
  }
  package_name <- attr(obj, "package")
  if(is.null(package_name) || !is.character(package_name)) {
    return(FALSE)
  }
  if(package_name == "base") {
    return(FALSE)
  }
  function_name <- attr(obj, "function_name")
  if(is.null(function_name) || !is.character(function_name)){
    return(FALSE)
  }
  if(function_name != nm) {
    return(FALSE)
  } 
  return(TRUE)
}
