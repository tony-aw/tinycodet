#' (Un)Expose Infix Operators From Package Namespace in the Current Environment
#'
#' @description
#' \code{import_inops(expose = ...)}
#' exposes infix operators specified
#' in a package or an alias object to the current environment
#' (like the global environment or the environment within a function). \cr
#' \cr
#' \code{import_inops(unexpose = ...)}
#' "unexposes" (i.e. removes) the infix operators specified
#' in a package or an alias object
#' from the current environment
#' (like the global environment or the environment within a function). \cr
#' Note that in this case only infix operators exposed by
#' the 'tinycodet' import system
#' will be removed from the current environment;
#' "regular" (i.e. user-specified) infix operators will not be touched. \cr
#' \cr
#'
#' @param expose,unexpose either one of the following:
#'  * an alias object as produced by the \link{import_as} function.
#'  * a string giving the package name.
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
#' See \link{import_inops.control}.
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
#'and without having to attach the infix operators globally. \cr
#' \cr
#' \cr
#' \bold{Other Details} \cr
#' The \code{import_inops()} function does not support overloading base/core R operators. \cr
#' \cr
#' When using \code{import_inops()} to remove infix operators from the current environment,
#' it will use the attributes of those operators to determine if the infix operator came from
#' the 'tinycodet' import system or not.
#' Only infix operators exposed by the 'tinycodet' import system will be removed.
#'
#'
#' @returns
#' If using argument \code{expose}: \cr
#' The infix operators specified in the given package or alias will be placed
#' in the current environment
#' (like the Global environment, or the environment within a function). \cr
#' \cr
#' If using argument \code{unexpose}: \cr
#' The infix operators specified in the given package or alias,
#' exposed by \code{import_inops()}, will be removed from the current environment
#' (like the Global environment, or the environment within a function). \cr
#' If such infix operators could not be found, this function simply returns \code{NULL}. \cr
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
    lst <- import_inops.control(...) # checks are done in here
    exclude <- lst$exclude
    include.only <- lst$include.only
    overwrite <- lst$overwrite
    inherits <- lst$inherits

    if(is.environment(expose)) {
      if(!.is.tinyalias(as.character(substitute(expose)), parent.frame(n = 1))) {
        stop("The given environment is not an alias from `import_as()`")
      }
      pkgs <- c(
        expose$.__attributes__.$re_exports.pkgs,
        expose$.__attributes__.$pkgs$packages_order
      ) |> unique()
      funs <- names(expose)[eapply(expose, is.function)|>unlist()]
      inops <- funs[stringi::stri_detect(funs, regex="%|:=")]
      if(length(inops)==0) {
        message("no infix operators in this alias")
        return(NULL)
      }
      inops <- .import_exclude_include(inops, exclude, include.only, abortcall = sys.call())
      if(length(inops)==0) {
        message("no infix operators to expose")
        return(NULL)
      }
      if(length(inops) > 0) {
        .internal_check_conflicting_inops(
          inops, overwrite, inherits, envir=parent.frame(n = 1), abortcall=sys.call()
        )
        message(
          "Placing infix operators in current environment..."
        )
        for(i in inops) {
          check_existence <- .is.tinyLL(i, env = parent.frame(n = 1))
          if(isTRUE(check_existence)) {
            rm(list = i, envir = parent.frame(n = 1))
          }
          assign(i, expose[[i]], envir = parent.frame(n = 1))
        }
        message("Done")
      }
    }

    if(is.character(expose)) {
      if(length(expose)>1) {
        stop("`expose` must be a package name (string) or an alias from `import_as()`")
      }
      
      .import_inops_main(
        expose, lib.loc = lib.loc,
        env=parent.frame(n = 1), abortcall = sys.call(),
        ...
      )
    }

  }

  # unexpose:
  if(!is.null(unexpose)) {
    if(is.environment(unexpose)) {
      if(!.is.tinyalias(as.character(substitute(unexpose)), parent.frame(n = 1))) {
        stop("The given environment is not an alias from `import_as()`")
      }
      pkgs <- c(
        unexpose$.__attributes__.$re_exports.pkgs,
        unexpose$.__attributes__.$pkgs$packages_order
      ) |> unique()
      funs <- names(unexpose)[eapply(unexpose, is.function)|>unlist()]
      inops <- funs[stringi::stri_detect(funs, regex="%|:=")]
      if(length(inops)==0) {
        message("no infix operators in this alias")
        return(NULL)
      }
      inops <- intersect(inops, utils::lsf.str(envir = parent.frame(n = 1)))
      if(length(inops)==0) {
        message("no infix operators to unexpose")
        return(NULL)
      }
      inops.get <- mget(inops, envir = parent.frame(n = 1), inherits = FALSE)
      checks <- .is.tinyinops(names(inops.get), pkgs, parent.frame(n = 1))
      inops.get <- inops.get[checks]
      if(length(inops.get)==0) {
        message("No infix operators from alias to unexpose")
        return(NULL)
      }
      message("unexposing infix operators...")
      for(i in names(inops.get)) rm(list=i, envir = parent.frame(n = 1))
      message("Done")
    }
  }
    if(is.character(unexpose)) {
      if(length(unexpose)>1) {
        stop("`unexpose` must be a package name (string) or an alias from `import_as()`")
      }
      .internal_check_pkgs(pkgs=unexpose, lib.loc=lib.loc, pkgs_txt = "packages", abortcall=sys.call())
      .import_inops_delete(
        unexpose, lib.loc = lib.loc,
        env=parent.frame(n = 1), abortcall = sys.call()
      )
    }
}


#' @keywords internal
#' @noRd
.import_inops_main <- function(
    package, lib.loc, env, abortcall, ...
){
  
  
  # check library:
  .internal_check_lib.loc(lib.loc, abortcall)
  
  
  # check package
  .internal_check_pkgs(
    pkgs=package, lib.loc=lib.loc, pkgs_txt = "packages", abortcall=sys.call()
  )

  # check additional arguments:
  lst <- import_inops.control(...) # checks are done in here
  exclude <- lst$exclude
  include.only <- lst$include.only
  overwrite <- lst$overwrite
  inherits <- lst$inherits

  # FUNCTION:
  ns <- .internal_prep_Namespace(package, lib.loc, abortcall = sys.call())
  
  
  export_names <-  grep("%|:=", names(ns), value=TRUE)

  if(length(export_names)==0){
    message("no infix operators in this package")
    return(NULL)
  }

  operators <- grep("%|:=", names(ns), value=TRUE)
  operators <- .import_exclude_include(operators, exclude, include.only, abortcall = sys.call())

  if(isTRUE(length(operators)==0)){
    message("No infix operators to expose...")
    return(NULL)
  }

  if(isTRUE(length(operators) > 0)) {
    .internal_check_conflicting_inops(
      operators, overwrite, inherits, envir=env, abortcall=abortcall
    )

    message(
      "Placing infix operators in current environment..."
    )
    for(op in operators){
      check_existence <- .is.tinyLL(op, env = env)
      if(isTRUE(check_existence)) {
        rm(list = op, envir = env)
      }
      class(ns[[op]]) <- c("function", "tinyimport")
      assign(op, ns[[op]], envir = env)

    }
    message("Done")

  }
}


#' @keywords internal
#' @noRd
.import_inops_delete <- function(delete, lib.loc, env, abortcall) {
  message(
    "checking for infix operators exposed to the current environment by `import_inops()` ..."
  )


  # get functions
  all.funs <- mget(utils::lsf.str(envir = env), envir = env, inherits = FALSE)
  if(length(all.funs) == 0) {
    message("No infix operators from `import_inops()` to delete")
    return(NULL)
  }

  # get infix operators
  all.ops <- all.funs[grep("%|:=", names(all.funs), value = TRUE)]
  if(length(all.ops) == 0) {
    message("No infix operators from `import_inops()` to delete")
    return(NULL)
  }

  # get tinyimport objects from selected packages
  checks <- .is.tinyinops(names(all.ops), delete, env)
  tinyops <- all.ops[checks]
  if(length(tinyops) == 0) {
    message("No infix operators from selected packages to delete")
    return(NULL)
  }

  # get tinyinops from selected packages
  tinyops <- tinyops[sapply(tinyops, FUN = \(x).internal_get_packagename(x) %in% delete)]
  if(length(tinyops) == 0) {
    message("No infix operators from selected packages to delete")
    return(NULL)
  }

  # MAIN FUNCTION:
  tinyops <- names(tinyops)
  message(paste0(
    "Removing the following infix operators:",
    "\n",
    paste0(tinyops, collapse = ", ")
  ))
  for(i in tinyops) rm(list=i, envir = env)
  message("Done")

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
      "ALL prepared infix operators already exist in the current environment",
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
  temp.fun <- function(nm, pkgs, env) {
    if(!exists(as.character(nm), envir = env, inherits = FALSE)) {
      return(FALSE)
    }
    obj <- get(as.character(nm), envir = env)
    checks <- c(
      isTRUE(is.function(obj)),
      isTRUE(grepl("%|:=", nm))
    )
    if(any(!checks)) {
      return(FALSE)
    }
    check_class <- isTRUE(all(class(obj) %in% c("function", "tinyimport")))
    if(!check_class) {
      return(FALSE)
    }
    package_name <- .internal_get_packagename(obj)
    if(is.null(package_name)){
      return(FALSE)
    }
    pkgs_core <- .internal_list_coreR()
    checks <- c(
      isTRUE(package_name %in% pkgs),
      isFALSE(package_name %in% pkgs_core)
    )
    if(any(!checks)) {
      return(FALSE)
    }
    check <- isTRUE(as.character(attr(obj, "function_name")) == nm) # was nms[i]
    if(check) {
      return(TRUE)
    }
  }
  checks <- rep_len(FALSE, length(nms))
  for (i in seq_along(nms)) {
    checks[i] <- temp.fun(nms[i], pkgs, env)
  }
  return(checks)
}


