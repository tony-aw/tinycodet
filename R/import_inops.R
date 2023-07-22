#' Expose infix operators to the current environment
#'
#' @description
#' The \code{import_inops()} function
#' exposes the infix operators of the specified packages to the current environment
#' (like the global environment, or the environment within a function). \cr
#' To ensure the user can still verify which operator function came from which package,
#' a "package" attribute is added to each exposed operator. \cr
#' If you wish to globally attach infix operators,
#' instead of just placing them in the current environment,
#' see \link{pkg_lsf}. \cr
#'
#'
#' @param pkgs a character vector of package name(s)
#' from which to load and expose infix operators. \cr
#' NOTE: The order of the character vector matters!
#' If multiple packages share infix operators with the same name,
#' the conflicting operators of the package named last
#' will overwrite those of the earlier named packages.
#' @param exclude a character vector,
#' giving the infix operators NOT to expose to the current environment. \cr
#' This can be handy to prevent overwriting any (user defined)
#' infix operators already present in the current environment. \cr
#' @param include.only a character vector,
#' giving the infix operators to expose to the current environment,
#' and the rest of the operators will not be exposed. \cr
#' This can be handy to prevent overwriting any (user defined)
#' infix operators already present in the current environment. \cr
#' @param overwrite logical,
#' indicating if it is allowed to overwrite existing infix operators. \cr
#'  * If \code{TRUE} (default), a warning is given when operators existing in the current environment
#'  are being overwritten,
#'   but the function continuous nonetheless.
#'  * If \code{FALSE}, an error is produced
#'  when the to be exposed operators already exist in the current environment,
#'  and the function is halted.
#' @param inherits logical; when \code{overwrite=FALSE},
#' should enclosed environments,
#' especially package namespaces,
#' also be taken into account? \cr
#' Defaults to \code{FALSE}. \cr
#' See also \link[base]{exists}.
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through). \cr
#' This is usually \code{.libPaths()}. \cr
#' See also \link[base]{loadNamespace}.
#' @param delete a character vector of package name(s)
#' from which to remove infix operators. \cr
#' Normally, the \code{import_inops()} function
#' exposes infix operators from packages \code{pkgs} to the current environment. \cr
#' But if the user specifies package names in the \code{delete} argument,
#' the \code{import_inops()} function will
#' ignore all other arguments, and simply delete all infix operators from the named packages
#' exposed by the \code{import_inops()} function. \cr
#' Infix operators manually defined by the user themselves will not be touched.
#'
#'
#' @details
#' \bold{On the \code{exclude} and \code{include.only} arguments}:\cr
#' You cannot specify both the \code{exclude} and \code{include.only} arguments.
#' Only one or the other, or neither. \cr
#' \cr
#' \bold{On the \code{pkgs} and \code{delete} arguments}: \cr
#' You cannot specify both the \code{pkgs} and \code{delete} arguments.
#' Only one or the other. \cr
#' Unlike the \link{import_as} function, the \code{import_inops()} function does
#' not require the packages to be necessarily related to each other. \cr
#' \cr
#' \bold{Other details}: \cr
#' The \code{import_inops()} function does not support overloading base/core R operators,
#' so don't try. \cr
#' \cr
#'
#'
#' @returns
#' If \code{pkgs} is specified: \cr
#' The infix operators from the specified packages will be placed
#' in the current environment
#' (like the Global environment, or the environment within a function). \cr
#' The infix operators will be LOCKED.
#' For information on the binding lock used, see \link{import_lock}. \cr
#' \cr
#' If \code{delete} is specified: \cr
#' The infix operators from the packages specified in \code{delete},
#' exposed by \code{import_inops()}, will be deleted. \cr
#' If such infix operators could not be found, this function returns \code{NULL}. \cr
#'
#' @seealso [tinyoperations_import()]
#'
#'
#' @examples
#' \dontrun{
#' import_inops("data.table") # expose infix operators
#' import_inops(delete="data.table") # remove the exposed infix operators.
#' }
#'
#'

#' @rdname import_inops
#' @export
import_inops <- function(
    pkgs, lib.loc=.libPaths(), exclude, include.only, overwrite=TRUE, inherits=FALSE,
    delete
) {

  # check args:
  if(!missing(pkgs) & !missing(delete)) {
    stop("cannot specify both `pkgs` and `delete`")
  }

  # check delete:
  if(!missing(delete) & missing(pkgs)) {
    .import_inops_delete(
      delete, lib.loc = lib.loc, env=parent.frame(n=1), abortcall = sys.call()
    )
  }

  if(missing(delete) & !missing(pkgs)) {
    .import_inops_main(
      pkgs, lib.loc = lib.loc,
      exclude, include.only, overwrite, inherits,
      env=parent.frame(n = 1), abortcall = sys.call()
    )
  }
}


#' @keywords internal
#' @noRd
.import_inops_main <- function(
    pkgs, lib.loc, exclude, include.only, overwrite, inherits, env, abortcall
){

  # check library:
  if(length(lib.loc)<1 | !is.character(lib.loc)) {
    stop(simpleError(
      "`lib.loc` must be a character vector with at least one library path",
      call = abortcall
    ))
  }

  # check packages:
  .internal_check_pkgs(pkgs=pkgs, lib.loc=lib.loc, pkgs_txt = "packages", abortcall=abortcall)

  # check exclude and include.only:
  if(!missing(exclude) & !missing(include.only)){
    stop(simpleError(
      "canntot specify both `exclude` and `include.only`; specify only one or none.",
      call = abortcall
    ))
  }

  # check overwrite:
  if(!isTRUE(overwrite) & !isFALSE(overwrite)) {
    stop(simpleError(
      "`overwrite` must be either `TRUE` or `FALSE`",
      call = abortcall
    ))
  }

  # check inherits:
  if(!isTRUE(inherits) & !isFALSE(inherits)) {
    stop(simpleError(
      "`inherits` must be either `TRUE` or `FALSE`",
      call = abortcall
    ))
  }

  # FUNCTION:
  export_names_all <- character()
  export_names_allconflicts <- character()
  namespaces <- list()

  for (i in 1:length(pkgs)) {
    namespace_current <- .internal_prep_Namespace(pkgs[i], lib.loc)
    export_names_current <-  grep("%|:=", names(namespace_current), value=TRUE)

    if(length(export_names_current)==0){
      message("no infix operators in this package; skipping...")
      namespace_current <- NULL
    }

    if(length(export_names_current)>0) {

      export_names_intersection <- intersect(export_names_current, export_names_all)

      if(i==1){
        message("Getting infix operators from package: ", pkgs[i], "...")
      }
      if(length(export_names_intersection)==0 & i>1) {
        message("Getting infix operators from package: ", pkgs[i], "... no conflicts")
      }
      if(length(export_names_intersection)>0) {
        message(
          "Getting infix operators from package: ",
          pkgs[i],
          "... The following infix operators detected:",
          "\n",
          paste0(export_names_intersection, collapse = ", "),
          "\n",
          pkgs[i], " will overwrite conflicting infix operators from previous imported packages..."
        )
      }
      export_names_allconflicts <- c(export_names_allconflicts, export_names_intersection)
      export_names_all <- c(export_names_all, export_names_current)
      namespaces <- utils::modifyList(namespaces, namespace_current)
      message("")
    }
  }

  operators <- grep("%|:=", names(namespaces), value=TRUE)
  if(!missing(exclude)){operators <- setdiff(operators, exclude)}
  if(!missing(include.only)){operators <- intersect(operators, include.only)}

  if(isTRUE(length(operators)==0)){
    message(
      "No operators to expose..."
    )
  }

  if(isTRUE(length(operators)>0)) {
    operators <- .internal_check_conflicting_inops(
      operators, overwrite, inherits, envir=env, abortcall=abortcall
    )
  }

  if(isTRUE(length(operators)>0)) {

    operators_to_delete <- operators[.is.tinyinops(operators, pkgs, env)]
    if(isTRUE(length(operators_to_delete)>0)) {
      message("deleting & overwriting existing infix operarors exposed by `import_inops()`")
      nms <- operators_to_delete
      for (i in 1:length(nms)) {
        if(exists(nms[i], envir = env, inherits = FALSE)) {
          outside_obj <- get(as.character(nms[i]), envir = env)
          rm(list = nms[i], envir = env)
        }
      }
    }


    message(
      "Placing infix operators in current environment..."
    )
    for(op in operators){
      assign(op, namespaces[[op]], envir = env)
      lockBinding(as.character(op), env = env)
    }
    message("Done")

  }
}


#' @keywords internal
#' @noRd
.import_inops_delete <- function(delete, lib.loc, env, abortcall) {
  if(!is.character(delete)) {
    stop(simpleError(
      "`delete` must be a character vector of packagenames",
      call = abortcall
    ))
  }
  .internal_check_pkgs(pkgs=delete, lib.loc=lib.loc, pkgs_txt = "packages", abortcall=abortcall)
  message(
    "checking for infix operators exposed to the current environment by `import_inops()` ..."
  )

  # functions -> inops
  all.funs <- mget(utils::lsf.str(envir = env), envir = env)
  if(length(all.funs)==0) {
    message("No infix operators from `import_inops()` to delete")
    return(NULL)
  }
  all.ops <- all.funs[grep("%|:=", names(all.funs), value = TRUE)]

  # inops -> inops with ns
  if(length(all.ops)==0) {
    message("No infix operators from `import_inops()` to delete")
    return(NULL)
  }
  all.ops <- all.ops[sapply(all.ops, FUN = \(x)isNamespace(environment(x)))]

  # inops with ns -> tinyinops
  if(length(all.ops)==0) {
    message("No infix operators from `import_inops()` to delete")
    return(NULL)
  }
  checks <- .is.tinyinops(names(all.ops), delete, env)
  tinyops <- all.ops[checks]

  # tinyinops -> tinyinops from selected packages
  tinyops <- tinyops[sapply(tinyops, FUN = \(x).internal_get_packagename(x) %in% delete)]
  if(length(tinyops)==0) {
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
.is.tinyinops <- function(nms, pkgs, env) {
  if(missing(nms)|missing(pkgs)|missing(env)) {
    stop("not all arguments given in `.is.tinyinops()`")
  }
  checks <- rep(FALSE, length(nms))
  for (i in 1:length(nms)) {
    if(exists(as.character(nms[i]), envir = env, inherits = FALSE)) {
      obj <- get(as.character(nms[i]), envir = env)
      check1 <- length(names(attributes(obj))) == 3
      check2 <- ifelse(
        check1,
        isTRUE(all(names(attributes(obj)) == c("package", "function_name", "tinyimport"))),
        FALSE
      )
      check3 <- isTRUE(is.function(obj))
      check4 <- isTRUE(bindingIsLocked(as.character(nms[i]), env = env))
      checks[i] <- check1 & check2 & check3 & check4
    }
  }
  return(checks)
}

#' @keywords internal
#' @noRd
.internal_check_conflicting_inops <- function(operators, overwrite, inherits, envir, abortcall) {
  message("Checking for conflicting infix operators in the current environment...")
  check_existing <- sapply(
    operators, \(x)exists(x, envir = envir, inherits = inherits, mode = "function")
  )
  all_conflicting <- sum(check_existing) == length(operators)

  if(sum(check_existing)>0 & !all_conflicting) {
    conflict.txt <- paste0(
      "The following infix operators already exist in the current environment:",
      "\n",
      paste0(operators[check_existing], collapse = ", "),
      "\n"
    )
  }
  if(sum(check_existing)>0 & all_conflicting) {
    conflict.txt <- paste0(
      "ALL prepared infix operators already exist in the current environment",
      "\n"
    )
  }
  if(isTRUE(overwrite) & sum(check_existing)>0) {
    message(simpleMessage(
      paste0(conflict.txt, "Overwriting existing infix operators...", "\n"),
      abortcall
    ))
    warning(simpleWarning(
      paste0(conflict.txt, "Existing infix operators have been overwritten...", "\n"),
      abortcall
    ))
  }
  if(isFALSE(overwrite) & sum(check_existing)>0) { stop(simpleError(
    paste0(conflict.txt, "Function halted"), abortcall
  ))}
  return(operators)
}

