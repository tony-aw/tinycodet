#' (un)expose infix operators in the current environment
#'
#' @description
#' \code{import_inops(expose=...)}
#' exposes infix operators specified in packages or a packages alias to the current environment
#' (like the global environment or the environment within a function). \cr
#' \cr
#' \code{import_inops(unexpose=...)}
#' unexposes (i.e. removes) the infix operators specified in packages or a package alias
#' from the current environment
#' (like the global environment or the environment within a function). \cr
#' Note that in this case only infix operators exposed by
#' \code{import_inops()}
#' will be removed from the current environment; \cr
#' "regular" infix operators (i.e. user-specified) will not be touched. \cr
#' \cr
#'
#' @param expose either one of the following:
#'  * a package alias as produced by the \link{import_as} function.
#'  * a character vector of package name(s),
#'  specifying the packages from which to load infix operators,
#'  and place them in the current environment. \cr
#'  Note: The order of the character vector matters!
#'  If multiple packages share infix operators with the same name,
#'  the conflicting operators of the package named last
#'  will overwrite those of the earlier named packages.
#' @param unexpose either one of the following:
#'  * a package alias as produced by the \link{import_as} function.
#'  * a character vector of package name(s).
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through). \cr
#' This is usually \code{.libPaths()}. \cr
#' See also \link[base]{loadNamespace}.
#' @param ... additional arguments,
#' only relevant if the \code{expose} argument is used. \cr
#' See \link{import_inops.control}.
#'
#'
#' @details
#' Unlike the \link{import_as} function, the \code{import_inops()} function does
#' not require the packages to be necessarily related to each other. \cr
#' \cr
#' The \code{import_inops()} function does not support overloading base/core R operators,
#' so don't try. \cr
#' \cr
#' When using \code{import_inops()} to remove infix operators from the current environment,
#' it will use the attributes of those operators to determine if the infix operator came from
#' \code{import_inops()}, or if they were user-defined.
#'
#'
#' @returns
#' If using argument \code{expose}: \cr
#' The infix operators specified in the given packages or package alias will be placed
#' in the current environment
#' (like the Global environment, or the environment within a function). \cr
#' \cr
#' If using argument \code{unexpose}: \cr
#' The infix operators specified in the given packages or package alias,
#' exposed by \code{import_inops()}, will be deleted. \cr
#' If such infix operators could not be found, this function returns \code{NULL}. \cr
#'
#' @seealso [tinyoperations_import()], [import_inops.control()]
#'
#'
#' @examples
#' \dontrun{
#' import_as(dt., "data.table")
#' import_inops(expose = dt.) # expose infix operators from alias
#' import_inops(unexpose = dt.) # unexposed infix operators from current environment
#' import_inops(expose = "data.table") # expose infix operators from package
#' import_inops(unexpose = "data.table") # remove the exposed infix operators from environment
#' }
#'
#'

#' @rdname import_inops
#' @export
import_inops <- function(
    expose=NULL, unexpose=NULL, lib.loc=.libPaths(), ...
) {

  # check expose/unexpose:
  if(!is.null(expose) & !is.null(unexpose)) {
    stop("Can only specify either `expose` or `unexpose`, not both")
  }
  if(!is.null(expose)) {
    if(!is.character(expose) & !is.environment(expose)) {
      stop("`expose` must be a character vector of package names or an alias from `import_as()`")
    }
  }
  if(!is.null(unexpose)) {
    if(!is.character(unexpose) & !is.environment(unexpose)) {
      stop("`unexpose` must be a character vector of package names or an alias from `import_as()`")
    }
  }

  # check library:
  if(length(lib.loc)<1 | !is.character(lib.loc)) {
    stop(
      "`lib.loc` must be a character vector with at least one library path"
    )
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
        stop("`expose` is not an alias from `import_as()`")
      }
      pkgs <- c(
        expose$.__attributes__.$re_exports.pkgs,
        expose$.__attributes__.$packages_order
      ) |> unique()
      .internal_check_pkgs(
        pkgs=pkgs, lib.loc=lib.loc, pkgs_txt = "packages", abortcall=sys.call()
      )
      funs <- names(expose)[eapply(expose, is.function)|>unlist()]
      inops <- funs[stringi::stri_detect(funs, regex="%|:=")]
      if(length(inops)==0) {
        message("no infix operators in this alias")
        return(NULL)
      }
      if(!is.null(exclude)){inops <- setdiff(inops, exclude)}
      if(!is.null(include.only)){inops <- intersect(inops, include.only)}
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
          assign(i, expose[[i]], envir = parent.frame(n = 1))
        }
      }
    }

    if(is.character(expose)) {
      .internal_check_pkgs(
        pkgs=expose, lib.loc=lib.loc, pkgs_txt = "packages", abortcall=sys.call()
      )
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
        stop("`unexpose` is not an alias from `import_as()`")
      }
      pkgs <- c(
        unexpose$.__attributes__.$re_exports.pkgs,
        unexpose$.__attributes__.$packages_order
      ) |> unique()
      .internal_check_pkgs(pkgs=pkgs, lib.loc=lib.loc, pkgs_txt = "packages", abortcall=sys.call())
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
      checks <- .is.tinyinops(names(inops.get), pkgs, parent.frame(n = 1), lib.loc)
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
    pkgs, lib.loc, env, abortcall, ...
){

  # check additional arguments:
  lst <- import_inops.control(...) # checks are done in here
  exclude <- lst$exclude
  include.only <- lst$include.only
  overwrite <- lst$overwrite
  inherits <- lst$inherits

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
  if(!is.null(exclude)){operators <- setdiff(operators, exclude)}
  if(!is.null(include.only)){operators <- intersect(operators, include.only)}

  if(isTRUE(length(operators)==0)){
    message(
      "No operators to expose..."
    )
  }

  if(isTRUE(length(operators)>0)) {
    .internal_check_conflicting_inops(
      operators, overwrite, inherits, envir=env, abortcall=abortcall
    )

    message(
      "Placing infix operators in current environment..."
    )
    for(op in operators){
      class(namespaces[[op]]) <- c("function", "tinyimport")
      assign(op, namespaces[[op]], envir = env)

    }
    message("Done")

  }
}


#' @keywords internal
#' @noRd
.import_inops_delete <- function(delete, lib.loc, env, abortcall) {
  if(!is.character(delete)) {
    stop(simpleError(
      "`pkgs` must be a character vector of packagenames",
      call = abortcall
    ))
  }

  message(
    "checking for infix operators exposed to the current environment by `import_inops()` ..."
  )


  # get functions
  all.funs <- mget(utils::lsf.str(envir = env), envir = env, inherits = FALSE)
  if(length(all.funs)==0) {
    message("No infix operators from `import_inops()` to delete")
    return(NULL)
  }

  # get infix operators
  all.ops <- all.funs[grep("%|:=", names(all.funs), value = TRUE)]
  if(length(all.ops)==0) {
    message("No infix operators from `import_inops()` to delete")
    return(NULL)
  }

  # get tinyimport objects from selected packages
  checks <- .is.tinyinops(names(all.ops), delete, env, lib.loc)
  tinyops <- all.ops[checks]
  if(length(tinyops)==0) {
    message("No infix operators from selected packages to delete")
    return(NULL)
  }

  # get tinyinops from selected packages
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
      paste0(conflict.txt, "Attempted overwrite existing infix operators", "\n"),
      abortcall
    ))
  }
  if(isFALSE(overwrite) & sum(check_existing)>0) { stop(simpleError(
    paste0(conflict.txt, "Function halted"), abortcall
  ))}
}

#' @keywords internal
#' @noRd
.is.tinyinops <- function(nms, pkgs, env, lib.loc) {
  if(!is.character(nms)){
    stop("`nms` must be a character vector")
  }

  if(missing(nms)|missing(pkgs)|missing(env)|missing(lib.loc)) {
    stop("not all arguments given in `.is.tinyinops()`")
  }
  temp.fun <- function(nm, pkgs, env, lib.loc) {
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
    pkgs_core <- c(
      utils::installed.packages(priority = "base") |> rownames(),
      utils::installed.packages(lib.loc=lib.loc, priority = "base") |> rownames()
    ) |> unique()
    checks <- c(
      isTRUE(package_name %in% pkgs),
      isFALSE(package_name %in% pkgs_core)
    )
    if(any(!checks)) {
      return(FALSE)
    }
    check <- isTRUE(as.character(attr(obj, "function_name")) == nms[i])
    if(check) {
      return(TRUE)
    }
  }
  checks <- rep(FALSE, length(nms))
  for (i in 1:length(nms)) {
    checks[i] <- temp.fun(nms[i], pkgs, env, lib.loc)
  }
  return(checks)
}


