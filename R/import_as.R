#' Import R-package (and Minimal Dependencies) Under an Alias
#'
#' @description
#'
#' The \code{import_as()} function
#' imports the namespace of an R-package,
#' and optionally also its direct minimal dependencies,
#' all under the same alias.
#' The specified alias,
#' containing the exported functions from the specified packages,
#' will be placed in the specified environment. \cr
#'
#' @param main a 2-sided formula
#' (or a string that evaluates as a 2-sided formula),
#' where the left-hand side gives the alias,
#' and the right-hand side gives the main package to import under the given alias. \cr
#' Starting the alias name with a dot will hide it from `ls()`,
#' preventing accidental removal. \cr
#' For example : \cr
#' `.alias ~ packagename` \cr
#' Core R (i.e. "base", "stats", etc.) is not allowed for the main package.
#' @param re_exports \code{TRUE} or \code{FALSE}.
#'  * If \code{re_exports = TRUE} the re-exports from the main package
#'  (including those exported from Core R)
#'  are added to the alias together with the main package. \cr
#'  This is the default,
#'  as it is analogous to the behaviour of base R's \link[base]{::} operator. \cr
#'  * If \code{re_exports = FALSE},
#'  these re-exports are not added together with the main package. \cr
#'  The user can still import the packages under the alias from which the re-exported functions came from,
#'  by specifying them in the \code{dependencies} argument.
#' @param deps an optional character vector,
#' giving the names of the dependencies of the
#' main package to be imported also under the alias. \cr
#' Only dependencies that appear in \link{pkg_get_deps_minimal} are allowed. \cr
#' Defaults to \code{NULL}, which means no dependencies are imported under the alias. \cr
#' Core R (i.e. "base", "stats", etc.) is not allowed.
#' @param lib.loc a character vector describing the location of R library trees to search through.
#' @param env see \link{import_env}. \cr \cr
#'
#' 
#'
#'
#' @returns
#' A locked environment object, similar to the output of \link[base]{loadNamespace},
#' with the name as specified in the \code{alias} argument,
#' will be created. \cr
#' This object, of class \link{tinyimport_alias},
#' will contain the exported functions from the specified package(s). \cr
#' The alias object will be placed in the specified environment. \cr
#' For its usage, see \link{tinyimport_alias}. \cr
#' \cr
#' Note the following:
#' 
#' - No more than 5 packages
#'  (ignoring re-exports)
#'  are allowed to be imported under a single alias.
#'  - Packages are imported in the following order: \cr
#'  First the dependencies  in the order they are specified in `deps`,
#'  and then the main package, and then its re-exports (if `re_exports = TRUE`). \cr
#'  Thus main package will always overwrite the dependencies in case of conflicting names. \cr
#' 
#'
#'
#' @section Why Aliasing A Package with its Dependencies is Useful:
#' To use an R-package with its dependencies,
#' whilst avoiding the disadvantages of attaching a package (see \link{tinycodet_import}),
#' one would traditionally use the \link[base]{::} operator like so: \cr
#'
#' ```{r eval = FALSE}
#' main_package::some_function1()
#' dependency1::some_function2()
#' ```
#'
#' This becomes cumbersome as more packages are needed and/or
#' as the package name(s) become longer. \cr
#' The \code{import_as()} function avoids this issue
#' by allowing multiple \bold{related} packages to be imported under a single alias,
#' allowing one to code like this:
#'
#' ```{r eval = FALSE}
#' import_as(.alias ~ main_package, deps = "dependency1")
#' .alias$some_function1()
#' .alias$some_function2()
#' ```
#'
#' Thus importing a package, or multiple directly related packages, under a single alias,
#' which \code{import_as()} provides, avoids the above issues.
#' Importing a package under an alias is referred to as "aliasing" a package. \cr \cr
#'
#'
#' @seealso \link{tinycodet_import}
#'
#'
#' @example inst/examples/import.R
#'
#'
#'

#' @rdname import_as
#' @export
import_as <- function(
    main, re_exports = TRUE,
    deps = NULL,
    lib.loc = .libPaths(), env = NULL
) {
  
  .check_pkgenv(parent.frame(), sys.call())
  env <- .internal_importenv(env, parent.frame(), sys.call())
  
  
  # process `main`:
  if(.internal_is_string(main)) {
    main <- stats::as.formula(main)
  }
  if(!.internal_is_formula(main) || length(main) != 3L) {
    stop("`main` must be a 2-sided formula")
  }
  if(length(all.vars(main)) != 2L) {
    stop("improper formula given for `main`")
  }
  alias <- as.character(main[[2L]])
  main_package <- as.character(main[[3L]])
  if(length(alias) != 1L || length(main_package) != 1L) {
    stop("improper formula given for `main`")
  }
  
  # process library:
  lib.loc <- .import_lib.loc(lib.loc, sys.call())
  
  
  # Check alias:
  check_proper_alias <- c(
    make.names(alias) == alias,
    length(alias) == 1,
    isTRUE(nchar(alias) > 0),
    isFALSE(alias %in% c("T", "F")),
    !startsWith(alias, "._"),
    !startsWith(alias, "_.")
  )
  if(!isTRUE(all(check_proper_alias))){
    stop("Syntactically invalid name for alias")
  }
  

  # perform checks:
  .import_as_checks(alias, main_package, re_exports, deps, lib.loc, sys.call())
  
  
  # list packages:
  pkgs <- c(deps, main_package)
  
  
  # import packages:
  export_names_all <- character()
  export_names_allconflicts <- character()
  conflicts_df <- data.frame(
    package = character(length(pkgs)),
    winning_conflicts = character(length(pkgs))
  )
 
  namespaces <- list()

  
  for (i in seq_along(pkgs)) {
    
    namespace_current <- .internal_prep_Namespace(pkgs[i], lib.loc, abortcall = sys.call())
    conflicts_df$package[i] <- pkgs[i]
    
    
    if(pkgs[i] == main_package && isTRUE(re_exports)) {
      reexports <- .internal_get_reexports_ns(main_package, lib.loc, abortcall = sys.call())
      namespace_current <- utils::modifyList(
        namespace_current,
        reexports
      )
      
      conflicts_df$package[i] <- paste0(pkgs[i], " + re-exports")
      
    }
    
    export_names_current <- names(namespace_current)
    
    export_names_intersection <- intersect(export_names_current, export_names_all)
    
    if(length(export_names_intersection) > 0) {
      conflicts_df$winning_conflicts[i] <- paste0(export_names_intersection, collapse = ", ")
    }
    
    export_names_allconflicts <- c(export_names_allconflicts, export_names_intersection)
    export_names_all <- c(export_names_all, export_names_current)
    namespaces <- utils::modifyList(namespaces, namespace_current)
  }
  
  
  # make attributes:
  ordered_object_names <- names(namespaces)
  out <- as.environment(namespaces)
  class(out) <- c("tinyimport_alias", "environment")
  if(isTRUE(re_exports)){
    re_exports.pkgs <- lapply(reexports, \(x)attr(x, "package")) |>
      unlist() |> unname() |> unique()
  }
  if(isFALSE(re_exports)) {
    re_exports.pkgs <- NA
  }
  pkgs <- list(packages_order = pkgs,
               main_package = main_package,
               re_exports.pkgs = re_exports.pkgs)
  out$.__attributes__. <- list(
    pkgs = pkgs,
    conflicts = .format_conflicts_df(conflicts_df),
    ordered_object_names = ordered_object_names,
    tinyimport = "tinyimport"
  )
  
  # lock environment (JUST LIKE LOADNAMESPACE)
  lockEnvironment(out, bindings = TRUE)
  assign(alias, out, envir = env)
  
  .check_import_post(c(main_package, deps), lib.loc, sys.call())
  
  
  message("Import & method registration complete")
  return(invisible(NULL))
}

#' @keywords internal
#' @noRd
.format_conflicts_df <- function(conflicts_df) {
  if(nrow(conflicts_df)<=2) {
    return(conflicts_df)
  }
  if(nrow(conflicts_df)>2) {
    n <- nrow(conflicts_df)-1
    ind <- 2:n
    for(i in ind) {
      current_overwrites <- stringi::stri_split(
        conflicts_df$winning_conflicts[i], fixed = ", ", simplify=TRUE
      ) |> as.vector()
      next_overwrites <- stringi::stri_split(
        conflicts_df$winning_conflicts[i:nrow(conflicts_df)], fixed = ", ", simplify = TRUE
      ) |> as.vector()
      if(isTRUE(any(current_overwrites %in% next_overwrites))) {
        conflicts_df$winning_conflicts[i] <- paste0(
          setdiff(current_overwrites, next_overwrites), collapse = ", "
        )
      }
    }
    return(conflicts_df)
  }
}



#' @keywords internal
#' @noRd
.import_as_checks <- function(alias, main_package, re_exports, deps, lib.loc, abortcall) {
  
  # check main_package:
  if(length(main_package) != 1 || !is.character(main_package)){
    stop(simpleError("main package must be a single string", call = abortcall))
  }
  .check_forbidden_pkgs(
    pkgs = main_package, lib.loc = lib.loc, abortcall = sys.call()
  )
  .check_pkgs(
    pkgs = main_package, lib.loc = lib.loc, abortcall = sys.call()
  )
  
  
  # check re-exports:
  if(!isTRUE(re_exports) && !isFALSE(re_exports)) {
    stop(simpleError("`re_exports` must be `TRUE` or `FALSE`", call = abortcall))
  }
  
  # check total number of packages:
  
  
  # Check deps:
  if(!is.null(deps)) {
    if(!is.character(deps) || length(deps) == 0) { 
      stop(simpleError("`deps` must be a character vector", call = abortcall))
    }
    if(main_package %in% deps) {
      stop(simpleError("`deps` cannot include main package", call = abortcall))
    }
    if((length(deps) + 1L) > 5L) {
      stop(simpleError(
        "no more than 5 packages allowed to be imported under a single alias",
        call = abortcall
      ))
    }
    
    .check_forbidden_pkgs(deps, lib.loc = lib.loc, abortcall = sys.call())
    
    if(!isNamespaceLoaded(main_package)) {
      min_deps <- pkg_get_deps_minimal(main_package, lib.loc)
    }
    else {
      min_deps <- pkg_get_deps_minimal(main_package, NULL)
    }
    .check_pkgs(deps, lib.loc, "minimal dependencies", min_deps, abortcall)
    
  }

}
