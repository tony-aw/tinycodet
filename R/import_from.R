#' Expose Exported Objects From Package Namespace in an Environment
#'
#' @description
#' `import_from()`
#' exposes exported objects from a package to the specified environment. \cr \cr
#'
#' @param package a single string, giving the package name.
#' @param ls a character vector giving the names of exported objects to expose. \cr
#' To expose all objects of a certain type,
#' like all (infix/replacement) operators or all non-functions,
#' the \link{import_ls} function can be used to get those object names.
#' @param re_exports `TRUE` or `FALSE`, indicating if re-exports of `package` should be included. \cr
#' Default is `TRUE`, as that is analogous to the behaviour of base R's \link[base]{::} operator.
#' @param lock `TRUE` or `FALSE`, indicating if the exported objects should be locked
#' (see \link[base]{lockBinding}).
#' @param prefix either `NULL` or a single string, giving the prefix to add to the functions, to avoid possible conflicts. \cr
#' If `NULL` no prefix is added; this is the default. \cr
#' Otherwise, a prefix is added to the exported object names. \cr
#' Note that \bold{no} prefix is added to infix operators, primitive functions, and non-functions (like constants). \cr
#' The prefix cannot end with a single dot, as that might interfere with S3 methods.
#' @param lib.loc a character vector describing the location of R library trees to search through.
#' @param env see \link{import_env}. \cr \cr
#'
#'
#'
#'
#' @returns
#' The objects specified in the given package will be placed & locked
#' in the specified environment. \cr
#' \cr
#'
#' @seealso \link{tinycodet_import}, [import_ls()]
#'
#'
#' @examples
#'
#' import_from("stringi", import_ls("stringi", "infix"))
#' import_from("stringi", import_ls("stringi", "rp"))
#'
#'
#'

#' @rdname import_from
#' @export
import_from <- function(
    package, ls, re_exports = TRUE, lock = FALSE, prefix = NULL, lib.loc = .libPaths(), env = NULL
) {

  # CHECKS:
  .check_pkgenv(parent.frame(), sys.call())
  env <- .internal_importenv(env, parent.frame(), sys.call())
  .import_from_prechecks(package, ls, re_exports, lock, sys.call())
  lib.loc <- .import_lib.loc(lib.loc, sys.call())
  .check_forbidden_pkgs(
    pkgs = package, lib.loc = lib.loc, pkgs_txt = "packages", abortcall = sys.call()
  )
  .check_pkgs(
    pkgs = package, lib.loc = lib.loc, pkgs_txt = "packages", abortcall = sys.call()
  )
  
  
  # FUNCTION:
  
  # get entire namespace:
  ns <- .internal_prep_Namespace(package, lib.loc, abortcall = sys.call())
  if(re_exports) {
    re <- .internal_get_reexports_ns(package, lib.loc, sys.call())
    ns <- utils::modifyList(ns, re)
  }
  
  # subset namespace:
  .check_exists(
    ls, names(ns), "package", sys.call()
  )
  ns <- ns[ls]
  
  # prefix names:
  if(!is.null(prefix) && !.internal_is_missingstring(prefix)) {
    names(ns) <- .import_prefix_names(ns, prefix, sys.call())
  }
  
  # check for conflicts:
  .check_conflicts(
    names(ns), envir = env, abortcall = sys.call()
  )
  
  # assign objects to environment:
  .use_from(ns, env, lock)
  
  .check_import_post(package, lib.loc, sys.call())
  
  message("Import & method registration complete")
  
  return(invisible(NULL))

}


#' @keywords internal
#' @noRd
.import_from_prechecks <- function(
    package, ls, re_exports, lock, abortcall
) {
  
  if(!.internal_is_string(package)) {
    stop(simpleError("`package` must be a single string", call = abortcall))
  }
  if(length(ls) == 0L) {
    message(simpleMessage("nothing to import", call = abortcall))
    return(invisible(NULL))
  }
  if(!is.character(ls)) {
    stop(simpleError(
      "`ls` must be a character vector of object names",
      call = abortcall
    ))
  }
  if(anyNA(ls) || any(!nzchar(ls)) || anyDuplicated(ls)) {
    stop(simpleError(
      "`ls` cannot have missing values, empty strings, or duplicate values",
      call = abortcall
    ))
  }
  
  if(!isTRUE(re_exports) && !isFALSE(re_exports)) {
    stop(simpleError("`re_exports` must be `TRUE` or `FALSE`", call = abortcall))
  }
  if(!isTRUE(lock) && !isFALSE(lock)) {
    stop(simpleError("`lock` must be `TRUE` or `FALSE`", call = abortcall))
  }
  
  
  
}


#' @keywords internal
#' @noRd
.use_from <- function(input.env, output.env, lock) {
  nms <- names(input.env)
  for(i in nms){
    assign(i, input.env[[i]], envir = output.env)
  }
  
  if(lock) {
    for(i in nms){
      lockBinding(i, env = output.env)
    }
  }
}