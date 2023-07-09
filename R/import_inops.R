#' Expose infix operators to the current environment
#'
#' @description
#' The \code{import_inops()} function
#' exposes the infix operators of the specified packages to the current environment
#' (like the global environment, or the environment within a function). \cr
#' To ensure the user can still verify which operator function came from which package,
#' a "package" attribute is added to each exposed operator. \cr
#' Naturally, the namespace attribute of each of the operators remains intact. \cr
#' If you wish to globally attach infix operators,
#' instead of just placing them in the current environment,
#' see \link{pkg_lsf}. \cr
#'
#'
#' @param pkgs a single string, or character vector, with the package name(s). \cr
#' NOTES: \cr
#'  1) The order of the character vector matters!
#' If multiple packages share infix operators with the same name,
#' the conflicting operators of the package named last
#' will overwrite those of the earlier named packages. \cr
#'  2) The \code{import_inops} function performs a basic check
#' that the packages are mostly (reverse) dependencies of each other.
#' If not, it will give an error.
#' @param exclude a character vector,
#' giving the infix operators NOT to expose to the current environment. \cr
#' This can be handy to prevent overwriting any (user defined)
#' infix operators already present in the current environment. \cr
#' \cr
#' NOTE: You cannot specify both the \code{exclude} and \code{include.only} arguments.
#' Only one or the other, or neither.
#' @param include.only a character vector,
#' giving the infix operators to expose to the current environment,
#' and the rest of the operators will not be exposed. \cr
#' This can be handy to prevent overwriting any (user defined)
#' infix operators already present in the current environment. \cr
#' \cr
#' NOTE: You cannot specify both the \code{exclude} and \code{include.only} arguments.
#' Only one or the other, or neither.
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
#'
#'
#' @details
#' The \code{import_inops()} function does not support overloading base/core R operators,
#' so don't try. \cr
#' \cr
#' For a more detailed description of the import system introduced by the
#' \code{tinyoperations} R package,
#' please refer to the Read Me file on the GitHub main page: \cr
#' \url{https://github.com/tony-aw/tinyoperations}
#'
#'
#' @returns
#' For \code{import_inops()}: \cr
#' The infix operators from the specified packages will be placed
#' in the current environment
#' (like the Global environment, or the environment within a function). \cr
#' \cr
#'
#' @seealso \link{tinyoperations_import}, \link[=source_inops]{source_module}, \link[=pkg_get_deps]{pkgs}
#'
#'
#' @examples
#' \dontrun{
#' import_inops("data.table")
#' }
#'
#'

#' @rdname import_inops
#' @export
import_inops <- function(
    pkgs, lib.loc=.libPaths(), exclude, include.only, overwrite=TRUE, inherits=FALSE
) {

  # check library:
  if(length(lib.loc)<1) {
    stop("At least one library path must be given")
  }

  # check packages:
  .internal_check_pkgs(pkgs=pkgs, lib.loc=lib.loc, pkgs_txt = "packages", abortcall=sys.call())
  if(length(pkgs)>1) {
    check_deps_OK <- .internal_check_deps_overlap_any(
      pkgs, lib.loc=lib.loc, deps_type=c("Depends", "Imports", "LinkingTo")
    )
    if(!check_deps_OK) {
      error.txt <- paste0(
        "Multiple packages specified, but the packages have no dependency overlap at all.",
        "\n",
        "Function halted."
      )
      stop(error.txt)
    }
  }

  # check exclude and include.only:
  if(!missing(exclude) & !missing(include.only)){
    stop("canntot specify both `exclude` and `include.only`; specify only one or none.")
  }

  # check overwrite:
  check_overwrite <- c(
    is.logical(overwrite),
    length(overwrite)==1,
    all(overwrite) %in% c(TRUE, FALSE)
  )
  if(!all(check_overwrite)) {
    stop("`overwrite` must be either `TRUE` or `FALSE`")
  }

  # check inherits:
  check_inherits <- c(
    is.logical(inherits),
    length(inherits)==1,
    all(inherits) %in% c(TRUE, FALSE)
  )
  if(!all(check_inherits)) {
    stop("`inherits` must be either `TRUE` or `FALSE`")
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
      export_names_allconflicts <- c(export_names_intersection, export_names_allconflicts)
      export_names_all <- c(export_names_current, export_names_all)
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
      operators, overwrite, inherits, envir=parent.frame(1), abortcall=sys.call()
    )
  }

  if(isTRUE(length(operators)>0)) {

    message(
      "Placing infix operators in current environment..."
    )
    for(op in operators){
      assign(op, namespaces[[op]], envir = parent.frame(n = 1))
    }
    message("Done")

  }
}

