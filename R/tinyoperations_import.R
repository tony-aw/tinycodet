#' The tinyoperations import system
#'
#'@description
#' The \code{tinyoperations} R package introduces a new package import system.
#' This system attempts to combine the benefits of aliasing a package,
#' with the benefits of attaching a package. \cr
#' \cr
#' The main part of the import system is implemented in the following 4 functions:
#' \itemize{
#'  \item \link{import_as}:
#'  Allow a main package + its foreign exports + its dependencies + its enhances + its extensions
#'  to be loaded under a single alias.
#'  \item \link{import_inops}:
#'  Expose infix operators from one or more packages to the current environment.
#'  \item \link{import_data}:
#'  Directly return a data set from a package, to allow straight-forward assignment.
#'  \item \link{source_selection}:
#'  Source a script, but only place the specified objects in the current environment.
#' }
#'
#' There are also some additional helper functions for the package import system,
#' see \link[=pkg_get_deps]{pkgs}, and \link{report_inops}. \cr
#' \cr
#' Please refer to the Read-Me file on the GitHub main page of this page for more information. \cr
#' See: \url{https://github.com/tony-aw/tinyoperations}. \cr
#'
#'
#' @seealso \link{tinyoperations_help}


#' @rdname tinyoperations_import
#' @export
tinyoperations_import <- function() {
  utils::`?`(tinyoperations_import)
}
