#' Simple Project isolation by forcing library Paths
#'
#' @description
#'
#' The \code{force_libPaths()} function allows the user to force R to specific libraries.
#' This may in some occasions be needed as base R's \code{.libPaths()} function
#' only allows adding new library paths, not overwrite existing site or system libraries
#' The library paths are of course re-set again every time R restarts.
#'
#' @param lib_vec a character vector giving the new library path(s). \cr
#' Just like in \code{.libPaths()}, the order matters: \cr
#' R will first look for packages in the first path in \code{.libPaths()}, \cr
#' and if it cannot find the package(s),
#' it will look for the packages in the second path in \code{.libPaths()},
#' etc.
#'
#'
#'
#' @returns
#' \code{force_libPaths()}: adjusts the R library paths as defined in \code{.libPaths()} directly.
#'
#' @references McBain (2019, June 20). Before I Sleep: Hacking R's library paths. Retrieved from https://milesmcbain.com/posts/hacking-r-library-paths/
#'
#' @examples
#'
#' \dontrun{
#' force_libPaths("/mylibrary")
#' }
#'
#'
#'

#' @rdname force_libPaths
#' @export
force_libPaths <- function(lib_vec) {
  lib_vec <- normalizePath(lib_vec, mustWork = TRUE)
  shim_fun <- .libPaths
  shim_env <- new.env(parent = environment(shim_fun))
  shim_env$.Library <- character()
  shim_env$.Library.site <- character()
  environment(shim_fun) <- shim_env
  shim_fun(lib_vec)
}
