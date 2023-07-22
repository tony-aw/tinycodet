#' Explanation of tinyoperations' Import Lock
#'
#'@description
#' The \link{import_as} function returns a locked environment,
#' and the \link{import_inops} function returns one or more locked infix operators. \cr
#' These returned objects are locked to prevent the user from (accidentally) messing
#' with the functions, or their attributes. \cr
#' The \code{import_inops()} and \code{import_as()} functions will
#' delete locked objects created by \code{import_inops()} or \code{import_as()} when it needs to overwrite them. Any user-defined objects are not touched in this process.
#' The attributes of the objects, as well as class, namespace names, and other properties,
#' are used to determine if any object is created
#' by \link{import_as} / \link{import_inops} or not. This should be quite safe.
#'
#' @seealso [tinyoperations_import()]


#' @rdname import_lock
#' @export
import_lock <- function() {
  utils::`?`(import_lock)
}
