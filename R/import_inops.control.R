#' import_inops.control
#'
#' @description
#' Additional arguments to control exposing infix operators in the \link{import_inops} function.
#'
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
#' indicating if it is allowed to overwrite existing infix operators.
#' \itemize{
#'  \item If \code{TRUE} (default), a warning is given when operators existing in the current environment
#'  are being overwritten,
#'   but the function continuous nonetheless.
#'  \item If \code{FALSE}, an error is produced
#'  when the to be exposed operators already exist in the current environment,
#'  and the function is halted.
#' }
#' @param inherits logical. \cr
#' When exposing infix operators,
#' \link{import_inops}
#' checks if infix operators with the same names are already present in the current environment. \cr
#' If `inherits = FALSE`,
#' only the current environment is checked for existing operators. \cr
#' If `inherits = TRUE`, 
#' enclosed environments,
#' most notably package namespaces,
#' are also checked for existing operators. \cr
#' Defaults to \code{FALSE}. \cr
#' See also \link[base]{exists}. \cr
#'
#'
#' @details
#' You cannot specify both the \code{exclude} and \code{include.only} arguments.
#' Only one or the other, or neither. \cr
#' \cr
#'
#'
#' @returns
#' This function is used internally in the \link{import_inops} function. \cr \cr
#'
#' @seealso [import_inops()], [tinycodet_import()]
#'
#'
#' @examples
#'
#' # additional arguments (only used when exposing, not unexposing):
#' import_as(~ stri., "stringi")
#' import_inops(expose = stri., include.only = "%s==%")
#' import_inops(unexpose = stri.)
#' import_inops(expose = "stringi", exclude = "%s==%")
#' import_inops(unexpose = "stringi")
#' import_inops(expose = stri., overwrite = FALSE)
#' import_inops(unexpose = stri.)
#' import_inops(expose = "stringi", overwrite = FALSE)
#' import_inops(unexpose = "stringi")
#'
#'

#' @rdname import_inops.control
#' @export
import_inops.control <- function(
    exclude = NULL, include.only = NULL, overwrite = TRUE, inherits = FALSE
) {
  # check exclude and include.only:
  if(!is.null(exclude) && !is.null(include.only)){
    stop(
      "canntot specify both `exclude` and `include.only`; specify only one or none"
    )
  }
  if(length(exclude) > 0) {
    if(!is.character(exclude) || any(!nzchar(exclude)) || anyDuplicated(exclude)) {
      stop("`exclude` must be a character vector of unique function names")
    }
  }
  if(length(include.only) > 0) {
    if(!is.character(include.only) || any(!nzchar(include.only)) || anyDuplicated(include.only)) {
      stop("`include.only` must be a character vector of unique function names")
    }
  }

  if(!is.null(exclude)) {
    exclude_ops <- exclude[exclude %s{}% "%|:="]
    if(length(exclude_ops) == 0L) {
      stop("`exclude` must be names of infix operators")
    }
  }
  if(!is.null(include.only)) {
    include.only_ops <- include.only[include.only %s{}% "%|:="]
    if(length(include.only_ops) == 0L) {
      stop("`include.only` must be names of infix operators")
    }
  }

  # check overwrite:
  if(!isTRUE(overwrite) && !isFALSE(overwrite)) {
    stop(
      "`overwrite` must be either `TRUE` or `FALSE`"
    )
  }

  # check inherits:
  if(!isTRUE(inherits) && !isFALSE(inherits)) {
    stop(
      "`inherits` must be either `TRUE` or `FALSE`"
    )
  }

  # return arguments:
  out <- list(
    exclude = exclude,
    include.only = include.only,
    overwrite = overwrite,
    inherits = inherits
  )
  return(out)

}

#' @keywords internal
#' @noRd
.import_exclude_include <- function(operators, exclude, include.only, abortcall) {
  if(!is.null(exclude)){
    wrong_excludes <- exclude[!exclude %in% operators]
    if(length(wrong_excludes) > 0) {
      warn.txt <- paste0(
        "The following exclusions are not present, and are ignored: \n",
        paste0(wrong_excludes, collapse = ", "), "\n"
      )
      warning(simpleWarning(warn.txt, call = abortcall))
    }
    operators <- setdiff(operators, exclude)
  }
  if(!is.null(include.only)){
    wrong_includes <- include.only[!include.only %in% operators]
    if(length(wrong_includes) > 0) {
      warn.txt <- paste0(
        "The following inclusions are not present, and are ignored: \n",
        paste0(wrong_includes, collapse = ", "), "\n"
      )
      warning(simpleWarning(warn.txt, call = abortcall))
    }
    operators <- intersect(operators, include.only)
  }
  return(operators)
}
