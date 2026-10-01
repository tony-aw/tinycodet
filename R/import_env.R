#' Environment Specification in the import_ Functions
#'
#' @description
#' The `env` argument in the `import_` functions specify
#' where the functions, exported objects, or alias object will be placed. \cr
#' The following can be specified for `env`: 
#' 
#'  - `NULL`: If `env = NULL`,
#'  the objects will be placed in the caller environment.
#'  - an environment.
#'  - a string, giving the name of the search path,
#'  as given by \link[base]{search},
#'  to place the objects in. \cr
#'  If multiple search paths have the specified name, an error is returned.
#'  - a number larger than 1 and smaller than `length(search())`,
#'  giving the position of the search path to place the objects in. \cr \cr
#'  
#' If `env` is a string or number, and thus points to a place in the search path,
#' the search path environment is not allowed to be any of the following:
#' 
#' `r .txt_searchenv_forbidden()` \cr \cr
#'  
#' Attempting to use such a path results in an error. \cr
#' The user can use the \link[=searchenv_add]{searchenv_} functions,
#' provided by 'tinycodet',
#' to safely add or remove custom search paths. \cr
#' \cr
#' The default value for `env` is `NULL`. \cr \cr
#' 
#' 
#' @seealso \link{tinycodet_import}
#' 
#' 
#' @example inst/examples/searchenv.R
#' 
#' 
#' @name import_env
NULL
#' 
#' 
#'
#' 

