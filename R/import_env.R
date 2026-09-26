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
#'  - number larger than 1 and smaller than `length(search())`,
#'  giving the position of the search path to place the objects in. \cr
#'  
#' If `env` is a string or number, and thus points to a place in the search path,
#' the search path evironment must not be a package path
#' (their names start with `"package:"`)
#' or a tools path
#' (their names start with `"tools:`). \cr
#' Attempting to use such a path results in an error. \cr
#' The user can use the `search_` functions,
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

