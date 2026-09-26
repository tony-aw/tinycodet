#' Add, Remove, or Access Search Path Environments
#'
#' @description
#' Functions for safely attaching, removing, or accessing environments from \link[base]{search} path. \cr
#' \cr
#' `searchenv_add()` adds a new, empty environment to the \link[base]{search} path. \cr
#' `searchenv_rm()` removes a user-defined environment from the \link[base]{search} path. \cr
#' `searchenv_get()` returns a user-defined environment from the \link[base]{search} path. \cr \cr
#' 
#' @param name a single string giving the name for the environment in the \link[base]{search} path.
#' @param pos a single positive integer giving the position for the environment in the \link[base]{search} path.
#' 
#' 
#' @details
#' These functions were designed with safety in mind. \cr
#' They do not allow adding, removing, or accessing search environments like the following: 
#' 
#' `r .txt_searchenv_forbidden()` \cr \cr
#'  
#' Attempting to add a new search path environment whose name already exists gives an error. \cr
#' Attempting to remove or access a search path environmeent whose name does not exists gives an error. \cr
#' \cr
#' 
#' @example inst/examples/searchenv.R
#' 
#' @name searchenv
NULL


#' @rdname searchenv
#' @export
searchenv_add <- function(name, pos = 2L) {
  
  if(missing(name)) {
    stop("`name` must be specified when adding new search path environment")
  }
  .check_pkgenv(parent.frame(), sys.call())
  .check_searchenv(name, pos, TRUE, sys.call())
  
  env <- new.env(parent = emptyenv())
  make_attach <- attach
  make_attach(env, pos = pos, name = name)
  return(invisible(NULL))
  
}

#' @rdname searchenv
#' @export
searchenv_rm <- function(name, pos) {
  
  .check_pkgenv(parent.frame(), sys.call())
  
  if(anyDuplicated(search())) {
    if(missing(name) || missing(pos)) {
      stop("duplicate search path names found; `pos` and `name` must both be specified")
    }
  }
  
  if(missing(pos)) {
    pos <- which(search() == name)
  }
  
  .check_searchenv(name, pos, FALSE, sys.call())
  if(search()[pos] != name) {
    stop("search environment at position `pos` does not have name `name`")
  }
  
  make_detach <- detach
  make_detach(name, pos, character.only = TRUE)
  return(invisible(NULL))
  
}

#' @rdname searchenv
#' @export
searchenv_get <- function(name, pos) {
  
  .check_pkgenv(parent.frame(), sys.call())
  
  if(anyDuplicated(search())) {
    if(missing(name) || missing(pos)) {
      stop("duplicate search path names found; `pos` and `name` must both be specified")
    }
  }
  
  if(!missing(name) && !missing(pos)) {
    .check_searchenv(name, pos, FALSE, sys.call())
  }
  else if(missing(name)) {
    .check_searchenv(NULL, pos, FALSE, sys.call())
    name <- search()[pos]
  }
  else if(missing(pos)) {
    .check_searchenv(name, NULL, FALSE, sys.call())
    pos <- which(search() == name)
  }
  
  if(search()[pos] != name) {
    stop("search environment at position `pos` does not have name `name`")
  }
  .check_searchenv(name, pos, FALSE, sys.call())
  
  
  
  return(as.environment(name))
}


#' 
#' 
#' @seealso \link{tinycodet_import}

