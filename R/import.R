#' Additional package import management
#'
#' @description
#' These functions and operator are focused on making it easier to
#' use packages without having to explicitly attaching them to your namespace. \cr
#' \cr
#' \code{import_as}: \cr
#' The \code{import_as()} function
#' imports the namespaces of an R package
#' (or a small set of R packages that "belong" to each other)
#' under the same alias. \cr
#' \cr
#' \code{import_inops}: \cr
#' The \code{import_inops()} function
#' exposes the infix operators of the specified packages to the current environment
#' (like the global environment, or the environment within a function). \cr
#' To ensure the user can still verify which operator function came from which package,
#' a "package" attribute is added to each exposed operator. \cr
#' Naturally, the namespaces of the operators remain intact. \cr
#' \cr
#' \code{import_data}: \cr
#' The \code{import_data()} function gets a specified data set from a package. \cr
#' Unlike \code{utils::data()}, the \code{import_data()} function returns the data set directly,
#' and allows assigning the data set like so: \cr
#' \code{mydata <- import_data(...)}. \cr
#' \cr
#' \code{import_lsf}: \cr
#' The \code{import_lsf(package, ...)} function gets a list of exported functions/operators from a package. \cr
#' \cr
#'
#' @param alias a variable name (unquoted),
#' giving the (not yet existing) object
#' where the package(s) are to be assigned to. \cr
#' Syntactically invalid names are not allowed for the alias name.
#' @param package a single string, giving the name of the package.
#' @param deps either logical, or a character vector. \cr
#' If \code{FALSE} (default), no dependencies are loaded under the alias. \cr
#' If \code{TRUE}, ALL dependencies are loaded under the alias. \cr
#' If a character vector, then it is taken as the dependencies of the
#' package to be loaded also under the alias. \cr
#' NOTE (1): "Dependencies" here are defined as any package appearing in the
#' "Depends", "Imports", or "LinkingTo" sections of the Description file of the
#' package. \cr
#' NOTE (2): If \code{deps} is a character vector:
#' The order of the character vector matters!
#' If multiple packages share objects with the same name,
#' the package named last will overwrite the earlier named package. \cr
#' @param extensions a character vector,
#' giving the names of the reverse-dependencies of the
#' package to be loaded also under the alias.
#' Defaults to \code{NULL}, which means no extensions are loaded. \cr
#' NOTE: The order of the character vector matters!
#' If multiple packages share objects with the same name,
#' the package named last will overwrite the earlier named package. \cr
#' @param pkgs a single string, or character vector, with the package name(s). \cr
#' NOTE: The order of the character vector matters!
#' If multiple packages share objects with the same name,
#' the package named last will overwrite the earlier named package. \cr
#' @param exclude a character vector,
#' giving the infix operators NOT to expose to the current environment. \cr
#' This can be handy to prevent overwriting any (user defined)
#' infix operators already present in the current environment.
#' @param include.only a character vector,
#' giving the infix operators to expose to the current environment,
#' and the rest of the operators will not be exposed. \cr
#' This can be handy to prevent overwriting any (user defined)
#' infix operators already present in the current environment.
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through).
#' This is usually \code{.libPaths()}.
#' See also \link[base]{loadNamespace}.
#' @param dataname a single string, giving the name of the data set.
#' 
#' @param type The type of functions to list. Possibilities: \cr
#' \code{"inops"} or \code{"operators"}: Only infix operators. \cr
#' \code{"regfuns"}: Only regular functions (thus excluding infix operators). \cr
#' \code{"all"}: All functions, both regular functions and infix operators. \cr
#'
#'
#' @details
#' In general: \cr
#' The \code{import_as} and \code{import_inops} functions will inform the user
#' about conflicting objects. \cr
#' \cr
#' For \code{import_as()}: \cr
#' The \code{import_as()} function will first load the dependencies
#' in the order specified in argument \code{deps}, if any,
#' then it loads the package named in argument \code{package},
#' then it loads the extensions in the order specified in argument \code{extensions},
#' if any. \cr
#' The \code{import_as()} function does not import internal functions
#' (i.e. internal functions are kept internal, as they should). \cr
#' \cr
#' For \code{import_inops()}: \cr
#' The \code{import_inops()} function is less strict than \code{import_as()}
#' in terms of which R packages can be called together.
#' But still the packages specified in argument \code{pkgs} need to have SOME
#' overlap in their dependencies.
#' \cr
#'
#' @returns
#' For \code{import_as}: \cr
#' The variable named in the \code{alias} argument will be created
#' (if it did not already exist),
#' and it will contain the (merged) package environment. \cr
#' \cr
#' For \code{import_inops()}: \cr
#' The infix operators from the specified packages will be placed
#' in the current environment
#' (like the Global environment, or the environment within a function).\cr
#' \cr
#' For \code{import_data()}: \cr
#' Returns the data directly.
#' Thus, one can assign the data like so: \code{mydata <- import_data(...)}. \cr
#' \cr
#' For \code{import_lsf()}: \cr
#' Returns a character vector of function and/or operator names. \cr
#' \cr
#'
#'
#' @examples
#'
#' \dontrun{
#' deps <- unlist(tools::package_dependencies("devtools"))
#' pkgs <- c(deps, "devtools")
#' import_as(devt, "devtools", deps = TRUE) # this creates the devt object
#' import_inops(pkgs)
#' d <- import_data("chicago", "gamair")
#' head(d)
#' }
#'
#'
#'

#' @name import
NULL

#' @rdname import
#' @export
import_as <- function(
    alias, package, deps=FALSE, extensions=NULL, lib.loc=.libPaths()
) {
  
  # Check alias:
  check_proper_alias <- c(
    make.names(substitute(alias))==substitute(alias),
    isTRUE(nchar(substitute(alias))>0),
    length(substitute(alias))==1
  )
  if(isFALSE(all(check_proper_alias))){
    stop("Syntactically invalid name for object `alias`")
  }
  
  # check package:
  if(length(package)>1){
    stop("Only a single package can be given in the `package` argument")
  }
  check_install <- package %installed in% lib.loc
  if(isFALSE(check_install)) {
    stop("Given package not installed!")
  }
  
  # Check dependencies:
  actual_deps <- .internal_get_deps(
    package, lib.loc=.libPaths(), deps_type=c("Depends", "Imports", "LinkingTo")
  )
  if(length(actual_deps)>10){
    message("Note: this package has a lot of dependencies")
  }
  if(isFALSE(deps)) {
    deps <- NULL
  }
  if(isTRUE(deps)){
    deps <- actual_deps
  }
  if(is.character(deps) & length(deps)>0) {
    if(length(deps)!=length(unique(deps))) {
      stop("one or more duplicate dependent packages given")
    }
    
    wrong_deps <- deps[!deps %installed in% lib.loc]
    if(length(wrong_deps)>0) {
      error.txt <- paste0(
        "The following dependent packages are not installed:",
        "\n",
        paste0(wrong_deps, collapse = ", ")
      )
      stop(error.txt)
    }
    
    if(is.character(deps)) {
      wrong_deps <- deps[!deps %in% actual_deps]
      if(length(wrong_deps)>0) {
        error.txt <- paste0(
          "The following dependent packages are not in Depends or Imports:",
          "\n",
          paste0(wrong_deps, collapse = ", ")
        )
        stop(error.txt)
      }
    }
  }
  
  
  # Check extensions:
  if(!is.null(extensions) & is.character(extensions) & length(extensions)>0) {
    if(length(extensions)!=length(unique(extensions))) {
      stop("one or more duplicate dependent packages given")
    }
    
    wrong_extensions <- extensions[!extensions %installed in% lib.loc]
    if(length(wrong_extensions)>0) {
      error.txt <- paste0(
        "The following extenions are not installed:",
        "\n",
        paste0(wrong_extensions, collapse = ", ")
      )
      stop(error.txt)
    }
    tempfun <- function(x){
      deps <- .internal_get_deps(x, lib.loc=lib.loc, deps_type=c("Depends", "Imports", "LinkingTo"))
      return(package %in% deps)
    }
    check_extensions <- sapply(
      extensions, tempfun
    )
    wrong_extensions <- extensions[!check_extensions]
    if(length(wrong_extensions)>0) {
      error.txt <- paste0(
        "The following extensions were not found to be actual reverse dependencies:",
        "\n",
        paste0(wrong_extensions, collapse = ", ")
      )
      stop(error.txt)
    }
  }
  
  
  pkgs <- c(deps, package, extensions)
  
  namespaces <- .internal_import_namespaces(pkgs, lib.loc = lib.loc)
  
  message(paste0(
    "Done", "\n",
    "You can now access the functions using ", substitute(alias), "$...", "\n",
    "(S3)methods will work like normally. \n"
  ))
  out <- as.environment(namespaces)
  eval(call("<-", substitute(alias), out), envir = parent.frame(n = 1))
}


#' @rdname import
#' @export
import_inops <- function(pkgs, lib.loc=.libPaths(), exclude, include.only) {
  if(length(pkgs)!=length(unique(pkgs))) {
    stop("one or more duplicate packages given")
  }
  if(!missing(exclude) & !missing(include.only)){
    stop("Canntot specify both `exclude` and `include.only`; specify only one or none.")
  }
  
  if(length(pkgs)>1) {
    check_deps_OK <- .internal_check_deps_overlap_any(pkgs, lib.loc=lib.loc)
    if(!check_deps_OK) {
      error.txt <- paste0(
        "Multiple packages specified, but the packages have no dependency overlap at all.",
        "\n",
        "Function halted."
      )
      stop(error.txt)
    }
  }

  export_names_all <- character()
  export_names_allconflicts <- character()
  namespaces <- list()

  for (i in 1:length(pkgs)) {
    message(paste0("Getting infix operators from package: ", pkgs[i], "..."))
    namespace_current <- .internal_prep_Namespace(pkgs[i], lib.loc)
    export_names_current <-  grep("%|:=", names(namespace_current), value=TRUE)

    if(length(export_names_current)==0){
      message("no infix operators in this package; skipping...")
    }

    if(length(export_names_current)>0) {

      export_names_intersection <- intersect(export_names_current, export_names_all)
      if(length(export_names_intersection)==0 & i>1) {
        message("no conflicts")
      }
      if(length(export_names_intersection)>0) {
        message(
          "The following conflicting infix operators detected:",
          "\n \n",
          paste0(export_names_intersection, collapse = ", "),
          "\n \n",
          pkgs[i], " will overwrite conflicting infix operators from previous packages..."
        )
      }
      export_names_allconflicts <- c(export_names_intersection, export_names_allconflicts)
      export_names_all <- c(export_names_current, export_names_all)
      namespaces <- utils::modifyList(namespaces, namespace_current)
      message("\n")
    }
  }

  operators <- grep("%|:=", names(namespaces), value=TRUE)
  if(!missing(exclude)){operators <- setdiff(operators, exclude)}
  if(!missing(include.only)){operators <- intersect(operators, include.only)}
  if(length(operators)==0){
    message(
      "No operators to expose..."
    )
  }
  if(length(operators)>0) {
    message(
      "Placing infix operators in current environment..."
    )
    for(op in operators){
      eval(call("<-", op, namespaces[[op]]), envir = parent.frame(n = 1))
    }
    message("Done")
  }
}


#' @rdname import
#' @export
import_data <- function(dataname, package, lib.loc=.libPaths()) {
  if(length(dataname)>1 | length(package)>1) {
    stop("only a single dataset and a single package can be given")
  }
  return(get(
    utils::data(list=dataname, package = package, lib.loc=lib.loc, envir = environment())
  ))
}

#' @rdname import
#' @export
import_lsf <- function(package, type, lib.loc=.libPaths()) {
  if(length(package)>1){
    stop("only a single package can be given")
  }
  ns <- .internal_prep_Namespace(package, lib.loc) |> names()
  if(type=="inops" | type=="operators") {
    out <- grep("%|:=", ns, value = TRUE)
  }
  if(type=="regfuns") {
    out <- grep("%|:=", ns, value = TRUE, invert = TRUE)
  }
  if(type=="all") {
    out <- ns
  }
  return(out)
}



