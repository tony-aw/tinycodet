#' Additional package import management
#'
#' @description
#' These functions and operator are focused on making it easier to
#' use packages without having to explicitly attaching them to your namespace. \cr
#' \cr
#' \code{import_as}: \cr
#' The \code{import_as()} function
#' imports the namespace of an R package,
#' and optionally also its dependencies, enhances, and extensions,
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
#' @param main_package a single string, giving the name of the main package to load under the given alias.
#' @param package the quoted package name.
#' @param depends either logical, or a character vector. \cr
#' If \code{FALSE} (default), no dependencies are loaded under the alias. \cr
#' If \code{TRUE}, ALL dependencies of the \code{main_package} are loaded under the alias. \cr
#' If a character vector, then it is taken as the dependencies of the
#' package to be loaded also under the alias. \cr
#' NOTE (1): "Dependencies" here are defined as any package appearing in the
#' "Depends", "Imports", or "LinkingTo" sections of the Description file of the
#' \code{main_package}. \cr
#' NOTE (2): If \code{depends} is a character vector:
#' The order of the character vector matters!
#' If multiple packages share objects with the same name,
#' the package named last will overwrite the earlier named package. \cr
#' @param enhances either logical, or a character vector. \cr
#' If \code{FALSE} (default), no enhances are loaded under the alias. \cr
#' If \code{TRUE}, ALL "Enhances" packages are loaded under the alias. \cr
#' If a character vector, then it is taken as the list of enhanced packages
#' to be loaded also under the alias. \cr
#' NOTE(1): Enhances are defined as packages appearing in the "Enhances" section
#' of the Description file of the \code{main_package}. \cr
#' NOTE (2): The order of the character vector matters!
#' If multiple packages share objects with the same name,
#' the package named last will overwrite the earlier named package. \cr
#' @param extends a character vector,
#' giving the names of the reverse-dependencies of the
#' \code{main_package} to be loaded also under the alias.
#' Defaults to \code{NULL}, which means no extensions are loaded. \cr
#' NOTE (1): "Extensions" here are defined as reverse-depends or reverse-imports.
#' It does not matter if these are CRAN or non-CRAN packages. \cr
#' NOTE (2): The order of the character vector matters!
#' If multiple packages share objects with the same name,
#' the package named last will overwrite the earlier named package. \cr
#' @param pkgs a single string, or character vector, with the package name(s). \cr
#' NOTE (1): The order of the character vector matters!
#' If multiple packages share objects with the same name,
#' the package named last will overwrite the earlier named package. \cr
#' NOTE (2): The \code{import_inops} function performs a basic check
#' that the packages are mostly (reverse) dependencies of each other.
#' If not, it will give an error.
#' @param exclude a character vector,
#' giving the infix operators NOT to expose to the current environment. \cr
#' This can be handy to prevent overwriting any (user defined)
#' infix operators already present in the current environment. \cr
#' NOTE: You cannot specify both the \code{exclude} and \code{include.only} arguments.
#' Only one or the other, or neither.
#' @param include.only a character vector,
#' giving the infix operators to expose to the current environment,
#' and the rest of the operators will not be exposed. \cr
#' This can be handy to prevent overwriting any (user defined)
#' infix operators already present in the current environment. \cr
#' NOTE: You cannot specify both the \code{exclude} and \code{include.only} arguments.
#' Only one or the other, or neither.
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through).
#' This is usually \code{.libPaths()}.
#' See also \link[base]{loadNamespace}.
#' @param dataname a single string, giving the name of the data set.
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
#' The \code{import_as()} function will load the packages in the following order: \cr
#' (1) \code{depends}, (2) \code{main_package}, (3) \code{enhances}, (4) \code{extends}. \cr
#' Note that suggested packages are not included in the \code{import_as()} function.
#' The \code{import_as()} function does not import internal functions
#' (i.e. internal functions are kept internal, as they should). \cr
#' \cr
#' For \code{import_inops()}: \cr
#' The \code{import_inops()} function is less strict than \code{import_as()}
#' in terms of which R packages can be called together.
#' But still the packages specified in argument \code{pkgs} need to have SOME
#' overlap in their dependencies. \cr
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
#' depends <- unlist(tools::package_dependencies("devtools"))
#' pkgs <- c(depends, "devtools")
#' import_as(devt, "devtools", depends = TRUE) # this creates the devt object
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
    alias, main_package, depends=FALSE, enhances=FALSE, extends=NULL, 
    lib.loc=.libPaths()
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
  if(length(main_package)>1){
    stop("Only a single package can be given in the `main_package` argument")
  }
  check_install <- main_package %installed in% lib.loc
  if(isFALSE(check_install)) {
    stop("Given main_package not installed!")
  }
  
  # Check dependencies:
  depends <- .internal_import_as_check_depends(main_package, depends, lib.loc)
  
  # Check enhances:
  enhances <- .internal_import_as_check_enhances(main_package, enhances, lib.loc)
  
  # Check extends:
  extends <- .internal_import_as_check_extends(main_package, extends, lib.loc)
  
  pkgs <- c(depends, main_package, enhances, extends) |> unique()
  
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
  
  # check packages:
  if(length(pkgs)!=length(unique(pkgs))) {
    stop("one or more duplicate packages given")
  }
  wrong_pkgs<- pkgs[!pkgs%installed in% lib.loc]
  if(length(wrong_pkgs)>0) {
    error.txt <- paste0(
      "The following packages are not installed:",
      "\n",
      paste0(wrong_pkgs, collapse = ", ")
    )
    stop(error.txt)
  }
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
    stop("Canntot specify both `exclude` and `include.only`; specify only one or none.")
  }
  
  # FUNCTION:
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
  
  check <- package %installed in% lib.loc
  if(!isTRUE(check)) {
    stop("package not installed")
  }
  
  if(!type %in% c("inops", "operators", "regfuns", "all")) {
    stop("`type` must be one of `inops`, `operators`, `regfuns`, or `all`")
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



