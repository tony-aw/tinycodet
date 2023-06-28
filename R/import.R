#' Additional package import management
#'
#' @description
#' These functions implement a new package import system,
#' that attempts to combine the benefits of aliasing a package with the benefits of attaching a package. \cr
#' \cr
#' \code{import_as}: \cr
#' The \code{import_as()} function
#' imports the namespace of an R package,
#' and optionally also its dependencies, enhances, and extensions,
#' under the same alias.
#' The specified alias will be placed in the current environment
#' (like the global environment, or the environment within a function). \cr
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
#' @param foreign_exports logical. \cr
#' Some R packages export functions that are  not defined in their own package,
#' but in their direct dependencies; "foreign exports", if you will. \cr
#' If \code{foreign_exports = TRUE} these foreign exports are added to the namespace of \code{main_package},
#' even if \code{dependencies = FALSE}. \cr
#' If \code{foreign_exports = FALSE}, these foreign exports are not added,
#' and the user must specify the appropriate packages in argument \code{dependencies}. \cr
#' Defaults to \code{TRUE}, which is similar to the behaviour of base R's \link{::} operator.
#' @param package the quoted package name.
#' @param dependencies either logical, or a character vector. \cr
#' If \code{FALSE} (default), no dependencies are loaded under the alias. \cr
#' If \code{TRUE}, ALL direct dependencies of the \code{main_package} are loaded under the alias,
#' but \bold{excluding} base/core R,
#' and also \bold{excluding} pre-installed "recommended" R packages. \cr
#' See also \link{pkgs_get_deps}. \cr
#' If a character vector, then it is taken as the direct dependencies of the
#' package to be loaded also under the alias. \cr
#' \cr
#' NOTE (1): "Dependencies" here are defined as any package appearing in the
#' "Depends", "Imports", or "LinkingTo" sections of the Description file of the
#' \code{main_package}. So no recursive dependencies. \cr
#' NOTE (2): If \code{dependencies} is a character vector:
#' The order of the character vector matters!
#' If multiple packages share objects with the same name,
#' the package named last will overwrite the earlier named packages. \cr
#' @param enhances a character vector,
#' giving the names of the packages enhanced by the
#' \code{main_package} to be loaded also under the alias. \cr
#' Defaults to \code{NULL}, which means no enhances are loaded. \cr
#' \cr
#' NOTE(1): Enhances are defined as packages appearing in the "Enhances" section
#' of the Description file of the \code{main_package}. \cr
#' NOTE (2): The order of the character vector matters!
#' If multiple packages share objects with the same name,
#' the objects of the package named last will overwrite those of the earlier named packages. \cr
#' @param extensions a character vector,
#' giving the names of the extensions / reverse-dependencies of the
#' \code{main_package} to be loaded also under the alias. \cr
#' Defaults to \code{NULL}, which means no extensions are loaded. \cr
#' \cr
#' NOTE (1): "Extensions" here are defined as reverse-depends or reverse-imports.
#' It does not matter if these are CRAN or non-CRAN packages. \cr
#' NOTE (2): The order of the character vector matters!
#' If multiple packages share objects with the same name,
#' the objects of the package named last will overwrite those of the earlier named packages. \cr
#' @param loadorder the character vector \cr
#' \code{c("dependencies", "main_package", "enhances", "extensions")}, \cr
#' or some re-ordering of this character vector,
#' giving the relative load order of the groups of packages. \cr
#' \cr
#' The default setting (which is highly recommended) is the character vector \cr
#' \code{c("dependencies", "main_package", "enhances", "extensions")}, \cr
#' which results in the following load order: \cr
#' (1) The dependencies, in the order specified by the \code{depenencies} argument. \cr
#' (2) The main_package (see argument \code{main_package}),
#' including foreigg exports (if \code{foreign_exports=TRUE}). \cr
#' (3) The enhances, in the order specified by the \code{enhances} argument. \cr
#' (4) The reverse-dependencies/extensions, in the order specified by the \code{extensions} argument. \cr
#' @param pkgs a single string, or character vector, with the package name(s). \cr
#' \cr
#' NOTE (1): The order of the character vector matters!
#' If multiple packages share objects with the same name,
#' the objects of the package named last will overwrite those of the earlier named packages. \cr
#' NOTE (2): The \code{import_inops} function performs a basic check
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
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through). \cr
#' This is usually \code{.libPaths()}. \cr
#' See also \link[base]{loadNamespace}.
#' @param dataname a single string, giving the name of the data set.
#' @param type The type of functions to list. Possibilities: \cr
#' \code{"inops"} or \code{"operators"}: Only infix operators. \cr
#' \code{"regfuns"}: Only regular functions (thus excluding infix operators). \cr
#' \code{"all"}: All functions, both regular functions and infix operators. \cr
#'
#'
#' @details
#' The \code{import_as()} function
#' does not allow importing base/core R under an alias, so don't try. \cr
#' For a more detailed description of the import system introduced by the
#' \code{tinyoperators} R package,
#' please refer to the Read Me file on the GitHub main page: \cr
#' \url{https://github.com/tony-aw/tinyoperators} \cr
#' 
#'
#' @returns
#' For \code{import_as}: \cr
#' The variable named in the \code{alias} argument will be created
#' (if it did not already exist),
#' in the current environment
#' (like the global environment, or the environment within a function).
#' The alias object will contain the (merged) package environment. \cr
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
#' \dontrun{
#' import_as( # this creates the dr object
#' dr, "dplyr", depends=c("tibble", "tidyselect"), extends = "powerjoin"
#' ) 
#' import_inops("data.table")
#' d <- import_data("chicago", "gamair")
#' head(d)
#' }
#'
#'

#' @name import
NULL

#' @rdname import
#' @export
import_as <- function(
    alias, main_package, foreign_exports=TRUE, dependencies=FALSE, enhances=NULL, extensions=NULL,
    lib.loc=.libPaths(),
    loadorder = c("dependencies", "main_package", "enhances", "extensions")
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
  
  # check main_package:
  if(length(main_package)>1){
    stop("Only a single package can be given in the `main_package` argument")
  }
  check_install <- main_package %installed in% lib.loc
  if(isFALSE(check_install)) {
    stop("Given main_package not installed!")
  }
  
  # check library:
  if(length(lib.loc)<1) {
    stop("At least one library path must be given")
  }
  
  # check load order:
  loadorder <- tolower(loadorder)
  check_loadorder <- all(sort(loadorder) == sort(c("dependencies", "main_package", "enhances", "extensions")))
  if(!isTRUE(check_loadorder)) {
    stop("Improper load order given")
  }
  
  # Check dependencies:
  dependencies <- .internal_import_as_check_depends(main_package, dependencies, lib.loc)
  
  # Check enhances:
  enhances <- .internal_import_as_check_enhances(main_package, enhances, lib.loc)
  
  # Check extensions:
  extensions <- .internal_import_as_check_extends(main_package, extensions, lib.loc)
  
  # make packages:
  pkgs <- list(dependencies=dependencies, main_package=main_package, enhances=enhances, extensions=extensions)
  pkgs <- pkgs[loadorder]
  pkgs <- do.call(c, pkgs)
  pkgs <- unique(pkgs)
  
  if(length(pkgs)>10) {
    message("Be careful: More than 10 packages are being loaded under the same alias")
  }
  
  # load packages:
  export_names_all <- character()
  export_names_allconflicts <- character()
  namespaces <- list()
  for (i in 1:length(pkgs)) {
    namespace_current <- .internal_prep_Namespace(pkgs[i], lib.loc)
    
    if(pkgs[i]==main_package & foreign_exports) {
      message("listing foreign exports from package: ", pkgs[i], "...")
      namespace_current <- utils::modifyList(
        namespace_current,
        .internal_get_foreignexports_ns(main_package, lib.loc)
      )
      
    }
    
    export_names_current <- names(namespace_current)
    
    export_names_intersection <- intersect(export_names_current, export_names_all)
    if(i==1){
      message("Importing package: ", pkgs[i], "...")
    }
    if(length(export_names_intersection)==0 & i>1) {
      message("Importing package: ", pkgs[i], "... no conflicts")
    }
    if(length(export_names_intersection)>0) {
      message(
        "Importing package: ", pkgs[i], "... The following conflicting objects detected:",
        "\n",
        paste0(export_names_intersection, collapse = ", "),
        "\n",
        pkgs[i], " will overwrite conflicting objects from previous imported packages..."
      )
    }
    export_names_allconflicts <- c(export_names_intersection, export_names_allconflicts)
    export_names_all <- c(export_names_current, export_names_all)
    namespaces <- utils::modifyList(namespaces, namespace_current)
    message("")
  }
  
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
  
  # check library:
  if(length(lib.loc)<1) {
    stop("At least one library path must be given")
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
          "Getting infix operators from package: ", pkgs[i], "... The following infix operators detected:",
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



