#' Miscellaneous package functions
#'
#' @description
#' The \code{pkgs %installed in% lib.loc} operator
#' checks if one or more package(s) \code{pkgs} exist(s)
#' in library location \code{lib.loc},
#' and does so WITHOUT attaching or even loading the package(s). \cr
#' Moreover, this operator forces the user to make it
#' syntactically explicit
#' where one is looking for installed R package(s). \cr
#' \cr
#' The \code{pkg_get_deps()} function gets the dependencies of a package
#' from the Description file. It works on non-CRAN packages also. \cr
#' \cr
#' The \code{help.import()} function
#' finds the help file for exposed infix operators and functions in an alias object. \cr
#' \cr
#' The \code{pkg_lsf(package, ...)} function
#' gets a list of exported functions/operators from a package. \cr
#' One handy use for this function is to, for example,
#' globally attach all infix operators from a function using \code{library},
#' like so: \cr
#' \code{library(packagename, include.only = pkg_lsf("packagename", type="inops"))} \cr
#' \cr
#'
#' @param pkgs a single string, or character vector, with the package name(s).
#' @param package a single string giving the package name.
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through). \cr
#' This is usually \code{.libPaths()}. \cr
#' See also \link[base]{loadNamespace}.
#' @param deps_type a character vector, giving the dependency types to be used. \cr
#' Defaults to \code{c("LinkingTo", "Depends", "Imports")}. \cr
#' The order of the character vector given in \code{deps_type} affects
#' the order of the returned character vector; see Details sections.
#' @param base logical,
#' indicating whether base/core R should be included (\code{TRUE}),
#' or not included (\code{FALSE}; the default).
#' @param recom logical,
#' indicating whether the pre-installed "recommended" R packages should be included (\code{TRUE}),
#' or not included (\code{FALSE}; the default).
#' Note that only the recommended R packages actually installed in your system are taken into consideration.
#' @param rstudioapi logical,
#' indicating whether the \code{rstudioapi} R package should be included (\code{TRUE}),
#' or not included (\code{FALSE}; the default).
#' @param i either one of the following: \cr
#'  * a function (use back-ticks when the function is an infix operator).
#'  Examples:  \code{myfun} , \code{`\%operator\%`} , \code{myalias.$some_function} .
#'  * a string giving the function name or topic (i.e. \code{"myfun"}, \code{"thistopic"}).
#'  If a string, argument \code{alias} must be specified also.
#' @param alias the alias object as created by the \link{import_as} function.
#' Only needs to be specified if argument \code{i} is a string, otherwise it is ignored.
#' @param ... further arguments to be passed to \link[utils]{help}.
#' @param type The type of functions to list. Possibilities: \cr
#'  * \code{"inops"} or \code{"operators"}: Only infix operators.
#'  * \code{"regfuns"}: Only regular functions (thus excluding infix operators).
#'  * \code{"all"}: All functions, both regular functions and infix operators.
#'
#' @details
#' For \code{help.import(...)}: \cr
#' do not use the \code{topic} / \code{package} and
#' \code{i} / \code{alias} arguments together. It's either one set or the other. \cr
#' \cr
#' \cr
#' For \code{pkg_get_deps()}: \cr
#' If using the \code{pkgs_get_deps()} function
#' to fill in the \code{dependencies} argument of the \link{import_as} function,
#' one may want to know the how character vector returned by \code{pkgs_get_deps()} is ordered. \cr
#' The order is determined as follows. \cr
#' For each string in argument \code{deps_type},
#' the package names in the corresponding field of the Description file are extracted,
#' in the order as they appear in that field. \cr
#' The order given in argument \code{deps_type}
#' also affects the order of the returned character vector: \cr
#' The default, \cr
#' \code{c("LinkingTo", "Depends", "Imports")}, \cr
#' means the package names are extracted from the fields in the following order:
#'
#' \enumerate{
#'  \item "LinkingTo";
#'  \item "Depends";
#'  \item "Imports".
#' }
#' The unique (thus non-repeating) package names are then returned to the user.
#'
#'
#' @returns
#' For  \code{pkgs %installed in% lib.loc}: \cr
#' Returns a logical vector, where \code{TRUE} indicates a package is installed,
#' and \code{FALSE} indicates a package is not installed. \cr
#' \cr
#' For \code{pkg_get_deps()}: \cr
#' A character vector of unique dependencies. \cr
#' \cr
#' For \code{pkg_lsf()}: \cr
#' Returns a character vector of function and/or operator names. \cr
#' \cr
#' For \code{help.import()}: \cr
#' Opens the appropriate help page.
#'
#'
#' @seealso \link{tinyoperations_import}
#'
#' @references https://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran
#'
#'
#' @examples
#'
#' \dontrun{
#' pkgs <- c(unlist(tools::package_dependencies("devtools")), "devtools")
#' pkgs %installed in% .libPaths()
#' pkg_lsf("devtools", "all")
#' import_as(m., "magrittr")
#' import_inops("magrittr")
#' help.import(i=m.$add)
#' help.import(i=`%>%`)
#' help.import(i="add", alias=m.)
#' }
#'
#'
#'

#' @name pkgs
NULL

#' @rdname pkgs
#' @export
pkg_get_deps <- function(
    package, lib.loc=.libPaths(), deps_type=c("LinkingTo", "Depends", "Imports"),
    base=FALSE, recom=FALSE, rstudioapi=FALSE
) {
  if(length(package)>1){
    stop("Only one package can be given")
  }

  temp.fun <- function(x) { .internal_get_pkg_deps(
      package, lib.loc, x, base=base, recom = recom, rstudioapi = rstudioapi
  )}
  depends <- lapply(
    deps_type, FUN = temp.fun
  )
  depends <- do.call(c, depends) |> unique()

  return(depends)
}

#' @rdname pkgs
#' @export
`%installed in%` <- function(pkgs, lib.loc) {
  temp.fun <- function(x)nzchar(system.file(package=x, lib.loc=lib.loc))
  out <- sapply(pkgs, temp.fun)
  return(out)
}

#' @rdname pkgs
#' @export
help.import <- function(..., i, alias) {

  # check arguments:
  lst <- list(...)
  args_base <- any(names(lst) %in% c("package", "topic"))
  args_import <- !missing(i) | !missing(alias)
  if(args_base & args_import) {
    stop("you cannot provide both `package`/`topic` AND `i`/`alias`")
  }
  if(!missing(alias)) {
    if(!is.environment(alias)) {
      stop("`alias` must be an alias object")
    }
  }
  if(!missing(i)) {
    if(isFALSE(is.character(i)) & isFALSE(is.function(i))) {
      stop("`i` must be a function or string")
    }
  }

  if(args_base) {
    return(utils::help(...))
  }


  temp.fun <- function(f){
    f.env <- environment(f)
    if(!is.null(f.env)) { package <- getNamespaceName(f.env) }
    if(is.null(f.env)) { package <- attr(f, "package") }
    fun_name <- attr(f, "funtion_name")
    if(is.null(fun_name)) {
      stop(
        "no function name attribute found",
        "\n",
        "are you sure the function comes from `tinyoperations::import_as()` or `tinyoperations::import_inops()`?")
    }
    return(utils::help(fun_name, package = (package), ...))
  }

  if(!missing(i)) { # start i

    if(is.function(i)) { # start i is a function
      return(temp.fun(i))
    } # end i is a function


    if(is.character(i)) { # start i is a character
      if(missing(alias)) {
         stop("if `i` is specified as a string, `alias` must also be supplied")
      }

      if(i %in% names(alias)) {
        i <- alias[[i]]
        return(temp.fun(i))
      }

      if(!i %in% names(alias)) {
        pkgs <- c(
          eapply(alias, FUN=\(x)getNamespaceName(environment(x))) |> unlist(),
          eapply(alias, FUN=\(x)attr(x, "package")) |> unlist()
        ) |> unique()
        return(utils::help(topic = (i), package = (pkgs), ...))
      }

    } # end i is a character

  } # end i
}


#' @rdname pkgs
#' @export
pkg_lsf <- function(package, type, lib.loc=.libPaths()) {
  if(length(package)>1){
    stop("only a single package can be given")
  }

  check <- .internal_require_ns(package, lib.loc)
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
