#' Miscellaneous Package Related Functions
#'
#' @description
#' The \code{pkgs %installed in% lib.loc} operator
#' checks if one or more package(s) \code{pkgs} exist(s)
#' in library location \code{lib.loc}, without loading the package(s). \cr
#' The syntax of this operator forces the user to make it
#' syntactically explicit
#' where to look for installed R-package(s). \cr
#' As \code{pkgs %installed in% lib.loc} does not even load a package,
#' the user can safely use it
#' without fearing any unwanted side-effects. \cr
#' \cr
#' The \code{pkg_get_deps()} function gets the dependencies of a package
#' from the Description file. It works on non-CRAN packages also. \cr
#' \cr
#' The \code{pkg_lsf()} function
#' gets a list of exported functions/operators from a package. \cr
#' One handy use for this function is to, for example,
#' globally attach all infix operators from a function using \code{library},
#' like so:
#'
#' ```{r echo = TRUE, eval = FALSE}
#' library(packagename, include.only = pkg_lsf("packagename", type="inops"))
#' ```
#'
#'
#' @param pkgs a character vector with the package name(s).
#' @param package a single string giving the package name.
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through). \cr
#' The \code{lib.loc} argument would usually be \code{.libPaths()}. \cr
#' See also \link[base]{loadNamespace}.
#' @param deps_type a character vector, giving the dependency types to be used. \cr
#' Defaults to \code{c("LinkingTo", "Depends", "Imports")}. \cr
#' The order of the character vector given in \code{deps_type} affects
#' the order of the returned character vector; see Details sections.
#' @param base logical,
#' indicating whether base/core R should be included (\code{TRUE}),
#' or not included (\code{FALSE}; the default).
#' @param recom logical,
#' indicating whether the pre-installed "recommended" R-packages should be included (\code{TRUE}),
#' or not included (\code{FALSE}; the default).
#' Note that only the recommended R-packages actually installed in your system are taken into consideration.
#' @param rstudioapi logical,
#' indicating whether the \code{rstudioapi} R-package should be included (\code{TRUE}),
#' or not included (\code{FALSE}; the default).
#' @param type The type of functions to list. Possibilities:
#'  * \code{"inops"} or \code{"operators"}: Only infix operators.
#'  * \code{"regfuns"}: Only regular functions (thus excluding infix operators).
#'  * \code{"all"}: All functions, both regular functions and infix operators.
#'
#' @details
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
#' Returns a named logical vector, with the names giving the package names,
#' and where the value \code{TRUE} indicates a package is installed,
#' and the value \code{FALSE} indicates a package is not installed. \cr
#' \cr
#' For \code{pkg_get_deps()}: \cr
#' A character vector of unique dependencies. \cr
#' \cr
#' For \code{pkg_lsf()}: \cr
#' Returns a character vector of exported function names in the specified package.
#'
#' @references O'Brien J., elegantly extract R-package dependencies of a package not listed on CRAN. \emph{Stack Overflow}. (1 September 2023). \url{https://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran}
#'
#'
#' @seealso [tinycodet_import()]
#'
#'
#'
#' @examples
#'
#' check <- "dplyr" %installed in% .libPaths()
#'
#' if(check) pkgs <- pkg_get_deps("dplyr") # many dependencies
#' if(check) pkgs %installed in% .libPaths()
#' if(check) pkg_lsf("dplyr", "all")
#'
#'
#'
#'

#' @name pkgs
NULL


#' @rdname pkgs
#' @export
`%installed in%` <- function(pkgs, lib.loc) {

  if(!is.character(pkgs)) {
    stop("`pkgs` must be a character vector of package names")
  }
  misspelled_pkgs <- pkgs[pkgs != make.names(pkgs)]
  if(isTRUE(length(misspelled_pkgs)>0)) {
    stop(
      "You have misspelled the following packages:",
      "\n",
      paste0(misspelled_pkgs, collapse = ", ")
    )
  }

  .internal_check_lib.loc(lib.loc, sys.call())

  tempfun <- function(pkg, lib.loc){
    out <- tryCatch(
      {find.package(pkg, lib.loc = lib.loc)},
      error = function(cond) FALSE,
      warning = function(cond) FALSE
    )
    if(is.character(out) && !isFALSE(out)) { out <- TRUE }
    return(out)
  }

  out <- sapply(pkgs, \(x)tempfun(x, lib.loc = lib.loc), USE.NAMES = TRUE)
  return(out)
}


#' @rdname pkgs
#' @export
pkg_get_deps <- function(
    package, lib.loc=.libPaths(), deps_type=c("LinkingTo", "Depends", "Imports"),
    base=FALSE, recom=FALSE, rstudioapi=FALSE
) {
  if(length(package)>1){
    stop("Only one package can be given")
  }
  .internal_check_lib.loc(lib.loc, sys.call())
  .internal_check_pkgs(package, lib.loc, abortcall = sys.call())

  temp.fun <- function(x) { .internal_get_pkg_deps(
      package, lib.loc, x, base = base, recom = recom, rstudioapi = rstudioapi
  )}
  depends <- lapply(
    deps_type, FUN = temp.fun
  )
  depends <- do.call(c, depends) |> unique()

  return(depends)
}


#' @rdname pkgs
#' @export
pkg_lsf <- function(package, type, lib.loc = .libPaths()) {
  if(length(package)>1){
    stop("only a single package can be given")
  }

  .internal_check_lib.loc(lib.loc, sys.call())
  .internal_check_pkgs(package, lib.loc, abortcall = sys.call())

  if(!type %in% c("inops", "operators", "regfuns", "all")) {
    stop("`type` must be one of `inops`, `operators`, `regfuns`, or `all`")
  }

  ns <- .internal_prep_Namespace(package, lib.loc, abortcall = sys.call()) |> names()
  if(type=="inops" || type=="operators") {
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

#' @keywords internal
#' @noRd
.internal_get_pkg_deps <- function(
    package, lib.loc, type,
    base=FALSE, recom=FALSE, rstudioapi=FALSE
) {
  # based of https://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran
  dcf <- read.dcf(file.path(system.file("DESCRIPTION", package = package, lib.loc = lib.loc)))
  jj <- intersect(type, colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names=FALSE)
  val <- gsub("\\s.*", "", trimws(val))
  depends <- val[val != "R"]
  if(!base){
    pkgs_core <- .internal_list_coreR()
    depends <- setdiff(depends, pkgs_core)
  }
  if(!recom) {
    pkgs_preinst <- .internal_list_preinst()
    depends <- setdiff(depends, pkgs_preinst)
  }
  if(!rstudioapi) {
    depends <- setdiff(depends, "rstudioapi")
  }
  return(depends)
}
