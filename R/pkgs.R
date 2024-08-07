#' Miscellaneous Package Related Functions
#'
#' @description
#' The \code{pkgs %installed in% lib.loc} operator
#' checks if one or more given packages (\code{pkgs}) exist
#' in the given library paths (\code{lib.loc}),
#' without loading the packages. \cr
#' The syntax of this operator forces the user to make it
#' syntactically explicit
#' where to look for installed R-packages. \cr
#' As \code{pkgs %installed in% lib.loc} does not even load a package,
#' the user can safely use it
#' without fearing any unwanted side-effects. \cr
#' \cr
#' The \code{pkg_get_deps()} function gets the \bold{direct} dependencies of a package
#' from the Description file. It works on non-CRAN packages also. \cr
#' \cr
#' The \code{pkg_get_deps_minimal()} function is the same as
#' \code{pkg_get_deps()},
#' except with
#' \code{base, recom, rstudioapi, shared_tidy}
#' all set to \code{FALSE},
#' and the default value for \code{deps_type} is c("Depends", "Imports"). \cr
#' \cr
#' The \code{pkg_lsf()} function
#' gets a list of exported functions/operators from a package. \cr
#' One handy use for this function is to, for example,
#' globally attach all infix operators from a package using \code{library},
#' like so:
#'
#' ```{r echo = TRUE, eval = FALSE}
#' y <- pkg_lsf("packagename", type = "inops")
#' library(packagename, include.only = y)
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
#' The order of the character vector given in \code{deps_type} affects
#' the order of the returned character vector; see Details sections.
#' @param base logical,
#' indicating whether base/core R should be included (\code{TRUE}),
#' or not included (\code{FALSE}).
#' @param recom logical,
#' indicating whether the pre-installed 'recommended' R-packages should be included
#' (\code{TRUE}),
#' or not included (\code{FALSE}).
#' @param rstudioapi logical,
#' indicating whether the 'rstudioapi' R-package should be included
#' (\code{TRUE}),
#' or not included (\code{FALSE}).
#' @param shared_tidy logical,
#' indicating whether the shared dependencies of the 'tidyverse' should be included
#' (\code{TRUE}),
#' or not included (\code{FALSE}). \cr
#' \bold{Details:} \cr
#' Some of the (often many) dependencies 'tidyverse' packages have
#' are shared across the majority of the 'tidyverse'. \cr
#' The "official" list of shared dependencies in the 'tidyverse' currently is the following: \cr
#' 'rlang', 'lifecycle', 'cli', 'glue', and 'withr'.
#' @param type The type of functions to list. Possibilities:
#'  * \code{"inops"} or \code{"operators"}: Only infix operators.
#'  * \code{"regfuns"}: Only regular functions (thus excluding infix operators).
#'  * \code{"all"}: All functions, both regular functions and infix operators. \cr \cr
#'
#' @details
#' For \code{pkg_get_deps()}: \cr
#' For each string in argument \code{deps_type},
#' the package names in the corresponding field of the Description file are extracted,
#' in the order as they appear in that field. \cr
#' The order given in argument \code{deps_type}
#' also affects the order of the returned character vector: \cr
#' For example, \code{c("LinkingTo", "Depends", "Imports")}, \cr
#' means the package names are extracted from the fields in the following order:
#'
#' \enumerate{
#'  \item "LinkingTo";
#'  \item "Depends";
#'  \item "Imports".
#' }
#' The unique (thus non-repeating)
#' package names are then returned to the user. \cr \cr
#'
#'
#' @returns
#' For  \code{pkgs %installed in% lib.loc}: \cr
#' Returns a named logical vector. \cr
#' The names give the package names. \cr
#' The value \code{TRUE} indicates a package is installed in `lib.loc`. \cr
#' The value \code{FALSE} indicates a package is not installed in `lib.loc`. \cr
#' The value \code{NA} indicates a package is not actually a separate package,
#' but base/core 'R'
#' (i.e. 'base', 'stats', etc.). \cr
#' \cr
#' For \code{pkg_get_deps()} and \code{pkg_get_deps_minimal()}: \cr
#' A character vector of direct dependencies, without duplicates. \cr
#' \cr
#' For \code{pkg_lsf()}: \cr
#' Returns a character vector of exported function names in the specified package. \cr \cr
#'
#' @references O'Brien J., elegantly extract R-package dependencies of a package not listed on CRAN. \emph{Stack Overflow}. (1 September 2023). \url{https://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran}
#'
#'
#' @seealso \link{tinycodet_import}
#'
#'
#'
#' @examplesIf "dplyr" %installed in% .libPaths()
#' "dplyr" %installed in% .libPaths()
#' 
#' pkg_get_deps_minimal("dplyr")
#' pkgs <- pkg_get_deps("dplyr")
#' pkgs %installed in% .libPaths()
#' pkg_lsf("dplyr", "all")
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
    stop("left hand side must be a character vector of package names")
  }
  misspelled_pkgs <- pkgs[pkgs != make.names(pkgs)]
  if(length(misspelled_pkgs) > 0L) {
    stop(
      "You have misspelled the following packages:",
      "\n",
      paste0(misspelled_pkgs, collapse = ", ")
    )
  }
   
  .internal_check_lib.loc(lib.loc, sys.call())

  tempfun <- function(pkg, lib.loc) {
    out <- find.package(package = pkg, lib.loc = lib.loc, quiet = TRUE)
    return(length(out) > 0L)
  }
  out <- vapply(pkgs, \(x)tempfun(x, lib.loc = lib.loc), logical(1), USE.NAMES = TRUE)
  out[pkgs %in% .internal_list_coreR()] <- NA
  
  return(out)
}


#' @rdname pkgs
#' @export
pkg_get_deps <- function(
    package, lib.loc = .libPaths(), deps_type = c("LinkingTo", "Depends", "Imports"),
    base = FALSE, recom = TRUE, rstudioapi = TRUE, shared_tidy = TRUE
) {
  if(length(package)>1){
    stop("Only one package can be given")
  }
  .internal_check_lib.loc(lib.loc, sys.call())
  .internal_check_pkgs(package, lib.loc, abortcall = sys.call())
  
  check_opts <- vapply(
    list(base, recom, rstudioapi, shared_tidy),
    FUN = \(x)isTRUE(x) || isFALSE(x),
    FUN.VALUE = logical(1)
  )
  if(any(!check_opts)) {
    stop("arguments `base`, `recom`, `rstudioapi`, `shared_tidy` must each be either `TRUE` OR `FALSE`")
  }

  temp.fun <- function(x) { .internal_get_pkg_deps(
      package, lib.loc, type = x, base = base, recom = recom, rstudioapi = rstudioapi, shared_tidy = shared_tidy
  )}
  depends <- lapply(
    deps_type, FUN = temp.fun
  )
  depends <- do.call(c, depends) |> unique()

  return(depends)
}


#' @rdname pkgs
#' @export
pkg_get_deps_minimal <- function(package, lib.loc = .libPaths(), deps_type = c("Depends", "Imports")) {
  return(pkg_get_deps(
    package, lib.loc, deps_type, base = FALSE, recom = FALSE, rstudioapi = FALSE, shared_tidy = FALSE
  ))
}


#' @rdname pkgs
#' @export
pkg_lsf <- function(package, type, lib.loc = .libPaths()) {
  if(length(package) > 1L){
    stop("only a single package can be given")
  }

  .internal_check_lib.loc(lib.loc, sys.call())
  .internal_check_pkgs(package, lib.loc, abortcall = sys.call())

  if(!type %in% c("inops", "operators", "regfuns", "all")) {
    stop("`type` must be one of `inops`, `operators`, `regfuns`, or `all`")
  }

  ns <- .internal_prep_Namespace(package, lib.loc, abortcall = sys.call()) |> names()
  if(type == "inops" || type == "operators") {
    out <- .internal_grep_inops(ns, type = 2)
  }
  if(type == "regfuns") {
    out <- .internal_grep_inops(ns, type = 2, invert = TRUE)
  }
  if(type == "all") {
    out <- ns
  }
  return(out)
}

#' @keywords internal
#' @noRd
.internal_get_pkg_deps <- function(
    package, lib.loc, type,
    base, recom, rstudioapi, shared_tidy
) {
  # based on https://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran
  dcf <- read.dcf(file.path(system.file("DESCRIPTION", package = package, lib.loc = lib.loc)))
  # note: using system.file() here above in case of multiple libPaths
  # and also to deal with prioritizing loaded vs installed packages
  jj <- intersect(type, colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names = FALSE)
  val <- gsub("\\s.*", "", trimws(val))
  depends <- val[val != "R"]
  
  if(!base) {
    depends <- setdiff(depends, .internal_list_coreR())
  }
  if(!recom) {
    depends <- setdiff(depends, .internal_list_preinst())
  }
  if(!rstudioapi) {
    depends <- setdiff(depends, "rstudioapi")
  }
  if(!shared_tidy) {
    depends <- setdiff(depends, .internal_list_tidyshared())
  }
  
  return(depends)
}
