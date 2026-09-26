#' List Exported Objects from Package Namespace
#'
#' @description
#' Lists exported objects defined in (but not re-exported by) a package namespace. \cr
#' Note that `import_ls()` necessary loads the package,
#' but does not attach the package. \cr
#' \cr
#' 
#'
#' @param package a single string, giving the package name.
#' @param types a character vector, specifying the type. \cr
#' The following types are supported: 
#'  - "reg": regular functions.
#'  - "infix": infix operators.
#'  - "rp": replacement operators, including their base functions.
#'  - "nonfun": non-functions.
#' @param re_exports `TRUE` or `FALSE`, indicating if re-exports of `package` should be included. \cr
#' Default is `TRUE`, as that is analogous to the behaviour of base R's \link[base]{::} operator.
#' @param lib.loc a character vector describing the location of R library trees to search through.
#' @param print `TRUE` (default) or `FALSE`,
#' indicating if the exported objects should be printed to the console as literal code. \cr
#' Note that this is a \bold{side-effect},
#' and does not impact the storable return value of `import_ls()`. \cr \cr
#'
#' @details
#' \bold{Why Listing Functions by Type is Useful} \cr
#' One can import a package under an alias using \link{import_as}. \cr
#' But using infix operators or replacement operators from an alias
#' requires convoluted code like so: \cr
#' 
#' ```{r echo = TRUE, eval = FALSE}
#' 
#' .alias$`%op%`(x, y)
#' .alias$`fun<-`(x, ..., value)
#' 
#' ```
#' 
#' Instead, it would be easier to just attach (via \link[base]{library})
#' or expose (via \link{import_from})
#' such operators so that they can be used on their own. \cr
#' The `import_ls()` function allows the user to get a list of all functions from a certain type,
#' like "infix operators" or "replacement operators". \cr
#' The listed functions can then be passed to
#' \link[base]{library} (to attach them)
#' or \link{import_from} (to expose them). \cr
#' \cr \cr
#' \bold{Programmaticly Dynamic Code or Syntactically Readable Code} \cr
#' The return value of `import_ls()` is for programmatically dynamic code (see section `Value`). \cr
#' The side-effect of `import_ls()` is for syntactically readable code (see section `Side Effect`). \cr
#' The trade-off between these 2 options is sometimes referred to as the tension between dynamism and readability. \cr \cr
#'
#' @returns
#' A character vector of exported object names defined in the package. \cr
#' Can be used programmatically in \link[base]{library} or \link{import_from} functions. \cr
#' I.e.:
#' 
#' ```{r echo = TRUE, eval = FALSE}
#' # Like so:
#' 
#' ls <- import_ls("packagename", "infix")
#' library(packagename, include.only = ls)
#' 
#' # Or like so:
#' 
#' ls <- import_ls("packagename", "infix")
#' import_from(packagename, ls = ls)
#' ```
#' @section Side Effect: 
#' \bold{(if `print = TRUE`)} \cr
#' The returned character vector is printed to your console as literal code. \cr
#' I.e. `'c("obj1", "obj2")'`. \cr
#' One can then copy-paste the printed literal code, for syntactical clarity. \cr
#' I.e.:
#' 
#' ```{r echo = TRUE, eval = FALSE}
#' 
#' # Like so:
#' 
#' import_ls("packagename", "infix") # prints literal code to your console
#' library(packagename, include.only = ...paste printed literal code here...)
#' 
#' # Or like so:
#' 
#' import_ls("packagename", "infix") # prints literal code to your console
#' import_from("packagename", ls = ...paste printed literal code here...)
#' ```
#' 
#' 
#'
#' @seealso \link{tinycodet_import}, \link{import_from}
#'
#'
#' @example inst/examples/import_ls.R
#'

#' @rdname import_ls
#' @export
import_ls <- function(
    package, types = c("reg", "infix", "rp", "nonfun"), re_exports = TRUE, lib.loc = .libPaths(),
    print = TRUE
) {
  
  .check_pkgenv(parent.frame(), sys.call())
  lib.loc <- .import_lib.loc(lib.loc, sys.call())
  
  if(!.internal_is_string(package)) {
    stop("`package` must be a string")
  }
  
  .check_forbidden_pkgs(package, lib.loc, abortcall = sys.call())
  .check_pkgs(package, lib.loc, abortcall = sys.call())
  
  
  types.sup <-  c("reg", "infix", "rp", "nonfun")
  if(!is.character(types) || (!length(types) %in% 1:4) || anyNA(types) || any(!types %in% types.sup)) {
    stop("`types` must be a non-duplicate character vector with at least one element, containing only {reg, infix, rp, nonfun}")
  }
  if(!isTRUE(re_exports) && !isFALSE(re_exports)) {
    stop("`re_exports` must be `TRUE` or `FALSE`")
  }
  
  
  ns <- .internal_prep_Namespace(package, lib.loc, sys.call())
  if(re_exports) {
    re <- .internal_get_reexports_ns(package, lib.loc, sys.call())
    ns <- utils::modifyList(ns, re)
  }
  exported <- names(ns)
  
  is_reg <- exported == make.names(exported)
  is_infix <- stringi::stri_detect_regex(exported, "%|:=")
  is_rpops <- stringi::stri_endswith_fixed(exported, "<-")
  rpbases <- stringi::stri_replace_all_fixed(exported[is_rpops], "<-", "")
  is_rpbase <- exported %in% rpbases
  is_nonfun <- vapply(ns, \(x) !is.function(x), FUN.VALUE = logical(1)) |>
    unlist(use.names = FALSE)
  
  exported <- list(
    reg = exported[is_reg & !is_nonfun],
    infix = exported[is_infix],
    rp = exported[is_rpops | is_rpbase],
    nonfun = exported[is_nonfun]
  )
  exported <- exported[types]
  exported <- do.call(c, exported) |> unique() |> sort()
  
  .check_import_post(package, lib.loc, sys.call())
  
  if(print) {
    dput(exported)
    return(invisible(exported))
  }
  else {
    return(exported)
  }
  
  
}


