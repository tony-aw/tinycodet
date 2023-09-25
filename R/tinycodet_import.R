#' The tinycodet import system
#'
#' @description
#' The tinycodet R package introduces a new package import system. \cr
#' \cr
#' One can use a package without attaching the package
#' (i.e. using `::` or using a package alias),
#' or one can attach a package
#' (i.e. using `library()` or `require()`).
#' The advantages and disadvantages of loading without attaching a package versus attaching a package - at least those relevant here - can be compactly presented in the following table: \cr
#'
#' ```{r echo=FALSE, results='markup'}
#' X <- rbind(
#'   c("prevent masking functions from other packages", "|", " Yes (+)", "|", " No (-)"),
#'   c("", "|", " ", "|", " "),
#'   c("prevent masking core R functions", "|", " Yes (+)", "|", " No (-)"),
#'   c("", "|", " ", "|", " "),
#'   c("clarify which function came from which package", "|", " Yes (+)", "|", " No (-)"),
#'   c("", "|", " ", "|", " "),
#'   c("place/expose functions only in current environment instead of globally", "|", " Yes (+)", "|", " No (-)"),
#'   c("", "|", " ", "|", " "),
#'   c("prevent namespace pollution", "|", " Yes (+)", "|", " No (-)"),
#'   c("", "|", " ", "|", " "),
#'   c("minimize typing - especially for infix operators
#'   (i.e. typing ``package::`%op%`(x, y)`` instead of `x %op% y` is cumbersome)",
#'     "|", " No (-)", "|", " Yes (+)"),
#'   c("", "|", " ", "|", " "),
#'   c("use multiple related packages,
#'   without constantly switching between package prefixes" , "|", " No (-)", "|", " Yes (+)"),
#'   c("", "|", " ", "|", " "),
#'   c("NOTE: + = advantage, - = disadvantage", "", "", "", "" )
#' )
#' colnames(X) <- c("aspect", "|",  "alias / ::", "|", "attaching")
#' X <- as.data.frame(X)
#' knitr::kable(X, row.names=FALSE)
#' ```
#'
#' What `tinycodet` attempts to do with its import system,
#' is to somewhat find the best of both worlds.
#' It does this by introducing the following functions: \cr
#'
#' - \link{import_as}:
#' Allow a main package + its re-exports + its dependencies + its extensions to be loaded under a single alias.
#' This essentially combines the attaching advantage of using multiple related packages,
#' whilst keeping most advantages of aliasing a package.
#' - \link{import_inops}:
#' Expose infix operators from a package or an alias object to the current environment.
#' This gains the attaching advantage of less typing,
#' whilst simultaneously avoiding the disadvantage of attaching functions from a package globally.
#' - \link{import_data}:
#' Directly return a data set from a package,
#' to allow straight-forward assignment.
#'
#' Furthermore, there are two miscellaneous \code{import_} - functions:
#' \link{import_LL} and \link{import_int}. \cr
#' And there are also some additional helper functions for the package import system,
#' see \link[=help.import]{x.import} and \link[=pkg_get_deps]{pkgs}. \cr
#' \cr
#'
#' @md
#'
#'
#' @seealso \link{tinycodet_help}
#'
#' @examples
#' \dontrun{
#' # loading "tidytable" + "data.table" under alias "tdt.":
#' import_as(
#'   ~ tdt., "tidytable", dependencies = "data.table"
#' )
#'
#' # exposing operators from "magrrittr" to current environment:
#' import_inops("magrittr")
#'
#' # directly assigning dplyr's "starwars" dataset to object "d":
#' d <- import_data("dplyr", "starwars")
#'
#' # see it in action:
#' d %>% tdt.$filter(species == "Droid") %>%
#'   tdt.$select(name, tdt.$ends_with("color"))
#'
#' }
#'


#' @rdname tinycodet_import
#' @export
tinycodet_import <- function() {
  utils::`?`(tinycodet_import)
}
