#' The tinycodet import system
#'
#' @description
#' The tinycodet R package introduces a new package import system. \cr
#' \cr
#' One can load a package \bold{without} attaching the package -
#' i.e. using \link[base]{::} ("colons") or aliasing the package. \cr
#' Or, one can \bold{attach} a package by using \link[base]{library} or \link[base]{require}. \cr
#' The advantages and disadvantages
#' of loading without attaching a package versus attaching a package,
#' at least those relevant here,
#' are compactly presented in the following list: \cr
#' \cr
#' (1) Prevent masking functions from other packages: \cr
#' `r .mybadge_import("alias or colons", "Yes(advantage)", "darkgreen")` \verb{ }
#' `r .mybadge_import("attaching", "No(disadvantage)", "red")` \cr
#' \cr
#' (2) Prevent masking core R functions: \cr
#' `r .mybadge_import("alias or colons", "Yes(advantage)", "darkgreen")` \verb{ }
#' `r .mybadge_import("attaching", "No(disadvantage)", "red")` \cr
#' \cr
#' (3) Clarify which function came from which package: \cr
#' `r .mybadge_import("alias or colons", "Yes(advantage)", "darkgreen")` \verb{ }
#' `r .mybadge_import("attaching", "No(disadvantage)", "red")` \cr
#' \cr
#' (4) Place/expose functions only in current environment instead of globally: \cr
#' `r .mybadge_import("alias or colons", "Yes(advantage)", "darkgreen")` \verb{ }
#' `r .mybadge_import("attaching", "No(disadvantage)", "red")` \cr
#' \cr
#' (5) Prevent namespace pollution: \cr
#' `r .mybadge_import("alias or colons", "Yes(advantage)", "darkgreen")` \verb{ }
#' `r .mybadge_import("attaching", "No(disadvantage)", "red")` \cr
#' \cr
#' (6) Minimize typing - especially for infix operators \cr
#' (i.e. typing ``package::`%op%`(x, y)`` instead of \code{x %op% y} is cumbersome): \cr
#' `r .mybadge_import("alias or colons", "No(disadvantage)", "red")` \verb{ }
#' `r .mybadge_import("attaching", "Yes(advantage)", "darkgreen")` \cr
#' \cr
#' (7) Use multiple related packages,
#' without constantly switching between package prefixes \cr
#' (i.e. doing \code{packagename1::some_function1()}; \cr
#' \code{packagename2::some_function2()}; \cr
#' \code{packagename3::some_function3()} is chaotic and cumbersome): \cr
#' `r .mybadge_import("alias or colons", "No(disadvantage)", "red")` \verb{ }
#' `r .mybadge_import("attaching", "Yes(advantage)", "darkgreen")` \cr
#' \cr
#'
#' What `tinycodet` attempts to do with its import system,
#' is to somewhat find the best of both worlds.
#' It does this by introducing the following functions: \cr
#'
#' - \link{import_as}:
#' Allow a main package + its re-exports + its dependencies + its extensions to be loaded under a single alias.
#' This essentially combines the attaching advantage of using multiple related packages (item 7 on the list),
#' whilst keeping most advantages of aliasing a package.
#' - \link{import_inops}:
#' Expose infix operators from a package or an alias object to the current environment.
#' This gains the attaching advantage of less typing (item 6 on the list),
#' whilst simultaneously avoiding the disadvantage of attaching functions from a package globally (item 4 on the list).
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
#' # exposing infix operators from "magrrittr" to current environment:
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
