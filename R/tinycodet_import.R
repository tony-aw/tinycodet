#' Overview of the 'tinycodet' Import System
#'
#' @description
#'
#' The 'tinycodet' R-package introduces a new package import system. \cr
#' \cr
#' One can \bold{use} a package \bold{without attaching} the package -
#' for example by using the \link[base]{::} operator. \cr
#' Or, one can explicitly \bold{attach} a package -
#' for example by using the \link[base]{library} function. \cr
#' The advantages and disadvantages
#' of \bold{using without attaching} a package versus \bold{attaching} a package,
#' at least those relevant here,
#' are compactly presented in the following list: \cr
#' \cr
#' (1) Prevent masking functions from other packages: \cr
#' `r .mybadge_import("use without attach", "Yes(advantage)", "darkgreen")` \verb{ }
#' `r .mybadge_import("attaching", "No(disadvantage)", "red")` \cr
#' \cr
#' (2) Prevent masking core R functions: \cr
#' `r .mybadge_import("use without attach", "Yes(advantage)", "darkgreen")` \verb{ }
#' `r .mybadge_import("attaching", "No(disadvantage)", "red")` \cr
#' \cr
#' (3) Clarify which function came from which package: \cr
#' `r .mybadge_import("use without attach", "Yes(advantage)", "darkgreen")` \verb{ }
#' `r .mybadge_import("attaching", "No(disadvantage)", "red")` \cr
#' \cr
#' (4) Enable functions only in current/local environment instead of globally: \cr
#' `r .mybadge_import("use without attach", "Yes(advantage)", "darkgreen")` \verb{ }
#' `r .mybadge_import("attaching", "No(disadvantage)", "red")` \cr
#' \cr
#' (5) Prevent namespace pollution: \cr
#' `r .mybadge_import("use without attach", "Yes(advantage)", "darkgreen")` \verb{ }
#' `r .mybadge_import("attaching", "No(disadvantage)", "red")` \cr
#' \cr
#' (6) Minimise typing - especially for infix operators \cr
#' (i.e. typing ``package::`%op%`(x, y)`` instead of \code{x %op% y} is cumbersome): \cr
#' `r .mybadge_import("use without attach", "No(disadvantage)", "red")` \verb{ }
#' `r .mybadge_import("attaching", "Yes(advantage)", "darkgreen")` \cr
#' \cr
#' (7) Use multiple related packages,
#' without constantly switching between package prefixes \cr
#' (i.e. doing \code{packagename1::some_function1()}; \cr
#' \code{packagename2::some_function2()}; \cr
#' \code{packagename3::some_function3()} is chaotic and cumbersome): \cr
#' `r .mybadge_import("use without attach", "No(disadvantage)", "red")` \verb{ }
#' `r .mybadge_import("attaching", "Yes(advantage)", "darkgreen")` \cr
#' \cr
#'
#' What 'tinycodet' attempts to do with its import system,
#' is to somewhat find the best of both worlds.
#' It does this by introducing the following functions: \cr
#'
#' - \link{import_as}:
#' Load a main package, and optionally its re-exports + its dependencies + its extensions, under a single alias.
#' This essentially combines the attaching advantage of using multiple related packages (item 7 on the list),
#' whilst keeping most advantages of using without attaching a package.
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
#' All \code{import_}-functions have the \code{lib.loc} argument
#' to specify the library path to load packages from,
#' thus allowing straight-forward project isolation. \cr
#' \cr
#' See the examples section below
#' to get an idea of how the 'tinycodet' import system works in practice.
#' More examples can be found on the website (\url{https://tony-aw.github.io/tinycodet/})
#'
#' @details
#' \bold{When to Use or Not to Use the 'tinycodet' Import System} \cr
#' The 'tinycodet' import system is helpful particularly
#' for packages that have at least one of the following properties:
#'
#'  * The namespace of the package(s) conflicts with other packages.
#'  * The namespace of the package(s) conflicts with core R,
#'  or with those of recommended R packages.
#'  * The package(s) have function names that are generic enough,
#'  such that it is not obvious which function came from which package.
#'
#' An example of a package that displays all 3 of the above properties,
#' at least at the time of writing,
#' is the 'dplyr' package - see examples below. \cr
#' \cr
#' There is no necessity for using the 'tinycodet' import system with every single package.
#' One can safely attach the 'stringi' package, for example,
#' as 'stringi' uses a unique and immediately recognisable naming scheme
#' (virtually all 'stringi' functions start with "\code{stri_}"),
#' and this naming scheme does not conflict with core R, nor with most other packages. \cr
#' \cr
#' Of course, if one wishes to use a package (like
#' 'stringi') \bold{only} within a specific environment,
#' like only inside a function,
#' it becomes advantageous to still load the package using the 'tinycodet' import system
#' (in that case the \link{import_LL} function would be most applicable). \cr
#' \cr \cr
#' \bold{Some Additional Comments on the 'tinycodet' Import System} \cr
#'
#'  * (S3) Methods will automatically be registered.
#'  * Pronouns, such as the \code{.data} and \code{.env} pronouns
#'  from the 'rlang' package, will work without any prefixes required.
#'
#'
#' @seealso \link{tinycodet_help}
#'
#' @examplesIf all(c("dplyr", "powerjoin", "magrittr") %installed in% .libPaths())
#' all(c("dplyr", "powerjoin", "magrittr") %installed in% .libPaths())
#'
#'
#' \donttest{
#'
#' # NO packages are being attached in any of the following code
#'
#' # load 'dplyr' + its re-exports + extension 'powerjoin', under alias "dpr.":
#' import_as(
#'   ~ dpr., "dplyr", re_exports = TRUE, extensions = "powerjoin"
#' )
#'
#' # exposing infix operators from 'magrrittr' to current environment:
#' import_inops("magrittr")
#'
#' # directly assigning dplyr's "starwars" dataset to object "d":
#' d <- import_data("dplyr", "starwars")
#'
#' # See it in Action:
#' d %>% dpr.$filter(species == "Droid") %>%
#'   dpr.$select(name, dpr.$ends_with("color"))
#'
#' male_penguins <- dpr.$tribble(
#'   ~name,    ~species,     ~island, ~flipper_length_mm, ~body_mass_g,
#'   "Giordan",    "Gentoo",    "Biscoe",               222L,        5250L,
#'   "Lynden",    "Adelie", "Torgersen",               190L,        3900L,
#'   "Reiner",    "Adelie",     "Dream",               185L,        3650L
#' )
#'
#' female_penguins <- dpr.$tribble(
#'   ~name,    ~species,  ~island, ~flipper_length_mm, ~body_mass_g,
#'   "Alonda",    "Gentoo", "Biscoe",               211,        4500L,
#'   "Ola",    "Adelie",  "Dream",               190,        3600L,
#'   "Mishayla",    "Gentoo", "Biscoe",               215,        4750L,
#' )
#' dpr.$check_specs()
#'
#' dpr.$power_inner_join(
#'   male_penguins[c("species", "island")],
#'   female_penguins[c("species", "island")]
#' )
#'
#' mypaste <- function(x, y) {
#'   import_LL("stringi", selection = "stri_c")
#'   stringi::stri_c(x, y)
#' }
#' mypaste("hello ", "world")
#'
#' }
#'


#' @rdname aaa2_tinycodet_import
#' @name aaa2_tinycodet_import
#' @aliases tinycodet_import
NULL
