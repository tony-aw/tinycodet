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
#' (6) Minimise typing - especially for replacement or infix operators \cr
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
#'  * \link{import_from}: \cr
#' Import specific objects from a package into the current or specific environment.
#'  * \link{import_as}: \cr
#' Import a main package,
#' and optionally its direct minimal dependencies,
#' under a single alias. \cr
#' This essentially combines the attaching advantage of using multiple related packages (item 7 on the list),
#' whilst keeping most advantages of using without attaching a package.
#'  * \link{import_ls}: \cr
#' List names of exported objects by category
#' (like "infix operators", or "replacement operators", etc.). \cr
#' Can be used in combination with, for example,
#' \link[base]{library} or \link{import_from},
#' to attach or expose all infix- and replacement operators at once. \cr
#' This gives the advantage of less typing (item 6 on the above list).
#'  * \link{import_data}: \cr
#' Directly return a data set from a package,
#' to allow straight-forward assignment.
#' * \link{import_diagnose}: \cr
#' Check for mismatch issues
#' (i.e. version or `lib.loc` mismatch)
#' between loaded packages and installed packages. \cr \cr
#'
#' The import system also includes general helper functions:
#' 
#'  * \link[=help.import]{help.import}: \cr
#'  Get help file for imported objects.
#'  * The \link[=pkg_get_deps]{pkg} - functions: \cr
#'  General helper functions regarding packages.
#'  * The \link[=searchenv_add]{searchenv} - functions: \cr
#'  safe functions for safely adding, removing, and accessing custom search path environments. \cr \cr
#' 
#' 
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
#' See examples below. \cr
#' \cr
#' There is no necessity for using the 'tinycodet' import system with every single package.
#' One can safely attach the 'stringi' package, for example,
#' as 'stringi' uses a unique and immediately recognisable naming scheme
#' (virtually all 'stringi' functions start with "\code{stri_}"),
#' and this naming scheme does not conflict with core R, nor with most other packages. \cr
#' \cr
#' Of course, if one wishes to use a package (like
#' 'stringi') \bold{only} within a specific environment,
#' it becomes advantageous to still import the package using the 'tinycodet' import system. \cr \cr
#' 
#' \bold{Some Additional Comments on the 'tinycodet' Import System} \cr
#'
#'  * Methods (like S3, S4) will automatically be registered.
#'  * Pronouns, such as the \code{.data} and \code{.env} pronouns
#'  from the 'rlang' package, will work without any prefixes required. 
#'  * 'tinycodet' avoids the \link[base]{exists} function, to prevent memory leakage. \cr \cr
#'
#'
#' @section For R Package Developers: 
#' It goes without saying,
#' just like one should NEVER use `library()` or `require()` inside an R-package,
#' similarly,
#' one should NOT use tinycodet’s import functions inside an R-Package. \cr
#' The import functions can still be used inside functions defined in a file to be sourced,
#' though. \cr
#' Just not in functions inside an R-package. \cr \cr
#'
#'
#' @seealso \link{tinycodet_help}
#'
#' @examplesIf all(c("dplyr", "tibble", "powerjoin", "magrittr") %installed in% .libPaths())
#' all(c("dplyr", "tibble", "powerjoin", "magrittr") %installed in% .libPaths())
#'
#' \donttest{
#'
#' # import dplyr, tibble, and powerjoin, under aliases:
#' import_as(.dpr ~ dplyr, deps = "tibble")
#' import_as(.pj ~ powerjoin)
#'
#' # attaching only the infix operators from 'magrrittr':
#' library(magrittr, include.only = import_ls("magrittr", "infix") )
#'
#' # directly assigning dplyr's "starwars" dataset to object "d":
#' d <- import_data("dplyr", "starwars")
#'
#' # See it in Action:
#' d %>% .dpr$filter(species == "Droid") %>%
#'   .dpr$select(name, .dpr$ends_with("color"))
#'
#' male_penguins <- .dpr$tribble(
#'   ~name,    ~species,     ~island, ~flipper_length_mm, ~body_mass_g,
#'   "Giordan",    "Gentoo",    "Biscoe",               222L,        5250L,
#'   "Lynden",    "Adelie", "Torgersen",               190L,        3900L,
#'   "Reiner",    "Adelie",     "Dream",               185L,        3650L
#' )
#'
#' female_penguins <- .dpr$tribble(
#'   ~name,    ~species,  ~island, ~flipper_length_mm, ~body_mass_g,
#'   "Alonda",    "Gentoo", "Biscoe",               211,        4500L,
#'   "Ola",    "Adelie",  "Dream",               190,        3600L,
#'   "Mishayla",    "Gentoo", "Biscoe",               215,        4750L,
#' )
#' .pj$check_specs()
#'
#' .pj$power_inner_join(
#'   male_penguins[c("species", "island")],
#'   female_penguins[c("species", "island")]
#' )
#'
#' mypaste <- function(x, y) {
#'   import_from("stringi", "stri_c")
#'   stri_c(x, y)
#' }
#' mypaste("hello ", "world")
#'
#' }
#'


#' @rdname aaa2_tinycodet_import
#' @name aaa2_tinycodet_import
#' @aliases tinycodet_import
NULL
