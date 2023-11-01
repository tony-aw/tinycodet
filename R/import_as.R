#' Load R-package and its Re-exports and/or its (Reverse) Dependencies Under a Single Alias
#'
#' @description
#'
#' The \code{import_as()} function
#' imports the namespace of an R-package,
#' and optionally also its re-exports, dependencies, and extensions,
#' all under the same alias.
#' The specified alias will be placed in the current environment
#' (like the global environment, or the environment within a function). \cr
#'
#' @param alias a syntactically valid non-hidden name giving the alias object
#' where the package(s) are to be loaded into. \cr
#' This name can be given either as a single string (i.e. \code{"alias."}),
#' or as a one-sided formula with a single term (i.e. \code{~ alias.}).
#' @param main_package a single string,
#' giving the name of the main package to load under the given alias.
#' @param re_exports \code{TRUE} or \code{FALSE}.
#'  * If \code{re_exports = TRUE} the re-exports from the \code{main_package}
#'  are added to the alias together with the main package.
#'  This is the default, as it is analogous to the behaviour of base R's \link{::} operator.
#'  * If \code{re_exports = FALSE}, these re-exports are not added together with the main package.
#'  The user can still load the packages from which the re-exported functions came from,
#'  by specifying them in the \code{dependencies} argument.
#' @param dependencies an optional character vector,
#' giving the names of the dependencies of the
#' \code{main_package} to be loaded also under the alias. \cr
#' Defaults to \code{NULL}, which means no dependencies are loaded. \cr
#' See \link{pkg_get_deps} to quickly get dependencies from a package. \cr
#' @param extensions an optional character vector,
#' giving the names of the extensions of the
#' \code{main_package} to be loaded also under the alias. \cr
#' Defaults to \code{NULL}, which means no extensions are loaded. \cr
#' @param loadorder the character vector \cr
#' \code{c("dependencies", "main_package", "extensions")}, \cr
#' or some re-ordering of this character vector,
#' giving the relative load order of the groups of packages. \cr
#' See Details section for more information. \cr
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through). \cr
#' The \code{lib.loc} argument would usually be \code{.libPaths()}. \cr
#' See also \link[base]{loadNamespace}.
#'
#'
#' @details
#'
#' \bold{Expanded Definitions of Some Arguments} \cr
#'
#'  * "Re-exports" are functions that are defined in the dependencies of the
#'  \code{main_package}, but are re-exported in the namespace of the \code{main_package}.
#'  * "Dependencies" are here defined as any R-package appearing in the
#'  "Depends", "Imports", or "LinkingTo" fields of the Description file of the
#'  \code{main_package}. So no recursive dependencies.
#'  * "Extensions" are here defined as
#'  direct reverse-depends or direct reverse-imports.
#'  It does not matter if these are CRAN or non-CRAN packages.
#'  However, the intended meaning of an extension is not merely a reverse dependency,
#'  but a package that actually extends the functionality of the
#'  \code{main_package}. \cr \cr
#'
#'
#' \bold{Why Aliasing Multiple Packages is Useful} \cr
#' To use an R-package with its extension packages or dependencies,
#' whilst avoiding the disadvantages of attaching a package (see \link{tinycodet_import}),
#' one would traditionally use the \link[base]{::} operator like so: \cr
#'
#' ```{r eval = FALSE}
#' main_package::some_function1()
#' extension1::some_function2()
#' extension2::some_function3()
#' ```
#'
#' This becomes cumbersome as more packages are needed and/or
#' as the package name(s) become longer. \cr
#' The \code{import_as()} function avoids this issue
#' by allowing multiple \bold{related} packages to be loaded under a single alias,
#' allowing one to code like this:
#'
#' ```{r eval = FALSE}
#' import_as(
#'    ~ alias., "main_package",
#'    extensions = c("extension1", "extension2"),
#'    lib.loc = .libPaths()
#' )
#' alias.$some_function1()
#' alias.$some_function2()
#' alias.$some_function3()
#' ```
#'
#' Thus loading a package, or multiple directly related packages, under a single alias,
#' which \code{import_as()} provides, avoids the above issues.
#' Loading (a) package(s) under an alias is known as "aliasing" (a) package(s). \cr
#' \cr
#' Notice that the \code{import_as()} function has the \code{lib.loc} argument,
#' allowing to specify the library path,
#' which the \link[base]{::} operator does not directly provide. \cr
#' \cr
#' \cr
#' \bold{Alias Naming Recommendation} \cr
#' To keep package alias object names easily distinguishable from other objects
#' that can also be subset with the \link[base]{$} operator,
#' I recommend ending (not starting!) all alias names
#' with a dot (\code{.}) or underscore (\code{_}). \cr
#' \cr
#' \cr
#' \bold{Regarding the Load Order} \cr
#' The order of the character vector given in
#' the \code{dependencies} and \code{extensions} arguments matters.
#' If multiple packages share objects with the same name,
#' the objects of the package named last will overwrite those of the earlier named packages. \cr
#' \cr
#' The \code{loadorder} argument defaults to the character vector \cr
#' \code{c("dependencies", "main_package", "extensions")}, \cr
#' which is the recommended setting. \cr
#' This setting results in the following load order: \cr
#'  1) The dependencies, \bold{in the order specified by the \code{depenencies} argument}.
#'  2) The main_package (see argument \code{main_package}),
#' including re-exports (if \code{re_exports = TRUE}).
#'  3) The extensions, \bold{in the order specified by the \code{extensions} argument}. \cr \cr
#'
#'
#'
#'
#' \bold{Other Details} \cr
#' The \code{import_as()} function
#' does not support loading base/core R under an alias. \cr
#' \cr
#'
#'
#' @returns
#' A locked environment object, similar to the output of \link[base]{loadNamespace},
#' with the name as specified in the \code{alias} argument,
#' will be created. \cr
#' This object, referred to as the "(package) alias object",
#' will contain the exported functions from the specified package(s). \cr
#' The alias object will be placed in the current environment
#' (like the global environment, or the environment within a function). \cr
#' \cr
#' To use, for example, function "some_function()" from alias "alias.", use: \cr
#' \code{alias.$some_function()} \cr
#' To see the special attributes of this alias object, use \link{attr.import}. \cr
#' To "unload" the package alias object, simply remove it (i.e. \code{rm(list="alias.")}). \cr
#'
#' @seealso [tinycodet_import()]
#'
#'
#' @examplesIf all(c("data.table", "tidytable") %installed in% .libPaths())
#'
#' import_as( # this creates the 'tdt.' object
#'   "tdt.", "data.table", extensions = "tidytable"
#' )
#' # same as:
#' import_as(
#'   ~ tdt., "data.table", extensions = "tidytable"
#' )
#'
#'
#'

#' @rdname import_as
#' @export
import_as <- function(
    alias, main_package, re_exports=TRUE,
    dependencies=NULL, extensions=NULL,
    lib.loc=.libPaths(),
    loadorder = c("dependencies", "main_package", "extensions")
) {

  # Check alias:
  alias_is_formula <- inherits(alias, "formula") && is.call(alias) && alias[[1]] == "~"
  if(!is.character(alias) & !alias_is_formula) {
    stop("`alias` needs to be either a string or a formula")
  }
  if(alias_is_formula) {
    if(length(all.vars(alias)) != 1) {
      stop("when `alias` is a formula, it must have 1 term")
    }
    alias <- all.vars(alias)[1]
  }
  if(is.character(alias)) {
    if(length(alias) != 1) {
      stop("when `alias` is a character, it must be a single string")
    }
  }
  check_proper_alias <- c(
    make.names(alias) == alias,
    length(alias) == 1,
    isTRUE(nchar(alias) > 0),
    isFALSE(alias %in% c("T", "F")),
    !startsWith(alias, ".")
  )
  if(!isTRUE(all(check_proper_alias))){
    stop("Syntactically invalid name for object `alias`")
  }

  # check library:
  .internal_check_lib.loc(lib.loc, sys.call())

  # check main_package:
  if(length(main_package) != 1){
    stop("A single package must be given in the `main_package` argument")
  }
  .internal_check_pkgs(pkgs=main_package, lib.loc=lib.loc, abortcall=sys.call())

  # check re-exports:
  if(!isTRUE(re_exports) & !isFALSE(re_exports)) {
    stop("`re_exports` must be either `TRUE` or `FALSE`")
  }

  # check load order:
  if(!.loadorder_is_correct(loadorder)) {
    stop("Improper load order given")
  }

  # Check dependencies:
  dependencies <- .internal_check_dependencies(main_package, dependencies, lib.loc, abortcall=sys.call())

  # Check extensions:
  extensions <- .internal_check_extends(main_package, extensions, lib.loc, abortcall=sys.call())

  # check dependencies + extensions combo:
  if(length(intersect(dependencies, extensions)) > 0) {
    stop("packages cannot be both dependencies and extensions!")
  }

  # make packages:
  pkgs <- list(
    dependencies=dependencies, main_package=main_package, extensions=extensions
  )
  pkgs <- pkgs[loadorder]
  pkgs <- do.call(c, pkgs)
  pkgs <- unique(pkgs)

  if(length(pkgs)>10) {
    message("Be careful: More than 10 packages are being loaded under the same alias")
  }

  # load packages:
  export_names_all <- character()
  export_names_allconflicts <- character()
  conflicts_df <- data.frame(
    package = character(length(pkgs)),
    winning_conflicts = character(length(pkgs))
  )
  versions_df <- data.frame(
    package = character(length(pkgs)),
    version = character(length(pkgs))
  )
  namespaces <- list()

  message("Importing packages...")

  for (i in 1:length(pkgs)) {

    namespace_current <- .internal_prep_Namespace(pkgs[i], lib.loc, abortcall = sys.call())
    conflicts_df$package[i] <- versions_df$package[i] <- pkgs[i]
    versions_df$version[i] <- getNamespaceVersion(loadNamespace(pkgs[i], lib.loc = lib.loc))

    if(pkgs[i]==main_package & isTRUE(re_exports)) {
      foreignexports <- .internal_get_foreignexports_ns(main_package, lib.loc, abortcall=sys.call())

      namespace_current <- utils::modifyList(
        namespace_current,
        foreignexports
      )
      conflicts_df$package[i] <- paste0(pkgs[i], " + re-exports")

    }

    export_names_current <- names(namespace_current)

    export_names_intersection <- intersect(export_names_current, export_names_all)
    if(length(export_names_intersection)>0) {
      conflicts_df$winning_conflicts[i] <- paste0(export_names_intersection, collapse = ", ")
    }
    export_names_allconflicts <- c(export_names_allconflicts, export_names_intersection)
    export_names_all <- c(export_names_all, export_names_current)
    namespaces <- utils::modifyList(namespaces, namespace_current)
  }

  # make attributes:
  ordered_object_names <- names(namespaces)
  out <- as.environment(namespaces)
  class(out) <- c(class(out), "tinyimport")
  args <- list(
    main_package = main_package, re_exports = re_exports,
    dependencies = dependencies, extensions = extensions,
    lib.loc = lib.loc, loadorder = loadorder
  )
  if(isTRUE(re_exports)){
    re_exports.pkgs <- lapply(foreignexports, \(x)attr(x, "package")) |>
      unlist() |> unname() |> unique()
  }
  if(isFALSE(re_exports)) {
    re_exports.pkgs <- NA
  }
  pkgs <- list(packages_order = pkgs,
               main_package = main_package,
               re_exports.pkgs = re_exports.pkgs)
  out$.__attributes__. <- list(
    pkgs = pkgs,
    conflicts = .format_conflicts_df(conflicts_df),
    versions = versions_df,
    args = args,
    ordered_object_names = ordered_object_names,
    tinyimport = "tinyimport"
  )

  # lock environment (JUST LIKE LOADNAMESPACE)
  lockEnvironment(out, bindings = TRUE)
  assign(alias, out, envir = parent.frame(n = 1))

  message(paste0(
    "Done", "\n",
    "You can now access the functions using `", alias, "$", "`", "\n",
    "Methods will work like normally \n",
    "For conflicts report, packages order, and other attributes, run `", "attr.import(", alias, ")", "` \n"
  ))

}

#' @keywords internal
#' @noRd
.is.tinyalias <- function(alias_chr, env) {
  if(!is.character(alias_chr) | length(alias_chr)!=1) {
    stop("`alias_chr` must be a single string")
  }
  if(!exists(alias_chr, envir = env, inherits = FALSE)) {
    return(FALSE)
  }
  obj <- get(as.character(alias_chr), envir = env)
  checks <- c(
    isTRUE(is.environment(obj)),
    isTRUE(all(class(obj) %in% c("environment", "tinyimport")))
  )
  if(any(!checks)) {
    return(FALSE)
  }
  args <- obj$.__attributes__.$args
  check <- isTRUE(is.list(args) & length(args) == 6)
  if(!check) {
    return(FALSE)
  }
  check_args <- isTRUE(all(names(args) %in% c("main_package", "re_exports",
                                              "dependencies", "extensions",
                                              "lib.loc", "loadorder")))
  if(!check_args){
    return(FALSE)
  }
  check_args <- c(
    isTRUE(is.character(args$main_package)) & isTRUE(length(args$main_package)==1),
    isTRUE(args$re_exports) | isFALSE(args$re_exports),
    isTRUE(is.character(args$dependencies) | is.null(args$dependencies)),
    isTRUE(is.character(args$extensions) | is.null(args$extensions)),
    isTRUE(.loadorder_is_correct(args$loadorder))
  )
  if(any(!check_args)) {
    return(FALSE)
  }
  pkgs <- list(
    dependencies=args$dependencies, main_package=args$main_package, extensions=args$extensions
  )
  pkgs <- pkgs[args$loadorder]
  pkgs <- do.call(c, pkgs)
  pkgs <- unique(pkgs)
  check_pkgs <- isTRUE(all(pkgs == obj$.__attributes__.$pkgs$packages_order))
  if(!check_pkgs) {
    return(FALSE)
  }

  return(TRUE)
}

#' @keywords internal
#' @noRd
.format_conflicts_df <- function(conflicts_df) {
  if(nrow(conflicts_df)<=2) {
    return(conflicts_df)
  }
  if(nrow(conflicts_df)>2) {
    n <- nrow(conflicts_df)-1
    ind <- 2:n
    for(i in ind) {
      current_overwrites <- stringi::stri_split(
        conflicts_df$winning_conflicts[i], fixed = ", ", simplify=TRUE
      ) |> as.vector()
      next_overwrites <- stringi::stri_split(
        conflicts_df$winning_conflicts[i:nrow(conflicts_df)], fixed = ", ", simplify = TRUE
      ) |> as.vector()
      if(isTRUE(any(current_overwrites %in% next_overwrites))) {
        conflicts_df$winning_conflicts[i] <- paste0(
          setdiff(current_overwrites, next_overwrites), collapse = ", "
        )
      }
    }
    return(conflicts_df)
  }
}

#' @keywords internal
#' @noRd
.loadorder_is_correct <- function(loadorder) {
  if(!is.character(loadorder)) {
    return(FALSE)
  }
  loadorder <- tolower(loadorder)
  if(length(loadorder) != 3) {
    return(FALSE)
  }
  if(anyDuplicated(loadorder)) {
    return(FALSE)
  }
  check_loadorder <- all(
    sort(loadorder) ==sort(c("dependencies", "main_package", "extensions"))
  )
  if(!isTRUE(check_loadorder)) {
    return(FALSE)
  }

  return(TRUE)

}
