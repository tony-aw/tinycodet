#' Load main package + its re-exports + its dependencies + its extensions under one alias
#'
#' @description
#'
#' The \code{import_as()} function
#' imports the namespace of an R package,
#' and optionally also its re-exports, dependencies, and extensions,
#' all under the same alias.
#' The specified alias will be placed in the current environment
#' (like the global environment, or the environment within a function). \cr
#'
#' @param alias a syntactically valid non-hidden name giving the alias object
#' where the package(s) are to be loaded into. \cr
#' This name can be given either as a single string (i.e. \code{"alias."}),
#' or as a one-sided formula with a single term (i.e. \code{~ alias.}). \cr
#' NOTE: To keep aliases easily distinguishable from other objects
#' that can also be subset with the \code{$} operator,
#' I recommend ending (not starting!) all alias names
#' with a dot (\code{.}) or underscore (\code{_}).
#' @param main_package a single string,
#' giving the name of the main package to load under the given alias.
#' @param re_exports logical;
#' Some R packages export functions that are not defined in their own package,
#' but in their direct dependencies; "re-exports", if you will. \cr
#' This argument determines what the \code{import_as} function
#' will do with the re-exports of the \code{main_package}: \cr
#'  * If \code{TRUE} the re-exports from the \code{main_package}
#'  are added to the alias,
#'  even if \code{dependencies = NULL}.
#' This is the default, as it is analogous to the behaviour of base R's \link{::} operator. \cr
#'  * If \code{FALSE}, these re-exports are not added,
#' and the user must specify the appropriate packages in argument \code{dependencies}.
#' @param dependencies an optional character vector,
#' giving the names of the dependencies of the
#' \code{main_package} to be loaded also under the alias. \cr
#' Defaults to \code{NULL}, which means no dependencies are loaded. \cr
#' See \link{pkg_get_deps} to quickly get dependencies from a package.
#' @param extensions an optional character vector,
#' giving the names of the extensions of the
#' \code{main_package} to be loaded also under the alias. \cr
#' Defaults to \code{NULL}, which means no extensions are loaded.
#' @param loadorder the character vector \cr
#' \code{c("dependencies", "main_package", "extensions")}, \cr
#' or some re-ordering of this character vector,
#' giving the relative load order of the groups of packages. \cr
#' The default setting (which is highly recommended) is the character vector \cr
#' \code{c("dependencies", "main_package", "extensions")}, \cr
#' which results in the following load order: \cr
#'  1) The dependencies, \bold{in the order specified by the \code{depenencies} argument}. \cr
#'  2) The main_package (see argument \code{main_package}),
#' including re-exports (if \code{re_exports=TRUE}). \cr
#'  3) The extensions, \bold{in the order specified by the \code{extensions} argument}.
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through). \cr
#' The \code{lib.loc} argument would usually be \code{.libPaths()}. \cr
#' See also \link[base]{loadNamespace}.
#'
#'
#' @details
#' \bold{On the \code{dependencies} and \code{extensions} arguments} \cr
#'
#'  * \code{dependencies}: "Dependencies" here are defined as any package appearing in the
#' "Depends", "Imports", or "LinkingTo" fields of the Description file of the
#' \code{main_package}. So no recursive dependencies.
#'  * \code{extensions}: "Extensions" here are defined as reverse-depends or reverse-imports.
#' It does not matter if these are CRAN or non-CRAN packages.
#' However, the intended meaning of an extension is not merely being a reverse dependency,
#' but a package that actually extends the functionality of the \code{main_package}.
#'
#' As implied in the description of the \code{loadorder} argument,
#' the order of the character vectors given in the
#' \code{dependencies}, and \code{extensions} arguments
#' matter: \cr
#' If multiple packages share objects with the same name,
#' the objects of the package named last will overwrite those of the earlier named packages. \cr
#' \cr
#' \cr
#' \bold{Additional details} \cr
#' The \code{import_as()} function
#' does not support loading base/core R under an alias. \cr
#' \cr
#'
#'
#' @returns
#' A locked environment object, similar to the output of \link[base]{loadNamespace},
#' with the name as specified in the \code{alias} argument,
#' will be created in the current environment
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
#' @examples
#' \dontrun{
#' import_as( # this creates the 'tdt.' object
#'   "tdt.", "tidytable", dependencies = "data.table"
#' )
#' # same as:
#' import_as(
#'   ~ tdt., "tidytable", dependencies = "data.table"
#' )
#' # using a function from the alias:
#' tdt.$mutate
#' }
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
  loadorder <- tolower(loadorder)
  check_loadorder <- all(
    sort(loadorder) ==sort(c("dependencies", "main_package", "extensions"))
  )
  if(!isTRUE(check_loadorder)) {
    stop("Improper load order given")
  }

  # Check dependencies:
  dependencies <- .internal_check_dependencies(main_package, dependencies, lib.loc, abortcall=sys.call())

  # Check extensions:
  extensions <- .internal_check_extends(main_package, extensions, lib.loc, abortcall=sys.call())

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
    "You can now access the functions using `", alias, "$`", "\n",
    "Methods will work like normally \n",
    "For conflicts report, packages order, and other attributes, run `attr.import(", alias, ")` \n"
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
    isTRUE(all(
      sort(args$loadorder) == sort(c("dependencies", "main_package", "extensions"))
    ))
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
