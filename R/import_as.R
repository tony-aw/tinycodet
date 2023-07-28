#' Load main package + its foreign exports + its dependencies + its enhances + its extensions under one alias
#'
#' @description
#'
#' The \code{import_as()} function
#' imports the namespace of an R package,
#' and optionally also its dependencies, enhances, and extensions,
#' under the same alias.
#' The specified alias will be placed in the current environment
#' (like the global environment, or the environment within a function). \cr
#'
#' @param alias a syntactically valid non-hidden object name (unquoted),
#' giving the alias object
#' where the package(s) are to be loaded into. \cr
#' NOTE: To keep aliases easily distinguishable from other objects
#' that can also be subset with the \code{$} operator,
#' I recommend ending (not starting!) the names of all alias names
#' with a dot (\code{.}) or underscore (\code{_}).
#' @param main_package a single string,
#' giving the name of the main package to load under the given alias.
#' @param foreign_exports logical;
#' some R packages export functions that are not defined in their own package,
#' but in their direct dependencies; "foreign exports", if you will. \cr
#' This argument determines what the \code{import_as} function
#' will do with the foreign exports of the \code{main_package}: \cr
#'  * If \code{TRUE} the foreign exports from the \code{main_package}
#'  are added to the alias,
#'  even if \code{dependencies = NULL}.
#' This is the default, as it is analogous to the behaviour of base R's \link{::} operator. \cr
#'  * If \code{FALSE}, these foreign exports are not added,
#' and the user must specify the appropriate packages in argument \code{dependencies}.
#' @param dependencies an optional character vector,
#' giving the names of the dependencies of the
#' \code{main_package} to be loaded also under the alias. \cr
#' Defaults to \code{NULL}, which means no dependencies are loaded. \cr
#' See \link{pkg_get_deps} to quickly get dependencies from a package.
#' @param enhances an optional character vector,
#' giving the names of the packages enhanced by the
#' \code{main_package} to be loaded also under the alias. \cr
#' Defaults to \code{NULL}, which means no enhances are loaded.
#' @param extensions an optional character vector,
#' giving the names of the extensions of the
#' \code{main_package} to be loaded also under the alias. \cr
#' Defaults to \code{NULL}, which means no extensions are loaded.
#' @param verbose logical,
#' indicating whether messages regarding conflicts and foreign exports
#' should be printed while importing packages (\code{TRUE}),
#' or if these should not be printed (\code{FALSE}). \cr
#' Defaults to \code{FALSE},
#' because all information conveyed by the messages can be more compactly be viewed
#' by running
#' \code{alias.$.__attributes__.}
#' (where "alias." is the given name of the alias object).
#' @param loadorder the character vector \cr
#' \code{c("dependencies", "main_package", "enhances", "extensions")}, \cr
#' or some re-ordering of this character vector,
#' giving the relative load order of the groups of packages. \cr
#' The default setting (which is highly recommended) is the character vector \cr
#' \code{c("dependencies", "main_package", "enhances", "extensions")}, \cr
#' which results in the following load order: \cr
#'  1) The dependencies, \bold{in the order specified by the \code{depenencies} argument}. \cr
#'  2) The main_package (see argument \code{main_package}),
#' including foreign exports (if \code{foreign_exports=TRUE}). \cr
#'  3) The enhances, \bold{in the order specified by the \code{enhances} argument}. \cr
#'  4) The extensions, \bold{in the order specified by the \code{extensions} argument}.
#' @param lib.loc character vector specifying library search path
#' (the location of R library trees to search through). \cr
#' This is usually \code{.libPaths()}. \cr
#' See also \link[base]{loadNamespace}.
#'
#'
#' @details
#' \bold{On the \code{dependencies}, \code{enhances} and \code{extensions} arguments} \cr
#'
#'  * \code{dependencies}: "Dependencies" here are defined as any package appearing in the
#' "Depends", "Imports", or "LinkingTo" fields of the Description file of the
#' \code{main_package}. So no recursive dependencies.
#'  * \code{enhances}: Enhances are defined as packages appearing in the "Enhances" field
#' of the Description file of the \code{main_package}. \cr
#'  * \code{extensions}: "Extensions" here are defined as reverse-depends or reverse-imports.
#' It does not matter if these are CRAN or non-CRAN packages.
#' However, the intended meaning of an extension is not merely being a reverse dependency,
#' but a package that actually extends the functionality of the \code{main_package}.
#'
#' As implied in the description of the \code{loadorder} argument,
#' the order of the character vectors given in the
#' \code{dependencies}, \code{enhances}, and \code{extensions} arguments
#' matter: \cr
#' If multiple packages share objects with the same name,
#' the objects of the package named last will overwrite those of the earlier named packages. \cr
#' \cr
#' \cr
#' \bold{Additional details} \cr
#' The \code{import_as()} function
#' does not allow importing base/core R under an alias,
#' so don't try. \cr
#' \cr
#'
#'
#' @returns
#' A locked environment object, similar to the output of \link[base]{loadNamespace}, \cr
#' with the name as specified in the \code{alias} argument,
#' will be created in the current environment
#' (like the global environment, or the environment within a function). \cr
#' \cr
#' To use, for example, function "some_function()" from alias "alias.", use: \cr
#' \code{alias.$some_function()} \cr
#' To see attributes of this alias object, use: \cr
#' \code{alias.$.__attributes__.}
#' \cr
#'
#' @seealso [tinyoperations_import()]
#'
#'
#' @examples
#' \dontrun{
#' import_as( # this creates the 'tdt.' object
#'   tdt., "tidytable", dependencies = "data.table"
#' )
#' tdt.$mutate
#' }
#'
#'

#' @rdname import_as
#' @export
import_as <- function(
    alias, main_package, foreign_exports=TRUE,
    dependencies=NULL, enhances=NULL, extensions=NULL,
    lib.loc=.libPaths(), verbose=FALSE,
    loadorder = c("dependencies", "main_package", "enhances", "extensions")
) {

  # Check alias:
  alias_chr <- as.character(substitute(alias))
  check_proper_alias <- c(
    make.names(alias_chr)==alias_chr,
    length(alias_chr)==1,
    isTRUE(nchar(alias_chr)>0),
    isFALSE(alias_chr %in% c("T", "F")),
    !startsWith(alias_chr, ".")
  )
  if(!isTRUE(all(check_proper_alias))){
    stop("Syntactically invalid name for object `alias`")
  }

  # check library:
  if(length(lib.loc)<1 | !is.character(lib.loc)) {
    stop("`lib.loc` must be a character vector with at least one library path")
  }

  # check main_package:
  if(length(main_package)>1){
    stop("Only a single package can be given in the `main_package` argument")
  }
  .internal_check_pkgs(pkgs=main_package, lib.loc=lib.loc, abortcall=sys.call())

  # check foreign exports:
  if(!isTRUE(foreign_exports) & !isFALSE(foreign_exports)) {
    stop("`foreign_exports` must be either `TRUE` or `FALSE`")
  }

  # check verbose:
  if(!isTRUE(verbose) & !isFALSE(verbose)) {
    stop("`verbose` must be either `TRUE` or `FALSE`")
  }

  # check load order:
  loadorder <- tolower(loadorder)
  check_loadorder <- all(
    sort(loadorder) ==sort(c("dependencies", "main_package", "enhances", "extensions"))
  )
  if(!isTRUE(check_loadorder)) {
    stop("Improper load order given")
  }

  # Check dependencies:
  dependencies <- .internal_check_dependencies(main_package, dependencies, lib.loc, abortcall=sys.call())

  # Check enhances:
  enhances <- .internal_check_enhances(main_package, enhances, lib.loc, abortcall=sys.call())

  # Check extensions:
  extensions <- .internal_check_extends(main_package, extensions, lib.loc, abortcall=sys.call())

  # make packages:
  pkgs <- list(
    dependencies=dependencies, main_package=main_package, enhances=enhances, extensions=extensions
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
  namespaces <- list()

  message("Importing packages...")

  for (i in 1:length(pkgs)) {
    conflicts_df$package[i] <- pkgs[i]

    namespace_current <- .internal_prep_Namespace(pkgs[i], lib.loc)

    if(pkgs[i]==main_package & isTRUE(foreign_exports)) {
      if(verbose){ message("listing foreign exports from package: ", pkgs[i], "...") }
      namespace_current <- utils::modifyList(
        namespace_current,
        .internal_get_foreignexports_ns(main_package, lib.loc, abortcall=sys.call())
      )
      conflicts_df$package[i] <- paste0(pkgs[i], " + foreign exports")

    }

    export_names_current <- names(namespace_current)

    export_names_intersection <- intersect(export_names_current, export_names_all)
    if(i==1){
      if(verbose){ message("Importing package: ", pkgs[i], "...") }
    }
    if(length(export_names_intersection)==0 & i>1) {
      if(verbose){ message("Importing package: ", pkgs[i], "... no conflicts") }
    }
    if(length(export_names_intersection)>0) {
      conflicts_df$winning_conflicts[i] <- paste0(export_names_intersection, collapse = ", ")
      if(verbose){ message(
        "Importing package: ", pkgs[i], "... The following conflicting objects detected:",
        "\n",
        paste0(export_names_intersection, collapse = ", "),
        "\n",
        pkgs[i], " will overwrite conflicting objects from previous imported packages..."
      ) }
    }
    export_names_allconflicts <- c(export_names_allconflicts, export_names_intersection)
    export_names_all <- c(export_names_all, export_names_current)
    namespaces <- utils::modifyList(namespaces, namespace_current)
    if(verbose){ message("") }
  }

  if(verbose){message("setting up special attributes...")}
  out <- as.environment(namespaces)
  class(out) <- c(class(out), "tinyimport")
  args <- list(
    main_package = main_package, foreign_exports = foreign_exports,
    dependencies = dependencies, enhances = enhances, extensions = extensions,
    loadorder = loadorder
  )
  out$.__attributes__. <- list(
    packages_order = pkgs,
    conflicts = .format_conflicts_df(conflicts_df),
    args = args,
    tinyimport = "tinyimport"
  )

  # lock environment (JUST LIKE LOADNAMESPACE)
  lockEnvironment(out, bindings = TRUE)
  assign(alias_chr, out, envir = parent.frame(n = 1))

  message(paste0(
  "Done", "\n",
  "You can now access the functions using `", alias_chr, "$`.", "\n",
  "(S3)methods will work like normally. \n",
  "For conflicts report and package order, run `", alias_chr, "$.__attributes__.`. \n"
))

}

#' @keywords internal
#' @noRd
.is.tinyalias <- function(alias_chr, env) {
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
  check_args <- isTRUE(all(names(args) == c("main_package", "foreign_exports",
                              "dependencies", "enhances", "extensions",
                              "loadorder")))
  if(!check_args){
    return(FALSE)
  }
  check_args <- c(
    isTRUE(is.character(args$main_package)) & isTRUE(length(args$main_package)==1),
    isTRUE(args$foreign_exports) | isFALSE(args$foreign_exports),
    isTRUE(is.character(args$dependencies) | is.null(args$dependencies)),
    isTRUE(is.character(args$enhances) | is.null(args$enhances)),
    isTRUE(is.character(args$extensions) | is.null(args$extensions)),
    isTRUE(all(
      sort(args$loadorder) == sort(c("dependencies", "main_package", "enhances", "extensions"))
    ))
  )
  if(any(!check_args)) {
    return(FALSE)
  }
  pkgs <- list(
    dependencies=args$dependencies, main_package=args$main_package, enhances=args$enhances, extensions=args$extensions
  )
  pkgs <- pkgs[args$loadorder]
  pkgs <- do.call(c, pkgs)
  pkgs <- unique(pkgs)
  check_pkgs <- isTRUE(all(pkgs == args$pkgs))
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
