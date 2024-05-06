#' Import R-package, its Re-exports, Dependencies, and/or Extensions, Under a Single Alias
#'
#' @description
#'
#' The \code{import_as()} function
#' imports the namespace of an R-package,
#' and optionally also its re-exports, dependencies, and extensions,
#' all under the same alias.
#' The specified alias,
#' containing the exported functions from the specified packages,
#' will be placed in the current environment
#' (like the global environment, or the environment within a function). \cr
#'
#' @param alias a syntactically valid non-hidden name giving the alias object
#' where the package(s) are to be imported into. \cr
#' This name can be given either as a single string (i.e. \code{"alias."}),
#' or as a one-sided formula with a single term (i.e. \code{~ alias.}).
#' @param main_package a single string,
#' giving the name of the main package to import under the given alias. \cr
#' Core R (i.e. "base", "stats", etc.) is not allowed.
#' @param re_exports \code{TRUE} or \code{FALSE}.
#'  * If \code{re_exports = TRUE} the re-exports from the \code{main_package}
#'  are added to the alias together with the main package.
#'  This is the default, as it is analogous to the behaviour of base R's \link{::} operator.
#'  * If \code{re_exports = FALSE}, these re-exports are not added together with the main package.
#'  The user can still import the packages under the alias from which the re-exported functions came from,
#'  by specifying them in the \code{dependencies} argument.
#' @param dependencies an optional character vector,
#' giving the names of the dependencies of the
#' \code{main_package} to be imported also under the alias. \cr
#' Defaults to \code{NULL}, which means no dependencies are imported under the alias. \cr
#' See \link{pkg_get_deps} to quickly get dependencies from a package. \cr
#' Core R (i.e. "base", "stats", etc.) is not allowed.
#' @param extensions an optional character vector,
#' giving the names of the extensions of the
#' \code{main_package} to be imported also under the alias. \cr
#' Defaults to \code{NULL}, which means no extensions are imported under the alias. \cr
#' Core R (i.e. "base", "stats", etc.) is not allowed.
#' @param import_order the character vector \cr
#' \code{c("dependencies", "main_package", "extensions")}, \cr
#' or some re-ordering of this character vector,
#' giving the relative import order of the groups of packages. \cr
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
#'  \code{main_package}, but are re-exported in the namespace of the \code{main_package}. \cr
#'  Unlike the `Dependencies` argument, functions from core R are included in re-exports.
#'  * "Dependencies" are here defined as any R-package appearing in the
#'  "Depends", "Imports", or "LinkingTo" fields of the Description file of the
#'  \code{main_package}. So no recursive dependencies.
#'  * "Extensions" are reverse-dependencies that actually extend the functionality of the
#'  \code{main_package}. \cr
#'  Programmatically, some package "E" is considered an extension of some
#'  "main_package",
#'  if the following is \code{TRUE}: \cr
#'  \code{"main_package" %in% } \link{pkg_get_deps_minimal}\code{("E")} \cr \cr
#'
#'
#' \bold{Why Aliasing Multiple Packages is Useful} \cr
#' To use an R-package with its extension packages or dependencies,
#' whilst avoiding the disadvantages of attaching a package (see \link{tinycodet_import}),
#' one would traditionally use the \link[base]{::} operator like so: \cr
#'
#' ```{r eval = FALSE}
#' main_package::some_function1()
#' dependency1::some_function2()
#' extension1::some_function3()
#' ```
#'
#' This becomes cumbersome as more packages are needed and/or
#' as the package name(s) become longer. \cr
#' The \code{import_as()} function avoids this issue
#' by allowing multiple \bold{related} packages to be imported under a single alias,
#' allowing one to code like this:
#'
#' ```{r eval = FALSE}
#' import_as(
#'    ~ alias., "main_package",
#'    dependencies = "dependency1", extensions = "extension1",
#'    lib.loc = .libPaths()
#' )
#' alias.$some_function1()
#' alias.$some_function2()
#' alias.$some_function3()
#' ```
#'
#' Thus importing a package, or multiple directly related packages, under a single alias,
#' which \code{import_as()} provides, avoids the above issues.
#' Importing a package under an alias is referred to as "aliasing" a package. \cr
#' \cr
#' \cr
#' \bold{Alias Naming Recommendation} \cr
#' To keep package alias object names easily distinguishable from other objects
#' that can also be subset with the \link[base]{$} operator,
#' I recommend ending (not starting!) all alias names
#' with a dot (\code{.}) or underscore (\code{_}). \cr
#' \cr
#' \cr
#' \bold{Regarding \code{import_order}} \cr
#' The order of the character vector given in
#' the \code{dependencies} and \code{extensions} arguments matters.
#' If multiple packages share objects with the same name,
#' the objects of the package named last will overwrite those of the earlier named packages. \cr
#' \cr
#' The \code{import_order} argument defaults to the character vector \cr
#' \code{c("dependencies", "main_package", "extensions")}, \cr
#' which is the recommended setting. \cr
#' This setting results in the following importing order: \cr
#'  1) The dependencies, \bold{in the order specified by the \code{depenencies} argument}.
#'  2) The main_package (see argument \code{main_package}),
#' including re-exports (if \code{re_exports = TRUE}).
#'  3) The extensions, \bold{in the order specified by the \code{extensions} argument}. \cr \cr
#'
#'
#'
#' \bold{Other Details} \cr
#' The \code{import_as()} function
#' does not support importing base/core R under an alias. \cr
#' Packages that appear in the "Suggests" or "Enhances" fields of packages
#' are not considered dependencies or extensions. \cr
#' No more than 10 packages are allowed to be imported under a single alias. \cr
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
#' To "unimport" the package alias object, simply remove it (i.e. \code{rm(list = "alias.")}). \cr
#'
#' @seealso \link{tinycodet_import}
#'
#'
#' @examplesIf all(c("data.table", "tidytable") %installed in% .libPaths())
#' all(c("data.table", "tidytable") %installed in% .libPaths())
#'
#' import_as( # this creates the 'tdt.' object
#'   "tdt.", "tidytable", dependencies = "data.table"
#' )
#' # same as:
#' import_as(
#'   ~ tdt., "tidytable", dependencies = "data.table"
#' )
#'
#'
#'

#' @rdname import_as
#' @export
import_as <- function(
    alias, main_package, re_exports = TRUE,
    dependencies = NULL, extensions = NULL,
    lib.loc = .libPaths(),
    import_order = c("dependencies", "main_package", "extensions")
) {
  
  # Check alias:
  alias_is_formula <- inherits(alias, "formula") && is.call(alias) && alias[[1]] == "~"
  if(!is.character(alias) && !alias_is_formula) {
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
  if(length(main_package) != 1 || !is.character(main_package)){
    stop("`main_package` must be a single string")
  }
  .internal_check_forbidden_pkgs(
    pkgs = main_package, lib.loc = lib.loc, abortcall = sys.call()
  )
  .internal_check_pkgs(
    pkgs = main_package, lib.loc = lib.loc, abortcall = sys.call()
  )
  
  
  # check re-exports:
  if(!isTRUE(re_exports) && !isFALSE(re_exports)) {
    stop("`re_exports` must be either `TRUE` or `FALSE`")
  }
  
  # check import order:
  if(!.import_order_is_correct(import_order)) {
    stop("Improper `import_order` given")
  }
  
  # check dependencies + extensions combo:
  if(length(intersect(dependencies, extensions)) > 0) {
    stop("packages cannot be both dependencies and extensions!")
  }
  if((length(dependencies) + length(extensions) + 1) > 10) {
    stop("more than 10 packages not allowed to be imported under a single alias")
  }
  
  # Check dependencies:
  if(!is.null(dependencies)) {
    if(!is.character(dependencies) || length(dependencies) == 0) { 
      stop("`dependencies` must be a character vector")
    }
    .internal_check_forbidden_pkgs(dependencies, lib.loc = lib.loc, abortcall = sys.call())
    .internal_check_dependencies(main_package, dependencies, lib.loc, abortcall = sys.call())
    
  }
  
  
  # Check extensions:
  if(!is.null(extensions)) {
    if(!is.character(extensions) || length(extensions) == 0) { 
      stop("`extensions` must be a character vector")
    }
    .internal_check_forbidden_pkgs(extensions, lib.loc = lib.loc, abortcall = sys.call())
    .internal_check_extends(main_package, extensions, lib.loc, abortcall=sys.call())
    
  }
  
  
  
  # list packages:
  pkgs <- list(
    dependencies=dependencies, main_package=main_package, extensions=extensions
  )
  pkgs <- pkgs[import_order]
  pkgs <- do.call(c, pkgs)
  pkgs <- unique(pkgs)
  
  
  # import packages:
  export_names_all <- character()
  export_names_allconflicts <- character()
  conflicts_df <- data.frame(
    package = character(length(pkgs)),
    winning_conflicts = character(length(pkgs))
  )
 
  namespaces <- list()
  
  message("Importing packages and registering methods...")
  
  for (i in seq_along(pkgs)) {
    
    namespace_current <- .internal_prep_Namespace(pkgs[i], lib.loc, abortcall = sys.call())
    conflicts_df$package[i] <- pkgs[i]
    
    
    if(pkgs[i] == main_package && isTRUE(re_exports)) {
      foreignexports <- .internal_get_foreignexports_ns(main_package, lib.loc, abortcall = sys.call())
      namespace_current <- utils::modifyList(
        namespace_current,
        foreignexports
      )
      
      conflicts_df$package[i] <- paste0(pkgs[i], " + re-exports")
      
    }
    
    export_names_current <- names(namespace_current)
    
    export_names_intersection <- intersect(export_names_current, export_names_all)
    
    if(length(export_names_intersection) > 0) {
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
    lib.loc = lib.loc, import_order = import_order
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
    "For conflicts report, packages order, and other attributes, run `", "attr.import(", alias, ")", "` \n"
  ))
  
}

#' @keywords internal
#' @noRd
.is.tinyalias <- function(alias_chr, env) {
  if(!is.character(alias_chr) || length(alias_chr)!=1) {
    stop("`alias_chr` must be a single string")
  }
  if(!exists(alias_chr, envir = env, inherits = FALSE)) {
    return(FALSE)
  }
  obj <- get(as.character(alias_chr), envir = env)
  checks <- c(
    is.environment(obj),
    all(c("environment", "tinyimport") %in% class(obj))
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
                                              "lib.loc", "import_order")))
  if(!check_args){
    return(FALSE)
  }
  check_args <- c(
    isTRUE(is.character(args$main_package)) & isTRUE(length(args$main_package) == 1),
    isTRUE(args$re_exports) | isFALSE(args$re_exports),
    isTRUE(is.character(args$dependencies) | is.null(args$dependencies)),
    isTRUE(is.character(args$extensions) | is.null(args$extensions)),
    isTRUE(.import_order_is_correct(args$import_order))
  )
  if(any(!check_args)) {
    return(FALSE)
  }
  pkgs <- obj$.__attributes__.$pkgs
  check_args <- c(
    args$main_package != pkgs$main_package,
    c(args$main_package) %in% .internal_list_coreR(),
    args$dependencies %in% .internal_list_coreR(),
    args$extensions %in% .internal_list_coreR()
  )
  if(any(check_args)) {
    return(FALSE)
  }
  
  pkgs <- list(
    dependencies=args$dependencies, main_package=args$main_package, extensions=args$extensions
  )
  pkgs <- pkgs[args$import_order]
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
.import_order_is_correct <- function(import_order) {
  if(!is.character(import_order)) {
    return(FALSE)
  }
  import_order <- tolower(import_order)
  if(length(import_order) != 3) {
    return(FALSE)
  }
  if(anyDuplicated(import_order)) {
    return(FALSE)
  }
  check_import_order <- all(
    sort(import_order) == sort(c("dependencies", "main_package", "extensions"))
  )
  if(!isTRUE(check_import_order)) {
    return(FALSE)
  }
  
  return(TRUE)
  
}


