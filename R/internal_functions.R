#' Internal functions
#'
#'
#'
#'

#' @keywords internal
#' @noRd
s_get_pattern_attr_internal <- function(p) {
  fxd <- ic <- prl <- ub <- FALSE
  if(!is.null(attr(p, "fixed"))){fxd <- attr(p, "fixed")}
  if(!is.null(attr(p, "ignore.case"))){ic <- attr(p, "ignore.case")}
  if(!is.null(attr(p, "perl"))){prl <- attr(p, "perl")}
  if(!is.null(attr(p, "useBytes"))) {ub <- attr(p, "useBytes")}
  return(list(fxd=fxd, ic=ic, prl=prl, ub=ub))
}

#' @keywords internal
#' @noRd
.internal_prep_Namespace <- function(package, lib.loc) {
  ns <- loadNamespace(package, lib.loc = lib.loc) |> as.list(all.names=TRUE, sorted=TRUE)
  names_exported <- names(ns[[".__NAMESPACE__."]][["exports"]])
  ns <- ns[names_exported]
  ns <- ns[!is.na(names(ns))]
  names_exported <- names(ns)
  names_functions <- names(ns)[sapply(ns, is.function)]
  if(length(names_functions)>0) {
    for (i in names_functions){
      if(isFALSE(is.null(ns[[i]]))){
      attr(ns[[i]], which = "package") <- package
      attr(ns[[i]], which = "funtion_name") <- i
      }
    }
  }
  return(ns)
}

#' @keywords internal
#' @noRd
.internal_check_deps_overlap_any <- function(
    pkgs, lib.loc, deps_type=c("Depends", "Imports", "LinkingTo")
) {
  all_deps <- sapply(
    pkgs, function(p)pkg_get_deps(p, lib.loc=lib.loc, deps_type=deps_type,
    base=FALSE, recom=FALSE, rstudioapi=FALSE)
  ) |> unlist() |> unname()
  check <- any(pkgs %in% all_deps)
  return(check)
}

#' @keywords internal
#' @noRd
.internal_get_foreignexports_ns <- function(main_package, lib.loc, abortcall) {
  ns <- loadNamespace(main_package, lib.loc = lib.loc) |> as.list(all.names=TRUE, sorted=TRUE)
  names_exports <- names(ns[[".__NAMESPACE__."]][["exports"]])
  lst_imports <- ns[[".__NAMESPACE__."]][["imports"]]
  pkgs_core <- c(
    utils::installed.packages(priority = "base") |> rownames(),
    utils::installed.packages(lib.loc=lib.loc, priority = "base") |> rownames()
  ) |> unique()

  lst_imports <- lst_imports[!names(lst_imports) %in% pkgs_core]
  pkgs <- names(lst_imports) |> unique()

  uninstalled_pkgs <- pkgs[!pkgs %installed in% lib.loc]
  if(length(uninstalled_pkgs)>0) {
    error.txt <- simpleError(paste0(
      "The following dependent packages (for the forein exports) are not installed:",
      "\n",
      paste0(uninstalled_pkgs, collapse = ", ")
    ), call = abortcall)
    stop(error.txt)
  }

  ns_foreign <- list()
  for (i in pkgs) {
    names_funs <- lst_imports[names(lst_imports) %in% i] |> unlist()
    names_funs <- intersect(names_exports, names_funs)
    ns_i <- .internal_prep_Namespace(i, lib.loc = lib.loc) |> as.environment()
    names_funs <- intersect(names_funs, names(ns_i))
    ns_temp <- mget(
      names_funs, envir = ns_i,
      inherits = FALSE
    )
    ns_foreign <- utils::modifyList(
      ns_foreign, ns_temp
    )
  }
  return(ns_foreign)
}

#' @keywords internal
#' @noRd
.internal_check_pkgs <- function(pkgs, lib.loc, pkgs_txt="packages", correct_pkgs=NULL, abortcall) {

  pkgs_core <- c(
    utils::installed.packages(priority = "base") |> rownames(),
    utils::installed.packages(lib.loc=lib.loc, priority = "base") |> rownames()
  ) |> unique()
  pkgs_preinst <- c(
    utils::installed.packages(priority = "recommended") |> rownames(),
    utils::installed.packages(lib.loc=lib.loc, priority = "recommended") |> rownames()
  ) |> unique()

  misspelled_pkgs <- pkgs[pkgs != make.names(pkgs)]
  if(isTRUE(length(misspelled_pkgs)>0)) {
    error.txt <- simpleError(paste0(
      "You have misspelled the following ", pkgs_txt, ":",
      "\n",
      paste0(misspelled_pkgs, collapse = ", ")
    ), call=abortcall)
    stop(error.txt)
  }

  duplicate_pkgs <- pkgs[duplicated(pkgs)]
  if(isTRUE(length(duplicate_pkgs)>0)) {
    error.txt <- simpleError(paste0(
      "The following duplicate ", pkgs_txt, " given:",
      "\n",
      paste0(duplicate_pkgs, collapse = ", ")
    ), call=abortcall)
    stop(error.txt)
  }

  uninstalled_pkgs <- pkgs[!pkgs %installed in% lib.loc]
  if(isTRUE(length(uninstalled_pkgs)>0)) {
    error.txt <- simpleError(paste0(
      "The following ", pkgs_txt, " are not installed:",
      "\n",
      paste0(uninstalled_pkgs, collapse = ", ")
    ), call=abortcall)
    stop(error.txt)
  }

  forbidden_pkgs <- pkgs[pkgs %in% pkgs_core]
  if(isTRUE(length(forbidden_pkgs)>0)) {
    error.txt <- simpleError(paste0(
      'The following "packages" are base/core R, which is not allowed:',
      "\n",
      paste0(forbidden_pkgs)
    ), call=abortcall)
    stop(error.txt)
  }

  if(!is.null(correct_pkgs)) {
    wrong_pkgs <- pkgs[!pkgs %in% correct_pkgs]
    if(length(wrong_pkgs)>0) {
      error.txt <- simpleError(paste0(
        "The following given ", pkgs_txt, " were not found to be actual ", pkgs_txt, ":",
        "\n",
        paste0(wrong_pkgs, collapse = ", ")
      ), call=abortcall)
      stop(error.txt)
    }
  }

}

#' @keywords internal
#' @noRd
.internal_get_pkg_deps <- function(
    package, lib.loc=.libPaths(), type,
    base=FALSE, recom=FALSE, rstudioapi=FALSE
) {
  # based of https://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran
  dcf <- read.dcf(file.path(system.file("DESCRIPTION", package = package, lib.loc = lib.loc)))
  jj <- intersect(type, colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names=FALSE)
  val <- gsub("\\s.*", "", trimws(val))
  depends <- val[val != "R"]
  if(!base){
    pkgs_core <- c(
      utils::installed.packages(priority = "base") |> rownames(),
      utils::installed.packages(lib.loc=lib.loc, priority = "base") |> rownames()
    ) |> unique()
    depends <- setdiff(depends, pkgs_core)
  }
  if(!recom) {
    pkgs_preinst <- c(
      utils::installed.packages(priority = "recommended") |> rownames(),
      utils::installed.packages(lib.loc=lib.loc, priority = "recommended") |> rownames()
    ) |> unique()
    depends <- setdiff(depends, pkgs_preinst)
  }
  if(!rstudioapi) {
    depends <- setdiff(depends, "rstudioapi")
  }
  return(depends)
}

#' @keywords internal
#' @noRd
.internal_check_dependencies <- function(package, dependencies, lib.loc, abortcall) {

  actual_dependencies <- pkg_get_deps(
    package, lib.loc=lib.loc, deps_type=c("Depends", "Imports", "LinkingTo"),
    base=TRUE, recom=TRUE, rstudioapi = TRUE
  ) |> unique()

  if(is.character(dependencies) & length(dependencies)>0) {
    .internal_check_pkgs(
      pkgs=dependencies, lib.loc=lib.loc, pkgs_txt = "dependencies",
      correct_pkgs=actual_dependencies, abortcall=abortcall
    )
  }

  return(dependencies)
}

#' @keywords internal
#' @noRd
.internal_check_enhances <- function(package, enhances, lib.loc, abortcall) {
  actual_enhances <- pkg_get_deps(
    package, lib.loc=lib.loc, deps_type=c("Enhances"),
    base=TRUE, recom=TRUE, rstudioapi = TRUE
  ) |> unique()

  if(is.character(enhances) & length(enhances)>0) {
    .internal_check_pkgs(
      pkgs=enhances, lib.loc=lib.loc, pkgs_txt = "enhances",
      correct_pkgs = actual_enhances, abortcall=abortcall
    )
  }
  return(enhances)
}

#' @keywords internal
#' @noRd
.internal_check_extends <- function(package, extends, lib.loc, abortcall) {

  if(!is.null(extends) & is.character(extends) & length(extends)>0) {
    .internal_check_pkgs(
      pkgs=extends, lib.loc=lib.loc, pkgs_txt = "extensions",
      abortcall = abortcall
    )
    # checking extensions AFTER basic package checks,
    # because these packages need to be actually installed and correctly specified
    # before I can check them
    # (dependencies and enhances, on the other hand, can be checked from the main package itself)
    tempfun <- function(x){
      depends <- pkg_get_deps(
        x, lib.loc=lib.loc, deps_type=c("Depends", "Imports"),
        base=FALSE, recom=FALSE, rstudioapi = FALSE
      )
      return(package %in% depends)
    }
    check_extends <- sapply(
      extends, tempfun
    )
    wrong_extends <- extends[!check_extends]
    if(length(wrong_extends)>0) {
      error.txt <- simpleError(paste0(
        "The following given extensions were not found to be actual reverse dependencies:",
        "\n",
        paste0(wrong_extends, collapse = ", ")
      ), call=abortcall)
      stop(error.txt)
    }
  }
  return(extends)
}

#' @keywords internal
#' @noRd
.internal_get_topics <- function(package, lib.loc) {
  f <- system.file("INDEX", package = package, lib.loc=lib.loc)
  f <- readLines(f) |> as.list()
  f <- f[!sapply(f, \(x)startsWith(x, " "))]
  f <- lapply(f, stringi::stri_extract_first_words)
}

.internal_check_conflicting_inops <- function(operators, overwrite, inherits, envir, abortcall) {
  message("Checking for conflicting infix operators in the current environment...")
  check_existing <- sapply(
    operators, \(x)exists(x, envir = envir, inherits = inherits, mode = "function")
  )
  all_conflicting <- sum(check_existing) == length(operators)

  if(sum(check_existing)>0 & !all_conflicting) {
    conflict.txt <- paste0(
      "The following infix operators already exist in the current environment:",
      "\n",
      paste0(operators[check_existing], collapse = ", "),
      "\n"
    )
  }
  if(sum(check_existing)>0 & all_conflicting) {
    conflict.txt <- paste0(
      "ALL prepared infix operators already exist in the current environment",
      "\n"
    )
  }
  if(isTRUE(overwrite) & sum(check_existing)>0) {
    message(simpleMessage(
    paste0(conflict.txt, "Overwriting existing infix operators...", "\n"),
    abortcall
    ))
    warning(simpleWarning(
      paste0(conflict.txt, "Existing infix operators have been overwritten...", "\n"),
      abortcall
    ))
  }
  if(isFALSE(overwrite) & sum(check_existing)>0) { stop(simpleError(
    paste0(conflict.txt, "Function halted"), abortcall
  ))}
  return(operators)
}

#' @keywords internal
#' @noRd
.internal_form_to_fun <- function(form, abortcall) {
  lst <- as.list(form)
  form <- format(form)
  if(length(lst)!=3) {
    error.txt <- simpleError(
      "Improper formula", call=abortcall
    )
    stop(error.txt)
  }
  lhs <- paste0("~ ", format(lst[[2]])) |> stats::as.formula()
  rhs <- paste0("~ ", format(lst[[3]])) |> stats::as.formula()
  vars.left <- all.vars(lhs)
  vars.right <- all.vars(rhs)
  if(!all(c(vars.left, vars.right) == make.names(c(vars.left, vars.right)))) {
    error.txt <- simpleError(
      "syntactically invalid variable names used in formula", call=abortcall
    )
    stop(error.txt)
  }
  if(length(vars.left)!=1) {
    error.txt <- simpleError(
      "left hand side may only have 1 variable", call=abortcall
    )
    stop(error.txt)
  }
  if(length(vars.right)==0) {
    error.txt <- simpleError(
      "right hand side has no variables", call=abortcall
    )
    stop(error.txt)
  }
  if(!any(vars.right %in% vars.left)) {
    error.txt <- simpleError(
      "right hand side does not contain declared variable", call=abortcall
    )
    stop(error.txt)
  }
  operation <- format(rhs) |>
    stringi::stri_replace_all(replacement = "", fixed = " ") |>
    stringi::stri_replace_all(replacement = "", fixed = "~")
  fun.txt <- paste0(
    "function(", vars.left[1], ") { ", operation, " }"
  )
  fun <- eval(parse(text=fun.txt))
  return(fun)
}

#' @keywords internal
#' @noRd
.internal_f_function_exists <- function(package, funcname) {
  tryCatch({
    utils::getFromNamespace(funcname, package)
    TRUE
  }, error = function(...) { FALSE })
}

