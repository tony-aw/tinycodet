
#' @keywords internal
#' @noRd
.internal_check_deps_overlap_any <- function(
    pkgs, lib.loc, deps_type=c("Depends", "Imports", "LinkingTo")
) {
  all_deps <- sapply(
    pkgs, function(p) pkg_get_deps(
      p, lib.loc=lib.loc, deps_type=deps_type,
      base=FALSE, recom=FALSE, rstudioapi=FALSE, shared_tidy=FALSE
  )) |> unlist() |> unname()
  check <- any(pkgs %in% all_deps)
  return(check)
}


#' @keywords internal
#' @noRd
.internal_check_enhances <- function(package, enhances, lib.loc, abortcall) {
  actual_enhances <- pkg_get_deps(
    package, lib.loc=lib.loc, deps_type=c("Enhances"),
    base=TRUE, recom=TRUE, rstudioapi = TRUE, shared_tidy=TRUE
  ) |> unique()

  if(is.character(enhances) && length(enhances)>0) {
    .internal_check_pkgs(
      pkgs=enhances, lib.loc=lib.loc, pkgs_txt = "enhances",
      correct_pkgs = actual_enhances, abortcall=abortcall
    )
  }
}


#' @keywords internal
#' @noRd
.internal_get_Tmethods <- function(main_package, lib.loc, abortcall) {
  ns <- loadNamespace(main_package, lib.loc = lib.loc)
  Tfind <- stringi::stri_startswith_fixed(names(ns), ".__T__") & stringi::stri_detect_fixed(names(ns), ":")
  Tnames <- names(ns)[Tfind]
  if(length(Tnames) == 0) {
    return(list())
  }
  
  Tdenom <- stringi::stri_locate_last_fixed(Tnames, ":")[,1]
  Tfuns <- stringi::stri_sub(Tnames, 1L, Tdenom - 1L)
  Tfuns <- stringi::stri_replace_first_fixed(Tfuns, ".__T__", "")
  Tpkgs <- stringi::stri_sub(Tnames, Tdenom + 1L, nchar(Tnames))
  Tre_exports <- Tpkgs != main_package
  Tfuns <- Tfuns[Tre_exports]
  Tpkgs <- Tpkgs[Tre_exports]
  
  if(length(Tpkgs) == 0) {
    return(list())
  }
  
  uninstalled_pkgs <- Tpkgs[!Tpkgs %installed in% lib.loc]
  if(length(uninstalled_pkgs) > 0) {
    error.txt <- simpleError(paste0(
      "The following dependent packages (for the re-exports) are not installed:",
      "\n",
      paste0(uninstalled_pkgs, collapse = ", ")
    ), call = abortcall)
    stop(error.txt)
  }
  
  
  # get Tmethods from imports environment (i.e. the parent environment of the namespace):
  Tmethods <- as.list(parent.env(ns), all.names = TRUE, sorted = TRUE)
  
  
  # safety measures(1):
  if(any(!Tfuns %in% names(Tmethods))) {
    sel.ind <- which(Tfuns %in% names(Tmethods))
    Tfuns <- Tfuns[sel.ind]
    Tpkgs <- Tpkgs[sel.ind]
  }
  Tmethods <- Tmethods[Tfuns]
  
  # safety measures(2):
  sel.ind <- vapply(Tmethods, is.function, FUN.VALUE = logical(1))
  if(any(!sel.ind)) {
    sel.ind <- which(sel.ind)
    Tfuns <- Tfuns[sel.ind]
    Tpkgs <- Tpkgs[sel.ind]
  }
  Tmethods <- Tmethods[Tfuns]
  
  # make output list:
  Tmethods <- .rcpp_Tmethods(Tmethods, Tfuns, Tpkgs)
  return(Tmethods)
}


