
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
