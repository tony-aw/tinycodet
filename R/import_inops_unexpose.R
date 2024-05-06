
#' @keywords internal
#' @noRd
.import_inops_unexpose_alias <- function(unexpose, env, abortcall) {
  
  # get infix operators in alias
  pkgs <- unlist(unexpose$.__attributes__.$pkgs) |> unique()
  funs <- names(unexpose)[eapply(unexpose, is.function) |> unlist()]
  inops <- funs[stringi::stri_detect(funs, regex = "%|:=")]
  if(length(inops) == 0) {
    message("No infix operators in this alias object")
    return(NULL)
  }
  
  # MAIN FUNCTION:
  .import_inops_delete(inops, pkgs, env, abortcall)
  
}

#' @keywords internal
#' @noRd
.import_inops_unexpose_package <- function(delete, lib.loc, env, abortcall) {
  
  .internal_check_forbidden_pkgs(
    pkgs = delete, lib.loc = lib.loc, pkgs_txt = "packages", abortcall = abortcall
  )
  .internal_check_pkgs(
    pkgs = delete, lib.loc = lib.loc, pkgs_txt = "packages", abortcall = abortcall
  )
  
  # get infix operators in package:
  ns <- .internal_prep_Namespace(delete, lib.loc, abortcall)
  funs <- names(ns)[vapply(ns, is.function, logical(1)) |> unlist()]
  inops <- funs[stringi::stri_detect(funs, regex = "%|:=")]
  if(length(inops) == 0) {
    message("No infix operators in this package")
    return(NULL)
  }
  
  # MAIN FUNCTION:
  .import_inops_delete(inops, delete, env, abortcall)
  
}


#' @keywords internal
#' @noRd
.import_inops_delete <- function(inops, pkgs, env, abortcall) {
  
  
  # get intersection infix operators between alias and environment
  inops <- intersect(inops, utils::lsf.str(envir = env))
  if(length(inops) == 0) {
    message("No infix operators to unexpose")
    return(NULL)
  }
  
  # get tinyops in environment
  inops.get <- mget(inops, envir = env, inherits = FALSE)
  checks <- .is.tinyinops(names(inops.get), pkgs, env)
  inops.get <- inops.get[checks]
  if(length(inops.get) == 0) {
    message("No infix operators to unexpose")
    return(NULL)
  }
  
  operators <- names(inops.get)
  
  message(paste0(
    "Removing the following infix operators:",
    "\n",
    paste0(operators, collapse = ", ")
  ))
  rm(list = operators, envir = env)
  message("Done")
}

