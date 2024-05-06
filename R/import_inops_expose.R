
#' @keywords internal
#' @noRd
.import_inops_expose_alias <- function(
    expose, env, lst_opts, abortcall) {
  
  
  funs <- names(expose)[eapply(expose, is.function) |> unlist()]
  inops <- funs[stringi::stri_detect(funs, regex = "%|:=")]
  
  ns <- as.list(expose, all.names = TRUE, sorted = TRUE)
  
  .import_inops_expose(ns, inops, lst_opts, env, abortcall)
  
}

#' @keywords internal
#' @noRd
.import_inops_expose_package <- function(
    package, lib.loc, env, lst_opts, abortcall
) {
  
  
  # check package
  .internal_check_forbidden_pkgs(
    pkgs = package, lib.loc = lib.loc, pkgs_txt = "packages", abortcall = sys.call()
  )
  .internal_check_pkgs(
    pkgs = package, lib.loc = lib.loc, pkgs_txt = "packages", abortcall = sys.call()
  )

  # FUNCTION:
  ns <- .internal_prep_Namespace(package, lib.loc, abortcall = sys.call())
  operators <-  grep("%|:=", names(ns), value = TRUE)

  .import_inops_expose(ns, operators, lst_opts, env, abortcall)

}

#' @keywords internal
#' @noRd
.import_inops_expose <- function(
    ns, operators, lst_opts, env, abortcall
) {
  
  exclude <- lst_opts$exclude
  include.only <- lst_opts$include.only
  overwrite <- lst_opts$overwrite
  inherits <- lst_opts$inherits
  
  if(length(operators) == 0) {
    message("No infix operators present")
    return(NULL)
  }
  operators <- .import_exclude_include(operators, exclude, include.only, abortcall = abortcall)
  if(length(operators) == 0) {
    message("No infix operators to expose")
    return(NULL)
  }
  
  .internal_check_conflicting_inops(
    operators, overwrite, inherits, envir = env, abortcall = abortcall
  )
  
  message(
    "Placing infix operators in current environment..."
  )
  for(op in operators){
    check_existence <- .is.tinyLL(op, env = env)
    if(isTRUE(check_existence)) {
      rm(list = op, envir = env)
    }
    assign(op, ns[[op]], envir = env)
    
  }
  message("Done")
}


