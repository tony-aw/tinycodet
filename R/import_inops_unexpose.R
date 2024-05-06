
#' @keywords internal
#' @noRd
.import_inops_unexpose_alias <- function(unexpose, env, abortcall) {
  
  # get infix operators in alias
  pkgs <- unlist(unexpose$.__attributes__.$pkgs) |> unique()
  funs <- names(unexpose)[eapply(unexpose, is.function) |> unlist()]
  inops <- funs[stringi::stri_detect(funs, regex = "%|:=")]
  if(length(inops) == 0) {
    message("no infix operators in this alias")
    return(NULL)
  }
  
  # get intersection infix operators between alias and environment
  inops <- intersect(inops, utils::lsf.str(envir = env))
  if(length(inops) == 0) {
    message("no infix operators to unexpose")
    return(NULL)
  }
  
  # get tinyops in environment
  inops.get <- mget(inops, envir = env, inherits = FALSE)
  checks <- .is.tinyinops(names(inops.get), pkgs, env)
  inops.get <- inops.get[checks]
  if(length(inops.get) == 0) {
    message("No infix operators from alias to unexpose")
    return(NULL)
  }
  
  operators <- names(inops.get)
  # MAIN FUNCTION:
  .import_inops_delete(operators, env, abortcall)
  
}

#' @keywords internal
#' @noRd
.import_inops_unexpose_package <- function(delete, lib.loc, env, abortcall) {
  
  message(
    "checking for infix operators exposed to the current environment by `import_inops()` ..."
  )
  
  # get functions in env
  all.funs <- mget(utils::lsf.str(envir = env), envir = env, inherits = FALSE)
  if(length(all.funs) == 0) {
    message("No infix operators from `import_inops()` to delete")
    return(NULL)
  }
  
  # get infix operators in env
  all.ops <- all.funs[grep("%|:=", names(all.funs), value = TRUE)]
  if(length(all.ops) == 0) {
    message("No infix operators from `import_inops()` to delete")
    return(NULL)
  }
  
  # get tinyinops objects in env
  checks <- .is.tinyinops(names(all.ops), delete, env)
  tinyops <- all.ops[checks]
  if(length(tinyops) == 0) {
    message("No infix operators from selected package to delete")
    return(NULL)
  }
  
  # get tinyinops from selected package
  tinyops <- tinyops[sapply(tinyops, FUN = \(x).internal_get_packagename(x) %in% delete)]
  if(length(tinyops) == 0) {
    message("No infix operators from selected package to delete")
    return(NULL)
  }
  operators <- names(tinyops)
  
  # MAIN FUNCTION:
  .import_inops_delete(operators, env, abortcall)
  
}


#' @keywords internal
#' @noRd
.import_inops_delete <- function(operators, env, abortcall) {
  message(paste0(
    "Removing the following infix operators:",
    "\n",
    paste0(operators, collapse = ", ")
  ))
  rm(list = operators, envir = env)
  message("Done")
}

