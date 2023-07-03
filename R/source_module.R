#' Additional module import management
#'
#' @description
#' The \code{alias %@@source% list(file=...)} operator
#' imports all objects from a source-able script file under an \code{alias}. \cr
#' \cr
#' The \code{source_inops()} function
#' exposes the infix operators defined in a source-able script file
#' to the current environment
#' (like the global environment, or the environment within a function). \cr
#' \cr
#' Note that the \code{alias %@@source% list(file=...)} operator and
#' the \code{source_inops()} function do NOT suppress output
#' (i.e. plots, prints, messages)
#' from the sourced module file. \cr
#'
#' @param alias a variable name (unquoted),
#' giving the (not yet existing) object
#' where the sourced objects from the module are to be assigned to. \cr
#' Syntactically invalid names are not allowed for the alias name.
#' @param lst a named list, giving the arguments to be passed to the
#' \link[base]{source} function. \cr
#' For example: \code{alias \%@@source\% list(file="mydir/myscript.R")} \cr
#' The \code{local} argument should not be included in the list.
#' @param ... arguments to be passed to the \link[base]{source} function,
#' such as the \code{file} argument. \cr
#' The \code{local} argument should not be included.
#'
#'
#' @returns
#' For the \code{alias %@@source% list(file=...)} operator: \cr
#' The variable named as the \code{alias} will be created
#' (if it did not already exist) in the current environment
#' (like the Global environment, or the environment within a function),
#' and will contain all objects from the sourced script. \cr
#' \cr
#' For \code{source_inops()}: \cr
#' The infix operators from the specified module will be placed
#' in the current environment
#' (like the Global environment, or the environment within a function).\cr
#' \cr
#' 
#' @seealso \link[=import_as]{import}, \link[=pkgs_get_deps]{pkgs}, [base::source()]
#'
#' @examples
#'
#' \dontrun{
#' alias %@source% list(file="mydir/mymodule.R")
#' source_inops(file="mydir/mymodule.R")
#' }
#'
#'
#'

#' @name source_module
NULL

#' @rdname source_module
#' @export
`%@source%` <- function(alias, lst) {
  # Check alias:
  alias_chr <- as.character(substitute(alias))
  check_proper_alias <- c(
    make.names(alias_chr)==alias_chr,
    isTRUE(nchar(alias_chr)>0),
    length(alias_chr)==1,
    isFALSE(alias_chr %in% c("T", "F")),
    !startsWith(alias_chr, ".")
  )
  if(!isTRUE(all(check_proper_alias))){
    stop("Syntactically invalid name for object `alias`")
  }
  if(any(names(lst) %in% c("local"))) {
    stop(paste0(
      "Do not supply the `local` argument:",
      "\n",
      "Environment is already specified by the alias"
    ))
  }
  message("Importing module ... \n")
  tempenv <- new.env(parent=parent.frame())
  do.call(source, c(lst, local=tempenv))
  message(paste0(
    "Done", "\n",
    "You can now access the sourced objects using ",
    substitute(alias), "$...", "\n"
  ))
  assign(alias_chr, tempenv, envir = parent.frame(n = 1))
}


#' @rdname source_module
#' @export
source_inops <- function(...) {
  
  lst <- list(...)
  if(any(names(lst) %in% c("local"))) {
    stop(paste0(
      "Do not supply the `local` argument:",
      "\n",
      "Environment is already specified by the alias"
    ))
  }
  
  tempenv <- new.env(parent=parent.frame())
  do.call(source, c(list(local=tempenv), lst))
  operators <- names(tempenv)[unlist(eapply(tempenv, is.function))]
  operators <- stringi::stri_subset(operators, regex="%|:=")
  if(length(operators)==0) {
    message("no infix operators found in sourced module")
  }
  if(length(operators)>0) {
    message("placing operators in current environment...")
    for(op in operators) {
      assign(op, tempenv[[op]], envir = parent.frame(n = 1))
    }
  }
  
}
