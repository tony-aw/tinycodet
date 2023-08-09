#' Report infix operators
#'
#' @description
#' The \code{report_inops()} function
#' returns a data.frame listing the infix operators defined
#' in the current environment
#' (like the global environment, or the environment within a function),
#' or a user specified environment.
#' It also reports from which packages the infix operators came from.
#'
#' @param env an optional environment to give,
#' where the function should look for infix operators. \cr
#' When not specified, the current environment
#' (like the global environment, or the environment within a function)
#' is used. \cr
#'
#'
#' @returns
#' A data.frame.
#'
#'
#' @seealso [tinyoperations_import()]
#'
#' @examples
#'
#' \dontrun{
#' report_inops()
#' }
#'
#'
#'

#' @rdname report_inops
#' @export
report_inops <- function(env) {
  if(missing(env)){env <- parent.frame(n = 1)}
  lst.funs <- utils::lsf.str(envir = env)
  if(length(lst.funs)==0) {
    return(NULL)
  }
  lst.inops <- grep("%|:=", lst.funs, value = TRUE)
  n <- length(lst.inops)
  if(n==0) {
    return(NULL)
  }

  df <- data.frame(
    infix_operator = character(n),
    package = character(n),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  for(i in 1:n) {
    f <- get(lst.inops[i], envir = env)
    package <- .internal_get_packagename(f)
    if(is.null(package)) { package <- NA }
    df$package[i] <- package
    df$infix_operator[i] <- lst.inops[i]
  }
  return(df)
}
