#' Report Infix Operators
#'
#' @description
#' The \code{report_inops()} function
#' returns a data.frame listing the infix operators defined
#' in the current environment,
#' or a user specified environment.
#' It also reports from which packages the infix operators came from.
#'
#' @param env an optional environment to give,
#' where the function should look for infix operators. \cr
#' When not specified, the current environment
#' is used. \cr
#'
#'
#' @returns
#' A data.frame.
#' The first column gives the infix operator names.
#' The second column gives the package the operator came from,
#' or NA if it did not come from a package.
#'
#'
#' @seealso [tinycodet_misc()]
#'
#' @examples
#'
#' report_inops()
#'
#' `%paste%` <- function(x,y)paste0(x,y)
#'
#' report_inops()
#'
#' import_inops("stringi")
#'
#' report_inops()
#'
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
  lst.inops <- .internal_grep_inops(lst.funs, type = 2)
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
