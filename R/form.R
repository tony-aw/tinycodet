#' Construct Formula Without A Default Environment
#'
#' @description
#' Creating a formula variable captures the environment. \cr
#' This is usually fine and intended. \cr
#' \cr
#' However, in some rare situations,
#' one might not want to capture the environment. \cr
#' For example: \cr
#' When using a function to construct a formula,
#' the captured environment inside the function might not be freed from the memory,
#' thus potentially creating some memory leak
#' (see also the Examples section below). \cr
#' \cr
#' The \code{form()} function is a convenience function,
#' to be used in such situations. \cr
#' The \code{form()} function prevents memory leakage,
#' by having no default environment,
#' and allowing the user to specify the environment explicitly only when the user needs it. \cr
#' It can also convert a single string to a formula,
#' again allowing for explicit environment specification,
#' and having no environment by default. \cr \cr
#' 
#'
#' @param f either one of the following:
#' * a literal single string, or a variable containing a single string. \cr
#'  If a string is given that does not contain a tilde ( `~` ),
#'  a tilde is prepended to the string before converting it to a formula. \cr
#'  Do not forget to surrounded special names with back-ticks.
#' * a literal formula. \cr
#' Note that if a literal formula is given,
#' it must actually be a literal formula,
#' \bold{not} a variable that contains a formula,
#' as that would defeat the whole point of the `form()` function. \cr
#' @param env The formula environment. \cr
#' Use either `NULL`, or the environment the formula depends on. \cr
#' For example: \cr
#' If the formula depends on functions defined in the current environment,
#' one must specify `env  = environment()`. \cr
#' Defaults to `NULL`. \cr \cr
#'
#'
#' @note
#' The use-case for `form()` is not very common. \cr
#' In the majority of cases, using a regular formula is fine. \cr
#' Use `form()` in situation where,
#' for example,
#' one constructs a formula via a function. \cr \cr
#'
#'
#' @returns
#' A formula with no environment,
#' or a formula with the specified environment. \cr \cr
#'
#' @seealso \link{tinycodet_misc}
#'
#' @examples
#' 
#' # see also http://adv-r.had.co.nz/memory.html
#' 
#' 
#' f1 <- function() {
#'   foo <- c(letters, LETTERS)
#'   return(10)
#' }
#' x1 <- f1()
#' environment(x1) |> as.list() # empty, since no formula is used inside f1(), so safe
#' 
#' 
#' f2 <- function() {
#'   foo <- c(letters, LETTERS)
#'   out <- a ~ b
#'   return(out)
#' }
#' x2 <- f2()
#' environment(x2) |> as.list() # NOT safe: contains all objects from f2()
#' exists("foo", envir = environment(x2)) # = TRUE: "foo" still exists
#' environment(x2)$foo # can still access it; probably won't be removed by gc()
#' 
#' 
#' f3 <- function() {
#'   foo <- c(letters, LETTERS)
#'   out <- form(a ~ b)
#'   return(out)
#' }
#' x3 <- f3()
#' environment(x3) |> as.list() # empty, since form() is used, so safe
#' 
#' 
#' f4 <- function() {
#'   foo <- c(letters, LETTERS)
#'   out <- form("a ~ b")
#'   return(out)
#' }
#' x4 <- f4()
#' environment(x4) |> as.list() # empty, since form() is used, so safe
#' 
#' 

#' @rdname form
#' @export
form <- function(f, env = NULL) {
  
  if(is.character(f)) {
    if(length(f) > 1) stop("multiple strings not allowed")
    if(!grepl("~", f, fixed = TRUE)) {
       f <- paste0("~ ", f)
    }
    return(stats::as.formula(f, env = env))
  }
  
  is_formula <- inherits(f, "formula") && is.call(f) && f[[1]] == "~"
  if(isTRUE(is_formula)) {
    chrdepsub <- as.character(deparse(substitute(f), backtick = TRUE))
    is_literal_formula <- isTRUE(
      grepl("~", chrdepsub, fixed = TRUE)
    ) && isTRUE(
      chrdepsub == deparse(f)
    )
    if(!is_literal_formula) stop("if `f` is a formula, it must be a literal formula, not a variable that contains a formula")
    environment(f) <- env
    return(f)
  }
  
  stop("`f` must be a single string or a literal formula")
  
}

