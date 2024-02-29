#' Construct Formula With Safer Environment Specification
#'
#' @description
#' When creating a formula with the tilde ( `~ `) operator,
#' and storing a formula in a variable to be used later,
#' the environment is captured by the formula. \cr
#' Therefore,
#' any object in the captured environment might not be freed from the memory,
#' potentially creating some memory leak
#' (see also the Examples section below). \cr
#' \cr
#' The `form` function is a convenience function,
#' to quickly create and return/store a formula more safely,
#' by having no default environment,
#' and allowing the user to specify the environment explicitly. \cr
#' It can also quickly convert a single string to a formula,
#' again allowing for explicit environment specification,
#' and having no environment by default. \cr \cr
#' 
#'
#' @param f either one of the following:
#' * a literal single string, or a variable containing a single string. \cr
#'  If a string is given that does not contain a tilde ( `~` ),
#'  a tilde is prepended to the string before converting it to a formula. \cr
#' * a literal formula. \cr
#' Note that if a literal formula is given,
#' it must actually be a literal formula,
#' \bold{not} a variable that contains a formula,
#' as that would defeat the whole point of the `form()` function. \cr
#' @param env \link[base]{environment} of the formula. \cr
#' Defaults to `NULL`. \cr \cr
#'
#'
#' @returns
#' A formula with no environment,
#' or a formula with the specified environment. \cr \cr
#'
#' @seealso \link{tinycodet_safer}, \link{aes_pro}, \link{with_pro}
#'
#' @examples
#' 
#' # basic examples ====
#' 
#' (myform <- form(a ~ b))
#' (myform2 <- form("a ~ b"))
#' mystring <- "a ~ b"
#' (myform3 <- form(mystring))
#' form("a")
#' 
#' myform <- form(x ~ y)
#' environment(myform) # NULL
#' mydata <- data.frame(x = rnorm(1e5), y = rnorm(1e5))
#' lm(myform, data = mydata)
#' 
#' 
#' #############################################################################
#' 
#' 
#' # showcasing environment capture ====
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
#' exists("foo", envir = environment(x2)) # = TRUE: "foo" still exists!
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

