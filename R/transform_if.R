#' The transform_if function
#'
#' @description
#'
#' The \code{transform_if()} function is alternative form of \link[base]{ifelse}.
#' The \code{transform_if()} function transforms an object \code{x},
#' based on logical (\code{TRUE, FALSE, NA}) condition \code{cond(x)},
#' such that: \cr
#'
#' \itemize{
#'  \item For every value where \code{cond(x)==TRUE}, function \code{yes(x)} is run.
#'  \item For every value where \code{cond(x)==FALSE}, function \code{no(x)} is run.
#'  \item For every value where \code{cond(x)==NA}, function \code{other(x)} is run. \cr
#' }
#'
#'
#' @param x a vector, matrix, or array.
#' @param cond a (possibly anonymous) function that returns a \code{logical} vector
#' of the same length/dimensions as \code{x}. \cr
#' For example: \code{\(x)x>0}. \cr
#' @param yes the (possibly anonymous) transformation function to use
#' when \code{cond(x)==TRUE}. \cr
#' For example: \code{log}. \cr
#' If this is not specified, \code{yes} defaults to \code{function(x)x}.
#' @param no the (possibly anonymous) transformation function to use
#' when \code{cond(x)==FALSE}. \cr
#' For example: \code{log}. \cr
#' If this is not specified, \code{no} defaults to \code{function(x)x}.
#' @param other the (possibly anonymous) transformation function to use
#' when \code{cond(x)} returns \code{NA}. \cr
#' If this is not specified, \code{other} defaults to \code{function(x)return(NA)}. \cr
#' Note that \code{other(x)} is run when \code{cond(x)} is \code{NA},
#' not necessarily when \code{x} itself is \code{NA}!
#' @param text a single string,
#' to be used instead of the arguments
#' \code{cond, yes, no, other},
#' in the form of \cr
#' \code{"x ; cond ; yes ; no ; other"}, with \code{"x"} being the declared variable. \cr
#' If this string is given, \code{cond, yes, no, other} will be ignored. \cr
#' The single string in question should have the following properties: \cr
#'  * It should consist of 5 pieces of text, separated by semicolons (\code{;}).
#'  * The first piece should be a single name, declaring the name of the variable.
#'  * The second, third, fourth, and fifth pieces of text should give the expressions describing the functions
#'  to use for \code{cond, yes, no, other}, respectively, in exactly that order.
#'  * Suppose the declared variable is named \code{x}.
#'  Then, each expression in text pieces 2 to 5 will be translated as: \cr
#'  \code{function(x) text}. \cr
#'  Therefore, a function like \code{log} must be written in the text as
#'  \code{"log(x)"}, not just \code{"log"}.
#'  * All variables used on pieces 2 to 5
#'  that do no match the declared variable from the first piece,
#'  are taken from the environment from which \code{transform_if()} was called.
#'  * ALL 5 pieces of text are mandatory; any missing piece results in an error.
#'  * Thus the following functions (with the declared variable being \code{x}), \cr
#'  \code{cond=\(x)x > y, yes=log, no=\(x)x^2, other=\(x)-1000}, \cr
#'  can be expressed in a single string as: \cr
#'  \code{"x; x > y; log(x) ; x^2 ; -1000"}
#'
#'
#' @details
#' Be careful with coercion! For example the following code: \cr
#' \code{x <- c("a", "b")} \cr
#' \code{transform_if(x, \(x)x=="a", as.numeric, as.logical)} \cr
#' returns: \cr
#' \code{[1] NA NA} \cr
#' due to the same character vector being given 2 incompatible classes. \cr
#' \cr
#' Using \code{cond, yes, no, other} directly
#' is faster than using the \code{text} argument
#' (because the text must first be translated into functions).
#' Thus if speed is of primary interest,
#' don't use the text approach. \cr
#'
#'
#' @returns
#' Similar to \link[base]{ifelse}.
#' However, unlike \code{ifelse()}, the transformations are evaluated as
#' \code{yes(x[cond(x)])} and \code{no(x[!cond(x)])},
#' ensuring no unnecessary warnings or errors occur. \cr
#' \cr
#'
#'
#' @seealso [tinyoperations_dry()]
#'
#' @examples
#' x <- c(-10:9, NA, NA)
#' object_with_very_long_name <- matrix(x, ncol=2)
#' print(object_with_very_long_name)
#' y <- 0
#' z <- 1000
#' object_with_very_long_name |> transform_if(\(x)x>y, log, \(x)x^2, \(x)-z)
#' object_with_very_long_name |> transform_if(text = "x ; x>y ; log(x) ; x^2 ; -z")
#'


#' @rdname transform_if
#' @export
transform_if <- function(
    x, cond,
    yes=function(x)x, no=function(x)x, other=function(x)return(NA),
    text = NULL
) {
  if(!is.null(text)){
    lst <- .text2funs(text, env = parent.frame(n = 1), abortcall = sys.call())
    cond <- lst[[1]]
    yes <- lst[[2]]
    no <- lst[[3]]
    other <- lst[[4]]
  }
  check <- all(c(
   is.function(cond), is.function(yes), is.function(no), is.function(other)
  ))
  if(!isTRUE(check)) {
    stop("`cond`, `yes`, `no`, and `other` must all be functions")
  }
  y <- x
  ind_l <- cond(y)
  if(!is.logical(ind_l)) {
    stop("`cond` does not return a logical vector!")
  }
  if(isTRUE(any(ind_l))) {
    ind_T <- which(ind_l)
    y[ind_T] <- yes(y[ind_T])
  }
  if(isTRUE(any(!ind_l))) {
    ind_F <- which(!ind_l)
    y[ind_F] <- no(y[ind_F])
  }
  if(isTRUE(any(is.na(ind_l)))) {
    ind_NA <- which(is.na(ind_l))
    y[ind_NA] <- other(y[ind_NA])
  }

  return(y)
}


#' @keywords internal
#' @noRd
.text2funs <- function(text, env, abortcall) {
  if(length(text)>1) {
    error.txt <- simpleError(
      "Only a single string must be given", call=abortcall
    )
    stop(error.txt)
  }
  txt.trim <- stringi::stri_replace_all(text, "", fixed= " ")
  txt.cut <- stringi::stri_split_fixed(txt.trim, ";", simplify = TRUE)
  forms <- lapply(txt.cut, FUN = \(x)paste0("~", x)|> stats::as.formula())
  if(length(forms)!=5) {
    error.txt <- simpleError(
      "Improper string: \n String must consist of exactly 5 pieces, delimited by a semicolon (;)", call=abortcall
    )
    stop(error.txt)
  }
  vars.left <- all.vars(forms[[1]])
  vars.right <- lapply(forms[2:5], all.vars) |> unlist()
  check <- all(c(vars.left, vars.right) == make.names(c(vars.left, vars.right)))
  if(!check) {
    error.txt <- simpleError(
      "syntactically invalid variable variable names used in string", call=abortcall
    )
    stop(error.txt)
  }
  if(length(vars.left)!=1) {
    error.txt <- simpleError(
      "must declare exactly one variable", call=abortcall
    )
    stop(error.txt)
  }
  if(length(vars.right)==0) {
    error.txt <- simpleWarning(
      "expressions have no variables", call=abortcall
    )
    stop(error.txt)
  }
  if(!any(vars.right %in% vars.left)) {
    error.txt <- simpleError(
      "expressions in string do not contain declared variable", call=abortcall
    )
    stop(error.txt)
  }
  operations <- lapply(
    forms[2:5],
    FUN = \(x) { format(x) |>
    stringi::stri_replace_all(replacement = "", fixed = " ") |>
    stringi::stri_replace_all(replacement = "", fixed = "~") }
  )
  cond.txt <- paste0(
    "function(", vars.left[1], ") { ", operations[1], " }"
  )
  yes.txt <- paste0(
    "function(", vars.left[1], ") { ", operations[2], " }"
  )
  no.txt <- paste0(
    "function(", vars.left[1], ") { ", operations[3], " }"
  )
  other.txt <- paste0(
    "function(", vars.left[1], ") { ", operations[4], " }"
  )
  cond <- eval(parse(text=cond.txt), envir = env)
  yes <- eval(parse(text=yes.txt), envir = env)
  no <- eval(parse(text=no.txt), envir = env)
  other <- eval(parse(text=other.txt), envir = env)
  out <- list(cond, yes, no, other)
  return(out)
}
