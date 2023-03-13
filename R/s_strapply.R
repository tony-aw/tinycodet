#' s_strapply
#'
#'@description
#' The \code{s_strapply(x, fun, w=F, clp=NULL, custom_apply=NULL)} function
#' applies the following steps to every element (every string) of character vector \code{x}: \cr
#'  1) the string is split into a vector of single characters (\code{w=F}),
#' or a vector of space-delimited words (\code{w=T}). \cr
#'  2) the function \code{fun()} is applied to the vector from step 1. \cr
#'  3) the result from step 2 is pasted together to form a single string element again,
#' using \code{paste0(..., collapse=clp)}. \cr
#'
#' The point of this function is to increase the flexibility and usefulness
#' of the other string operators in this package.
#'
#'
#' @param x a string or character vector.
#' @param fun a function with a single input argument to be applied to the splitted string. \cr
#' Due what this function actually does (see description),
#' additional arguments need to be specified within the function definition.
#' @param w logical; should each string in character vector \code{x} be splitted into space-delimited words (\code{w=T}),
#' or into single characters (w=F).
#' @param clp how should each string be pasted together? If NULL (Default),
#' the string is pasted together using \code{paste0(..., collapse="")} if \code{w=F},
#' and using \code{paste0(..., collapse=" ")} if \code{w=T}.
#' @param custom_apply a function. \code{s_strapply()} internally uses \code{apply}.
#' The user may choose to replace this with a custom apply-like function,
#' usually for multi-threading purposes.\cr
#' \code{custom_apply} must have the same argument convention
#' as \code{apply}, or else use the arguments \code{x} and \code{fun}. \cr
#' For example: \cr
#' \code{plan(multisession)} \cr
#' \code{s_strapply(..., custom_apply=future.apply::future_apply)} \cr
#' NOTE: It's better not to use multi-threading inside \code{fun} itself. \cr
#'
#'
#' @returns
#'
#' The \code{s_strapply()} function
#' generally returns a character vector of the same length as \code{x},
#' although this could depend on the function chosen for \code{fun}. \cr
#' \cr
#'
#'
#' @examples
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#'
#' # get which letter of the alphabet every character is:
#' s_strapply(x, fun=\(x)match(tolower(x), letters), clp=",")
#'
#' # shuffle words randomly:
#' s_strapply(x, w=TRUE, fun=\(x)sample(x))
#'
#'
#' # completely customized sorting of characters (first vowels, then the rest of the letters):
#' custom_order <- c("a", "e", "i", "o", "u", setdiff(letters, c("a", "e", "i", "o", "u")))
#' print(paste0(custom_order, collapse = ""))
#' s_strapply(x, fun=\(x){
#'   rest <- setdiff(x = unique(x), y = custom_order)
#'   y <- factor(x = x, levels = c(custom_order, rest), ordered = TRUE)
#'   return(sort(y))
#' })
#'
#'

#' @export
s_strapply <- function(x, fun, w=FALSE, clp=NULL, custom_apply=NULL) {
  if(length(w)>1) { stop("w must be of length 1") }
  if(length(clp)>1) { stop("clp must be of length 1") }

  if(is.null(clp)) { clp <- ifelse(w, " ", "") }

  mat <- stringi::stri_split_boundaries(
    x, type = ifelse(w, "word", "character"), simplify = TRUE
  )
  fun2 <- function(x) {
    fun(x) |> paste0(collapse = clp)
  }
  if(is.null(custom_apply)) {
    return(apply(mat, 1, fun2))
  }
  if(!is.null(custom_apply)) {
    return(custom_apply(mat, 1, fun2))
  }
}


