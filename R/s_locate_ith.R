#' Locate the \eqn{i^{th}} occurrence of a pattern, or locate presence of pattern
#'
#'@description
#' The \code{s_locate_ith} functions
#' locates the \eqn{i^{th}} occurrence of a pattern in each string of
#' some character vector. It's output can be used in the many string sub-setting functions to
#' extract, transform, replace, add-before, or add-after the \eqn{i^{th}} pattern. \cr
#' \cr

#' @param x a string or character vector.
#' @param p the result from \link{s_pattern},
#' or else a character vector of the same length as \code{s} with regular expressions.
#' @param i a number, or a numeric vector of the same length as \code{x}.
#' This gives the \eqn{i^{th}} instance to be replaced. \cr
#' Positive numbers are counting from the left. Negative numbers are counting from the right. I.e.: \cr
#' \code{s_locate_ith(x, i=1, p, rp)}
#' gives the position (range) of the first occurence of pattern \code{p}. \cr
#' \code{s_locate_ith(x, i=-1, p, rp)}
#' gives the position (range) of the last occurence of pattern \code{p}. \cr
#' \code{s_locate_ith(x, i=2, p, rp)}
#' gives the position (range) of the second occurence of pattern \code{p}. \cr
#' \code{s_locate_ith(x, i=1, p, rp)}
#' gives the position (range) of the second-last occurence of pattern \code{p}. \cr
#' If i is larger than the number of instances, the maximum instance will be given. \cr
#' For example: suppose a string has 3 instances of p; \cr
#' then if \code{i=4} the third instance will be located, \cr
#' and if \code{i=-3} the first instance will be located. \cr
#' @param custom_mapply the \code{s_locate_ith()} function
#' internally use \code{mapply()}. The user may choose to replace this with a custom functions,
#' for example for multi-threading purposes. The replacing function must have the same argument convention
#' as \code{mapply}. \cr
#' For example:\cr
#' s_locate_ith(..., custom_mapply=future_mapply) \cr
#' NOTE: if you use \code{s_locate_ith()} inside an \code{s_strapply()} call,
#' and you want to replace the apply functions for multi-threading reasons,
#' I highly advise the user to only replace the \code{sapply} function in \code{s_strapply},
#' and to leave \code{mapply} inside \code{s_locate_ith()} without multi-threading: \cr
#' Running nested multi-threading processes may actually slow down the code, and may cause other problems also.
#' I.e. run this: \cr
#' \code{s_strapply(x, w=T, fun=\(x)s_locate_ith(x, -2, p), custom_sapply = future_sapply)} \cr
#' and not this: \cr
#' \code{s_strapply(x, w=T, fun=\(x)s_locate_ith(x, -2, p, custom_mapply=future_mapply), custom_sapply=future_sapply)} \cr
#'
#'
#'
#'
#' @returns
#' An integer matrix with 3 columns: \cr
#' The first column gives the start position of the \eqn{i^{th}} occurence of pattern \code{p}. \cr
#' The second column gives the end position of the \eqn{i^{th}} occurence of pattern \code{p}. \cr
#' The third column gives the length of the position range of the \eqn{i^{th}} occurence of pattern \code{p}. \cr
#'
#'
#'
#' @examples
#'
#' # simple pattern ====
#'
#' x <- rep(paste0(1:10, collapse=""), 10)
#' print(x)
#' out <- s_locate_ith(x, 1:10, as.character(1:10))
#' cbind(1:10, out)
#'
#' x <- rep(paste0(1:10, collapse=""), 10)
#' print(x)
#' out <- s_locate_ith(x, -1:-10, as.character(1:10))
#' cbind(1:10, out)
#'
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- rep("a|e|i|o|u",2) # same as p <- s_pattern(regex=rep("a|e|i|o|u", 2), ignore.case=FALSE)
#' out <- s_locate_ith(x, c(-1, 1), p)
#' substr(x, out[,1], out[,2])
#'
#'
#' #############################################################################
#'
#' # ignore case pattern ====
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' # pattern with ignore.case=TRUE:
#' p <- s_pattern(regex = rep("A|E|I|O|U", 2), ignore.case=TRUE)
#' out <- s_locate_ith(x, c(1, -1), p)
#' substr(x, out[,1], out[,2])
#'
#'
#' #############################################################################
#'
#' # multi-character pattern ====
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' # multi-character pattern:
#' p <- s_pattern(regex = rep("AB", 2), ignore_case=TRUE)
#' out <- s_locate_ith(x, c(1, -1), p)
#' substr(x, out[,1], out[,2])
#'

#' @rdname s_locate_ith
#' @export
s_locate_ith <- function(x, i, p, custom_mapply=NULL) {
  if(length(i)==1) i <- rep(i, length(x))
  if(length(i)!=length(x)){
    stop("i must be of length 1, or the same length as x")
  }
  if(isTRUE(attr(p, "engine")=="stringi")){
    p1 <- do.call(stringi::stri_locate_all, c(list(str=x), p))
  } else{
    p1 <- stringi::stri_locate_all(x, regex=p)
  }
  n.matches <- sapply(p1, nrow)
  i[i<0] <- pmax(n.matches[i<0] - abs(i[i<0]+1), 1)
  i[i>0] <- pmin(i[i>0], n.matches[i>0])

  if(is.null(custom_mapply)) {
    p2 <- mapply(function(x, i)x[i,], x=p1, i=i, SIMPLIFY = FALSE)
  }
  if(!is.null(custom_mapply)){
    p2 <- custom_mapply(function(x, i)x[i,], x=p1, i=i, SIMPLIFY = FALSE)
  }

  p3 <- do.call(rbind, p2)
  p3 <- cbind(p3, "length" = p3[,2] - p3[, 1] +1)
  return(p3)
}
