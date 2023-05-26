#' Locate \eqn{i^{th}} Pattern Occurrence
#'
#'@description
#' The \code{stri_locate_ith} function
#' locates the \eqn{i^{th}} occurrence of a pattern in each string of
#' some character vector. \cr
#'
#'
#' @param str a string or character vector.
#' @param regex,fixed,coll,charclass a character vector of search patterns,
#' as in \link[stringi]{stri_locate}. \cr
#' @param i a number, or a numeric vector of the same length as \code{str}.
#' This gives the \eqn{i^{th}} instance to be replaced. \cr
#' Positive numbers are counting from the left. Negative numbers are counting from the right.
#' I.e.: \cr
#' \code{stri_locate_ith(str, i=1, ...)}
#' gives the position (range) of the first occurrence of a pattern. \cr
#' \code{stri_locate_ith(str, i=-1, ...)}
#' gives the position (range) of the last occurrence of a pattern. \cr
#' \code{stri_locate_ith(str, i=2, ...)}
#' gives the position (range) of the second occurrence of a pattern. \cr
#' \code{stri_locate_ith(str, i=-2, ...)}
#' gives the position (range) of the second-last occurrence of a pattern. \cr
#' If \code{abs(i)} is larger than the number of instances,
#' the first (if \code{i < 0}) or last (if \code{i > 0}) instance will be given. \cr
#' For example: suppose a string has \code{3} instances of some pattern; \cr
#' then if \code{i >= 4} the third instance will be located, \cr
#' and if \code{i <= -3} the first instance will be located. \cr
#' @param ... more arguments to be supplied to
#' \link[stringi]{stri_locate} and \link[stringi]{stri_count}.
#' @param simplify either \code{TRUE} or \code{FALSE} (default = \code{FALSE}): \cr
#'  * If \code{FALSE}, \code{stri_locate_ith} returns a list,
#'  usable in the \link[stringi]{stri_sub_all} functions
#'  (for example: to transform all matches). \cr
#'  * If \code{TRUE}, \code{stri_locate_ith}
#'  returns an integer matrix of positions and lengths. \cr
#'
#'
#'
#'
#' @returns
#' If \code{simplify = FALSE}, \code{stri_locate_ith} returns a list, one element for each string.
#' Each list element consists of a matrix with 2 columns and one row: \cr
#' The first column gives the start position of the \eqn{i^{th}} occurrence of the pattern. \cr
#' The second column gives the end position of the \eqn{i^{th}} occurrence of the pattern. \cr
#' When \code{simplify=FALSE}, the results can be used in the \code{from} argument
#' in the \link[stringi]{stri_sub_all} functions,
#' for example to transform the \eqn{i^{th}} matches
#' (see examples section below). \cr
#' \cr
#' If \code{simplify = TRUE} (default), \code{stri_locate_ith} this returns an integer matrix with 3 columns: \cr
#' The first column gives the start position of the \eqn{i^{th}} occurrence of the pattern. \cr
#' The second column gives the end position of the \eqn{i^{th}} occurrence of the pattern. \cr
#' The third column gives the length of the position range of
#' the \eqn{i^{th}} occurrence of the pattern. \cr
#' \cr
#' If no matches are found in a string,
#' \code{NA} is filled in for the start and end positions (and length).
#' \cr
#'
#'
#'
#'
#' @examples
#'
#' # simple pattern ====
#'
#' x <- rep(paste0(1:10, collapse=""), 10)
#' print(x)
#' out <- stri_locate_ith(x, 1:10, regex = as.character(1:10), simplify=TRUE)
#' cbind(1:10, out)
#'
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- rep("a|e|i|o|u",2)
#' out <- stri_locate_ith(x, c(-1, 1), regex=p, simplify=TRUE)
#' print(out)
#' substr(x, out[,1], out[,2])
#'
#'
#' #############################################################################
#'
#' # ignore case pattern ====
#'
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- rep("A|E|I|O|U", 2)
#' out <- stri_locate_ith(x, c(1, -1), regex=p, case_insensitive=TRUE, simplify=TRUE)
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
#' p <- rep("AB", 2)
#' out <- stri_locate_ith(x, c(1, -1), regex=p, simplify=TRUE, case_insensitive=TRUE)
#' print(out)
#' substr(x, out[,1], out[,2])
#'
#'
#'
#' #############################################################################
#'
#' # Replacement transformation using stringi ====
#'
#' x <- c("hello world", "goodbye world")
#' loc <- stri_locate_ith(x, c(1, -1), regex="a|e|i|o|u", simplify = FALSE)
#' extr <- stringi::stri_sub_all(x, from=loc)
#' repl <- lapply(extr, \(x)chartr(x, old = "a-zA-Z", new = "A-Za-z"))
#' stringi::stri_sub_all_replace(x, loc, replacement=repl)
#'
#'


#' @rdname stri_locate_ith
#' @export
stri_locate_ith <- function(
    str, i, ... , regex, fixed, coll, charclass, simplify=FALSE
) {
  if(length(i)==1) i <- rep(i, length(str))
  if(length(i)!=length(str)) {
    stop("`i` must be the same length as `str`, or be a length of 1")
  }
  if(any(i==0)){
    stop("`i` is not allowed to be zero")
  }
  providedarg <- c(
    regex = !missing(regex), fixed = !missing(fixed),
    coll = !missing(coll), charclass = !missing(charclass)
  )
  if(sum(providedarg) != 1) {
    stop(
      "you have to specify either `regex`, `fixed`, `coll`, `charclass`"
    )
  }

  if (providedarg["regex"]) {
    p1 <- stringi::stri_locate_all_regex(
      str=str, pattern=regex, omit_no_match = FALSE, get_length = FALSE, ...
    )
    n.matches <- stringi::stri_count_regex(
      str=str, pattern=regex, ...
    )
  }
  else if (providedarg["fixed"]) {
    p1 <- stringi::stri_locate_all_fixed(
      str=str, pattern=fixed, omit_no_match = FALSE, get_length = FALSE, ...
    )
    n.matches <- stringi::stri_count_fixed(
      str=str, pattern=fixed, ...
    )
  }
  else if (providedarg["coll"]) {
    p1 <- stringi::stri_locate_all_coll(
      str=str, pattern=coll, omit_no_match = FALSE, get_length = FALSE, ...
    )
    n.matches <- stringi::stri_count_coll(
      str=str, pattern=coll, ...
    )
  }
  else if (providedarg["charclass"]) {
    p1 <- stringi::stri_locate_all_charclass(
      str=str, pattern=charclass, omit_no_match = FALSE, get_length = FALSE, ...
    )
    n.matches <- stringi::stri_count_charclass(
      str=str, pattern=charclass, ...
    )
  }

  n.matches <- pmax(n.matches, 1) # if no matches found, n.matches must be 1 so that NA is returned.
  neg <- i < 0
  pos <- i > 0
  i[neg] <- pmax(n.matches[neg] - abs(i[neg]+1), 1)
  i[pos] <- pmin(i[pos], n.matches[pos])
  p2 <- mapply(function(x, i)x[i, ,drop=FALSE], x=p1, i=i, SIMPLIFY = FALSE)

  if(!simplify) {
    return(p2)
  }
  if(simplify){
    p3 <- do.call(rbind, p2)
    p3 <- cbind(p3, "length" = p3[,2] - p3[, 1] +1)
    return(p3)
  }
}

