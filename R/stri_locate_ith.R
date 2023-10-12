#' Locate \eqn{i^{th}} Pattern Occurrence or Text Boundary
#'
#' @description
#'
#' The \code{stri_locate_ith()} function
#' locates the \eqn{i^{th}} occurrence of a pattern in each string of
#' some character vector. \cr
#' \cr
#' The \code{stri_locate_ith_boundaries()} function
#' locates the \eqn{i^{th}} text boundary
#' (like character, word, line, or sentence boundaries). \cr
#'
#' @param str a string or character vector.
#' @param regex,fixed,coll,charclass a character vector of search patterns,
#' as in \link[stringi]{stri_locate}. \cr
#' `r .mybadge_string("regex", "darkred")` \cr
#' `r .mybadge_string("fixed", "darkgreen")` \cr
#' `r .mybadge_string("coll", "pink")` \cr
#' `r .mybadge_string("charclass", "lightyellow")` \cr
#' @param type single string;
#' either the break iterator type,
#' one of \code{character}, \code{line_break}, \code{sentence}, \code{word},
#' or a custom set of ICU break iteration rules. Defaults to \code{"character"}. \cr
#' `r .mybadge_string("boundaries", "blue")` \cr
#' @param i a number, or a numeric vector of the same length as \code{str}. \cr
#' Positive numbers are counting from the left. Negative numbers are counting from the right.
#' I.e.:
#' \itemize{
#'  \item \code{stri_locate_ith(str, i=1, ...)} \cr
#' gives the position (range) of the first occurrence of a pattern.
#'  \item \code{stri_locate_ith(str, i=-1, ...)} \cr
#' gives the position (range) of the last occurrence of a pattern.
#'  \item \code{stri_locate_ith(str, i=2, ...)}\cr
#' gives the position (range) of the second occurrence of a pattern.
#'  \item \code{stri_locate_ith(str, i=-2, ...)} \cr
#' gives the position (range) of the second-last occurrence of a pattern.
#' }
#' If \code{abs(i)} is larger than the number of instances,
#' the first (if \code{i < 0}) or last (if \code{i > 0}) instance will be given. \cr
#' For example: suppose a string has \code{3} instances of some pattern; \cr
#' then if \code{i >= 3} the third instance will be located, \cr
#' and if \code{i <= -3} the first instance will be located. \cr
#' @param ... more arguments to be supplied to
#' \link[stringi]{stri_locate} or \link[stringi]{stri_locate_all_boundaries}. \cr
#' Do not supply the arguments
#' \code{omit_no_match}, \code{get_length}, or \code{pattern},
#' as they are already specified internally.
#' Supplying these arguments anyway will result in an error.
#'
#'
#' @details
#' \bold{Special note regarding charclass} \cr
#' The \code{stri_locate_ith()} function is based on
#' \link[stringi]{stri_locate_all}.
#' This generally gives results consistent with
#' \link[stringi]{stri_locate_first} or \link[stringi]{stri_locate_last},
#' but the exception is when \code{charclass} pattern is used. \cr
#' Where the functions
#' \link[stringi]{stri_locate_first} or \link[stringi]{stri_locate_last}
#' give the location of the first or last single character matching the \code{charclass}
#' (respectively),
#' \link[stringi]{stri_locate_all} gives the start and end of \bold{consecutive} characters. \cr
#' The \code{stri_locate_ith()} is in this aspect more in line with
#' \link[stringi]{stri_locate_all},
#' as it gives the \eqn{i^{th}} set of consecutive characters.
#'
#'
#' @returns
#' The \code{stri_locate_ith()} function returns an integer matrix with two columns,
#' giving the start and end positions of the \eqn{i^{th}} matches,
#' two \code{NA}s if no matches are found,
#' and also two \code{NA}s if \code{str} is \code{NA}.\cr
#' \cr
#'
#' @seealso [tinycodet_strings()]
#'
#'
#' @examples
#'
#' #############################################################################
#'
#' # practical example with regex & fixed ====
#'
#' # input character vector:
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#'
#' # report ith (second and second-last) vowel locations:
#' p <- rep("A|E|I|O|U", 2) # vowels
#' loc <- stri_locate_ith(x, c(2, -2), regex=p, case_insensitive=TRUE)
#' print(loc)
#'
#' # extract ith vowels:
#' extr <- stringi::stri_sub(x, from=loc)
#' print(extr)
#'
#' # replace ith vowels with numbers:
#' repl <- stringi::stri_replace_all(
#' extr, fixed = c("a", "e", "i", "o", "u"), replacement = 1:5, vectorize_all = FALSE
#' )
#' x <- stringi::stri_sub_replace(x, loc, replacement=repl)
#' print(x)
#'
#'
#' #############################################################################
#'
#' # practical example with boundaries ====
#'
#' # input character vector:
#' x <- c("good morning and good night",
#' "hello ladies and gentlemen")
#' print(x)
#'
#' # report ith word locations:
#' loc <- stri_locate_ith_boundaries(x, c(-3, 3), type = "word")
#' print(loc)
#'
#' # extract ith words:
#' extr <- stringi::stri_sub(x, from=loc)
#' print(extr)
#'
#' # transform and replace words:
#' tf <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
#' x <- stringi::stri_sub_replace(x, loc, replacement=tf)
#' print(x)
#'
#'
#' #############################################################################
#'
#' # find pattern ====
#'
#' extr <- stringi::stri_sub(x, from=loc)
#' repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
#' stringi::stri_sub_replace(x, loc, replacement=repl)
#'
#' # simple pattern ====
#'
#' x <- rep(paste0(1:10, collapse=""), 10)
#' print(x)
#' out <- stri_locate_ith(x, 1:10, regex = as.character(1:10))
#' cbind(1:10, out)
#'
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- rep("a|e|i|o|u",2)
#' out <- stri_locate_ith(x, c(-1, 1), regex=p)
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
#' out <- stri_locate_ith(x, c(1, -1), regex=p, case_insensitive=TRUE)
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
#' out <- stri_locate_ith(x, c(1, -1), regex=p, case_insensitive=TRUE)
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
#' loc <- stri_locate_ith(x, c(1, -1), regex="a|e|i|o|u")
#' extr <- stringi::stri_sub(x, from=loc)
#' repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
#' stringi::stri_sub_replace(x, loc, replacement=repl)
#'
#'
#' #############################################################################
#'
#' # Boundaries ====
#'
#' test <- c(
#' paste0("The\u00a0above-mentioned    features are very useful. ",
#'       "Spam, spam, eggs, bacon, and spam. 123 456 789"),
#'       "good morning, good evening, and good night"
#'       )
#' loc <- stri_locate_ith_boundaries(test, i = c(1, -1), type = "word")
#' stringi::stri_sub(test, from=loc)
#'


#' @rdname stri_locate_ith
#' @export
stri_locate_ith <- function(
    str, i, ... , regex, fixed, coll, charclass
) {
  n <- length(str)
  if(length(i) == 1) i <- rep.int(i, n)
  if(length(i) != n) {
    stop("`i` must be the same length as `str`, or be a length of 1")
  }
  if(any(i==0) || any(is.na(i))){
    stop("`i` is not allowed to be zero or NA")
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
    n.matches <- lengths(p1)/2
  } else if (providedarg["fixed"]) {
    p1 <- stringi::stri_locate_all_fixed(
      str=str, pattern=fixed, omit_no_match = FALSE, get_length = FALSE, ...
    )
    n.matches <- lengths(p1)/2
  } else if (providedarg["coll"]) {
    p1 <- stringi::stri_locate_all_coll(
      str=str, pattern=coll, omit_no_match = FALSE, get_length = FALSE, ...
    )
    n.matches <- lengths(p1)/2
  } else if (providedarg["charclass"]) {
    p1 <- stringi::stri_locate_all_charclass(
      str=str, pattern=charclass, omit_no_match = FALSE, get_length = FALSE, ...
    )
    n.matches <- lengths(p1)/2
  }

  n.matches <- pmax(n.matches, 1) # if no matches found, n.matches must be 1 so that NA is returned.
  neg <- which(i < 0)
  pos <- which(i > 0)
  i[neg] <- pmax(n.matches[neg] - abs(i[neg]+1), 1)
  i[pos] <- pmin(i[pos], n.matches[pos])

  rowind <- i + c(0, cumsum(n.matches))[seq_len(n)]
  mat <- do.call(rbind, p1)
  mat <- mat[rowind, , drop=FALSE]
  colnames(mat) <- c("start", "end")
  return(mat)
}

#' @rdname stri_locate_ith
#' @export
stri_locate_ith_boundaries <- function(
    str, i, ... , type = "character"
) {
  n <- length(str)
  if(length(i) == 1) i <- rep.int(i, n)
  if(length(i) != n) {
    stop("`i` must be the same length as `str`, or be a length of 1")
  }
  if(any(i==0) || any(is.na(i))){
    stop("`i` is not allowed to be zero or NA")
  }
  p1 <- stringi::stri_locate_all_boundaries(
    str = str, type = type, omit_no_match = FALSE, get_length = FALSE, ...
  )
  n.matches <- lengths(p1)/2

  n.matches <- pmax(n.matches, 1) # if no matches found, n.matches must be 1 so that NA is returned.
  neg <- which(i < 0)
  pos <- which(i > 0)
  i[neg] <- pmax(n.matches[neg] - abs(i[neg]+1), 1)
  i[pos] <- pmin(i[pos], n.matches[pos])

  rowind <- i + c(0, cumsum(n.matches))[seq_len(n)]
  mat <- do.call(rbind, p1)
  mat <- mat[rowind, , drop=FALSE]
  colnames(mat) <- c("start", "end")
  return(mat)
}

