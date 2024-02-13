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
#' @param pattern,regex,fixed,coll,charclass a character vector of search patterns,
#' as in \link[stringi]{stri_locate}. \cr
#' `r .mybadge_string("regex", "darkred")` \cr
#' `r .mybadge_string("fixed", "darkgreen")` \cr
#' `r .mybadge_string("coll", "pink")` \cr
#' `r .mybadge_string("charclass", "lightyellow")` \cr
#' @param i an integer, or an integer vector of the same length as \code{str}. \cr
#' Positive numbers are counting from the left. Negative numbers are counting from the right.
#' I.e.:
#' \itemize{
#'  \item \code{stri_locate_ith(str, i = 1, ...)} \cr
#' gives the position (range) of the first occurrence of a pattern.
#'  \item \code{stri_locate_ith(str, i = -1, ...)} \cr
#' gives the position (range) of the last occurrence of a pattern.
#'  \item \code{stri_locate_ith(str, i = 2, ...)}\cr
#' gives the position (range) of the second occurrence of a pattern.
#'  \item \code{stri_locate_ith(str, i = -2, ...)} \cr
#' gives the position (range) of the second-last occurrence of a pattern.
#' }
#' If \code{abs(i)} is larger than the number of pattern occurrences \code{n},
#' the first (if \code{i < -n}) or last (if \code{i > n}) instance will be given. \cr
#' For example: suppose a string has \code{3} instances of some pattern; \cr
#' then if \code{i >= 3} the third instance will be located, \cr
#' and if \code{i <= -3} the first instance will be located. \cr
#' @param ... more arguments to be supplied to
#' \link[stringi]{stri_locate} or \link[stringi]{stri_locate_all_boundaries}. \cr
#' Do not supply the arguments
#' \code{omit_no_match} or \code{get_length},
#' as they are already specified internally.
#' Supplying these arguments anyway will result in an error.
#' @param opts_regex,opts_fixed,opts_collator,opts_brkiter
#' named list used to tune up the selected search engine's settings. \cr
#' see \link[stringi]{stri_opts_regex},
#' \link[stringi]{stri_opts_fixed},
#' \link[stringi]{stri_opts_collator},
#' and \link[stringi]{stri_opts_brkiter} \cr
#' NULL for the defaults. \cr
#' `r .mybadge_string("regex", "darkred")` \cr
#' `r .mybadge_string("fixed", "darkgreen")` \cr
#' `r .mybadge_string("coll", "pink")` \cr
#' `r .mybadge_string("charclass", "lightyellow")` \cr
#' `r .mybadge_string("boundaries", "blue")` \cr
#' @param merge logical, indicating if charclass locations should be merged or not. \cr
#' \bold{Details:} \cr
#' For the \code{charclass} pattern type,
#' the \code{stri_locate_ith()} function gives the start and end of
#' \bold{consecutive} characters by default,
#' just like \link[stringi]{stri_locate_all}. \cr
#' To give the start and end positions of single characters,
#' much like \link[stringi]{stri_locate_first} or \link[stringi]{stri_locate_last},
#' set \code{merge = FALSE}.
#' 
#' 
#' @details
#' The 'stringi' functions only support operations on the
#' first, last, or all occurrences of a pattern. \cr
#' The \code{stri_locate_ith()} function
#' allows locating the \eqn{i^{th}} occurrence of a pattern. \cr
#' This allows for several workflows
#' for operating on the \eqn{i^{th}} pattern occurrence. \cr
#' See also the examples section. \cr
#' \cr
#' \bold{Extract \eqn{i^{th}} Occurrence of a Pattern} \cr
#' For extracting the \eqn{i^{th}} pattern occurrence: \cr
#' Locate the the \eqn{i^{th}} occurrence using \code{stri_locate_ith()},
#' and then extract it using, for example, \link[stringi]{stri_sub}. \cr
#' \cr
#' \bold{Replace/Transform \eqn{i^{th}} Occurrence of a Pattern} \cr
#' For replacing/transforming the \eqn{i^{th}} pattern occurrence:
#' 
#'  1) Locate the the \eqn{i^{th}} occurrence using \code{stri_locate_ith()}.
#'  2) Extract the occurrence using \link[stringi]{stri_sub}.
#'  3) Transform or replace the extracted sub-strings.
#'  4) Return the transformed/replaced sub-string back,
#'  using again \link[stringi]{stri_sub}. \cr \cr
#' 
#' \bold{Capture Groups of \eqn{i^{th}} Occurrence of a Pattern} \cr
#' The \code{capture_groups} argument for \code{regex} is not supported within \code{stri_locate_ith()}. \cr
#' To capture the groups of the \eqn{i^{th}} occurrences:
#' 
#' 1) Use \code{stri_locate_ith()} to locate the \eqn{i^{th}} occurrences without group capture.
#' 2) Extract the occurrence using \link[stringi]{stri_sub}.
#' 3) Get the matched group capture on the extracted occurrences using \link[stringi]{stri_match}. \cr \cr
#' 
#' 
#' 
#' 
#' @section Warning:
#' The \code{stri_locate_ith}-functions
#' do not support \code{long vectors}
#' (i.e. character vectors with more than \code{2^31 - 1} strings). \cr
#' \cr
#' 
#'
#'
#' @returns
#' The \code{stri_locate_ith()} function returns an integer matrix with two columns,
#' giving the start and end positions of the \eqn{i^{th}} matches,
#' two \code{NA}s if no matches are found,
#' and also two \code{NA}s if \code{str} is \code{NA}.\cr
#' \cr
#' If an empty string or empty pattern is supplied,
#' a warning is given and a matrix with 0 rows is returned. \cr
#' \cr
#'
#' @seealso \link{tinycodet_strings}
#'
#'
#' @examples
#'
#' #############################################################################
#'
#' # practical example: transform regex pattern ====
#'
#' # input character vector:
#' x <- c(paste0(letters[1:13], collapse = ""), paste0(letters[14:26], collapse = ""))
#' print(x)
#'
#' # locate ith (second and second-last) vowel locations:
#' p <- rep("A|E|I|O|U", 2) # vowels
#' loc <- stri_locate_ith(x, c(2, -2), regex = p, case_insensitive = TRUE)
#' print(loc)
#'
#' # extract ith vowels:
#' extr <- stringi::stri_sub(x, loc)
#' print(extr)
#'
#' # transform & replace ith vowels with numbers:
#' repl <- chartr("aeiou", "12345", extr)
#' stringi::stri_sub(x, loc) <- repl
#' 
#' # result (notice ith vowels are now numbers):
#' print(x)
#'
#' #############################################################################
#'
#'
#' # practical example: group-capture regex pattern ====
#' 
#' # input character:
#' # first group: c(breakfast=eggs, breakfast=bacon)
#' # second group: c(lunch=pizza, lunch=spaghetti)
#' x <- c('breakfast=eggs;lunch=pizza',
#'        'breakfast=bacon;lunch=spaghetti',
#'        'no food here') # no group here
#' print(x)
#'        
#' # locate ith=2nd group:
#' p <- '(\\w+)=(\\w+)'
#' loc <- stri_locate_ith(x, i = 2, regex = p)
#' print(loc)
#' 
#' # extract ith=2nd group:
#' extr <- stringi::stri_sub(x, loc)
#' print(extr)
#' 
#' # capture ith=2nd group:
#' stringi::stri_match(extr, regex = p)
#' 
#' #############################################################################
#'
#'
#' # practical example: replace words using boundaries ====
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
#' # transform and replace words (notice ith words have inverted case):
#' tf <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
#' stringi::stri_sub(x, loc) <- tf
#' 
#' # result:
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
#' x <- c(paste0(letters[1:13], collapse=""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' p <- rep("a|e|i|o|u",2)
#' out <- stri_locate_ith(x, c(-1, 1), regex = p)
#' print(out)
#' substr(x, out[,1], out[,2])
#'
#'
#' #############################################################################
#'
#' # ignore case pattern ====
#'
#'
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' p <- rep("A|E|I|O|U", 2)
#' out <- stri_locate_ith(x, c(1, -1), regex = p, case_insensitive = TRUE)
#' substr(x, out[,1], out[,2])
#'
#'
#' #############################################################################
#'
#' # multi-character pattern ====
#'
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' # multi-character pattern:
#' p <- rep("AB", 2)
#' out <- stri_locate_ith(x, c(1, -1), regex = p, case_insensitive = TRUE)
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
#' loc <- stri_locate_ith(x, c(1, -1), regex = "a|e|i|o|u")
#' extr <- stringi::stri_sub(x, from = loc)
#' repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
#' stringi::stri_sub_replace(x, loc, replacement = repl)
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
#' stringi::stri_sub(test, from = loc)
#'


#' @rdname stri_locate_ith
#' @export
stri_locate_ith <- function(
    str, i, ... , regex, fixed, coll, charclass
) {
  
  providedarg <- c(
    regex = !missing(regex), fixed = !missing(fixed),
    coll = !missing(coll), charclass = !missing(charclass)
  )
  if(sum(providedarg) != 1) {
    stop(
      "you have to specify either `regex`, `fixed`, `coll`, `charclass`"
    )
  }

  if (providedarg["regex"])
    {
    return(stri_locate_ith_regex(str = str, pattern = regex, i = i, ...))
  }
  else if (providedarg["fixed"])
    {
    return(stri_locate_ith_fixed(str = str, pattern = fixed, i = i, ...))
  }
  else if (providedarg["coll"])
    {
    return(stri_locate_ith_coll(str = str, pattern = coll, i = i, ...))
  }
  else if (providedarg["charclass"])
    {
    return(stri_locate_ith_charclass(str = str, pattern = charclass, i = i, ...))
  }
}


#' @rdname stri_locate_ith
#' @export
stri_locate_ith_regex <- function(str, pattern, i, ..., opts_regex = NULL) {
  
  n <- length(str)
  
  p1 <- stringi::stri_locate_all_regex(
    str = str, pattern = pattern, capture_groups = FALSE,
    omit_no_match = FALSE, get_length = FALSE,
    ..., opts_regex = opts_regex
  )
  return(.stri_locate_ith_internal(p1, i, n, sys.call()))
}


#' @rdname stri_locate_ith
#' @export
stri_locate_ith_fixed <- function(str, pattern, i, ..., opts_fixed = NULL) {
  
  n <- length(str)
  
  p1 <- stringi::stri_locate_all_fixed(
    str = str, pattern = pattern, omit_no_match = FALSE, get_length = FALSE,
    ..., opts_fixed = opts_fixed
  )
  return(.stri_locate_ith_internal(p1, i, n, sys.call()))
}


#' @rdname stri_locate_ith
#' @export
stri_locate_ith_coll <- function(str, pattern, i, ..., opts_collator = NULL) {
  
  n <- length(str)
  
  p1 <- stringi::stri_locate_all_coll(
    str = str, pattern = pattern, omit_no_match = FALSE, get_length = FALSE,
    ..., opts_collator = opts_collator
  )
  return(.stri_locate_ith_internal(p1, i, n, sys.call()))
}


#' @rdname stri_locate_ith
#' @export
stri_locate_ith_charclass <- function(str, pattern, i, merge = TRUE, ...) {
  
  n <- length(str)
  
  p1 <- stringi::stri_locate_all_charclass(
    str = str, pattern = pattern, merge = merge,
    omit_no_match = FALSE, get_length = FALSE,
    ...
  )
  return(.stri_locate_ith_internal(p1, i, n, sys.call()))
}


#' @rdname stri_locate_ith
#' @export
stri_locate_ith_boundaries <- function(
    str, i, ... , opts_brkiter = NULL
) {
  n <- length(str)
  
  p1 <- stringi::stri_locate_all_boundaries(
    str = str,
    omit_no_match = FALSE, get_length = FALSE,
    ..., opts_brkiter = opts_brkiter
  )
  return(.stri_locate_ith_internal(p1, i, n, sys.call()))
  
}


#' @keywords internal
#' @noRd
.stri_locate_ith_internal <- function(p1, i, n, abortcall) {
  
  # `i` must have length `n`, as the value of `i` will change as `n.matches` changes
  if(length(i) == 1) i <- rep.int(i, n)
  if(length(i) != n) {
    stop(simpleError("`i` must be the same length as `str`, or be a length of 1", call = abortcall))
  }
  
  if(length(p1) == 0) {
    warning(simpleWarning("empty search not supported", call = abortcall))
    return(cbind(start = integer(0), end = integer(0)))
  }
  n.matches <- as.integer(lengths(p1) / 2L)
  # n.matches <- pmax(n.matches, 1) # if no matches found, n.matches must be 1 so that "match" NA is returned.
  neg <- which(i < 0)
  pos <- which(i > 0)
  
  bad_i <- length(i) != (length(neg) + length(pos))
  if(bad_i){
    stop(simpleError("`i` is not allowed to be zero or NA", call = abortcall))
  }
  
  i[neg] <- pmax(n.matches[neg] - abs(i[neg] + 1), 1)
  i[pos] <- pmin(i[pos], n.matches[pos])
  
  mat <- .rcpp_alloc_stri_locate_ith(p1, as.integer(i - 1L))
  colnames(mat) <- c("start", "end")
  
  return(mat)
}
