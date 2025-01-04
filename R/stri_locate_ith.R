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
#' as in \link[stringi]{stri_locate_all}. \cr
#' `r .mybadge_string("regex", "darkred")` \cr
#' `r .mybadge_string("fixed", "darkgreen")` \cr
#' `r .mybadge_string("coll", "pink")` \cr
#' `r .mybadge_string("charclass", "lightyellow")` \cr
#' @param i an integer scalar,
#' or an integer vector of appropriate length
#' (vector recycling is not supported). \cr
#' Positive numbers count occurrences from the left/beginning of the strings. \cr
#' Negative numbers count occurrences from the right/end of the strings. \cr
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
#' \link[stringi]{stri_locate_all} or \link[stringi]{stri_locate_all_boundaries}. \cr
#' Do not supply the arguments
#' \code{omit_no_match} or \code{get_length},
#' as they are already specified internally.
#' Supplying these arguments anyway will result in an error.
#' @param opts_regex,opts_fixed,opts_collator,opts_brkiter
#' named list used to tune up the selected search engine's settings. \cr
#' see \link[stringi]{stri_opts_regex},
#' \link[stringi]{stri_opts_fixed},
#' \link[stringi]{stri_opts_collator},
#' and \link[stringi]{stri_opts_brkiter}. \cr
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
#' @note
#' \bold{Long Vectors} \cr
#' The \code{stri_locate_ith}-functions
#' do not support \code{long vectors}
#' (i.e. character vectors with more than \code{2^31 - 1} strings). \cr
#' \cr
#' \bold{Performance} \cr
#' The performance of `stri_locate_ith()` is about the same as that of \link[stringi]{stri_locate_all}. \cr \cr
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
#' @example inst/examples/stri_locate_ith.R
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
  
  p1 <- stringi::stri_locate_all_regex(
    str = str, pattern = pattern, capture_groups = FALSE,
    omit_no_match = FALSE, get_length = FALSE,
    ..., opts_regex = opts_regex
  )
  return(.stri_locate_ith_internal(p1, i, sys.call()))
}


#' @rdname stri_locate_ith
#' @export
stri_locate_ith_fixed <- function(str, pattern, i, ..., opts_fixed = NULL) {

  p1 <- stringi::stri_locate_all_fixed(
    str = str, pattern = pattern, omit_no_match = FALSE, get_length = FALSE,
    ..., opts_fixed = opts_fixed
  )
  return(.stri_locate_ith_internal(p1, i, sys.call()))
}


#' @rdname stri_locate_ith
#' @export
stri_locate_ith_coll <- function(str, pattern, i, ..., opts_collator = NULL) {
  
  p1 <- stringi::stri_locate_all_coll(
    str = str, pattern = pattern, omit_no_match = FALSE, get_length = FALSE,
    ..., opts_collator = opts_collator
  )
  return(.stri_locate_ith_internal(p1, i, sys.call()))
}


#' @rdname stri_locate_ith
#' @export
stri_locate_ith_charclass <- function(str, pattern, i, merge = TRUE, ...) {
  
  p1 <- stringi::stri_locate_all_charclass(
    str = str, pattern = pattern, merge = merge,
    omit_no_match = FALSE, get_length = FALSE,
    ...
  )
  return(.stri_locate_ith_internal(p1, i, sys.call()))
}


#' @rdname stri_locate_ith
#' @export
stri_locate_ith_boundaries <- function(
    str, i, ... , opts_brkiter = NULL
) {
  
  p1 <- stringi::stri_locate_all_boundaries(
    str = str,
    omit_no_match = FALSE, get_length = FALSE,
    ..., opts_brkiter = opts_brkiter
  )
  return(.stri_locate_ith_internal(p1, i, sys.call()))
  
}


#' @keywords internal
#' @noRd
.stri_locate_ith_internal <- function(p1, i, abortcall) {
  
  n <- length(p1)
  if(n == 0) {
    warning(simpleWarning("empty search patterns are not supported", call = abortcall))
    return(cbind(start = integer(0), end = integer(0)))
  }
  i <- as.integer(i)
  n.i <- length(i)
  if(n.i == 1L) {
    if(is.na(i) || i == 0L || is.infinite(i)) {
      stop("`i` is not allowed to be zero or NA")
    }
    mat <- .C_do_stri_locate_ith1(p1, i, c(n, 2L))
  }
  else if(n.i == n) {
    mat <- .C_do_stri_locate_ith0(p1, i, c(n, 2L))
  }
  else {
    stop(simpleError("recycling of vector `i` not allowed", call = abortcall))
  }
  
  colnames(mat) <- c("start", "end")
  
  return(mat)
}

