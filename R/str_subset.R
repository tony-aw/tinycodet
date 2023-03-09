#' String subsetting functions and operators
#'
#'@description
#' String subsetting functions. \cr
#' \cr
#' The \code{x %ss% s } operator
#' allows indexing a single string as-if it is an iterable object. \cr
#' \cr
#' The \code{x %sget% ss } operator
#' gives a certain number of the first and last characters of \code{x}. \cr
#' \cr
#' The \code{x %strim% ss } operator
#' removes a certain number of the first and last characters of \code{x}. \cr
#' \cr
#' The \code{s_repl_substr(x, rp, ...)} function
#' replaces a position (range) with string \code{rp}. \cr
#' \cr
#' The \code{s_chartr_substr(x, old, new, ...)} function
#' transforms the sub-string at a position range using \code{chartr(old, new)}. \cr
#' \cr
#' The \code{s_addin_substr(x, addin, side, ...)} function
#' adds the additional string \code{addin} at the side \code{side} of a position. \cr
#' \cr
#' The \code{s_extract_substr(x, type, ...)} function
#' extracts the string at, before, or after some position. \cr
#' \cr
#'
#' @param s a numeric vector giving the subset indices.
#' @param x a string or character vector.
#' See \code{\link{s_pattern}}.
#' @param ss a vector of length 2, or a matrix with 2 columns with \code{nrow(ss)==length(x)}.
#' The object \code{ss} should consist entirely of non-negative integers
#' (thus 0, 1, 2, etc. are valid, but -1, -2, -3 etc are not valid).
#' The first element/column of \code{ss}
#' gives the number of characters counting from the left side to be extracted/removed from \code{x}.
#' The second element/column of \code{ss}
#' gives the number of characters counting from the right side to be extracted/removed from \code{x}.
#' @param rp a string, or a character vector of the same length as \code{x},
#' giving the replacing strings.
#' @param loc an integer vector, giving the location/position indices
#' on the strings of character vector \code{x}.
#' @param loc the matrix result from the \link{s_locate_ith} function,
#' or a manually made 2-column integer matrix,
#' giving the start (first column) and stop (second column) position
#' of the range to be modified,  and with \code{nrow(loc)==length(x)}. \cr
#' NOTE: you cannot fill in both \code{loc} and \code{start,stop},
#' or both \code{loc} and \code{at}. Choose one or the other.
#' See \link{s_locate_ith}.
#' @param at an integer, or integer vector of the same length as \code{x}.
#' @param start,stop integers, or integer vectors of the same length as \code{x},
#' giving the start and stop position of the range to be modified.
#' @param old,new see \link[base]{chartr}.
#' Defaults to \code{old="a-zA-Z", new="A-Za-z"},
#' which means upper case characters will be transformed to lower case characters,
#' and vice-versa.
#' @param side which side of the position to add in the string.
#' Either \code{"before"} or \code{"after"}.
#' @param addin a string, or a character vector of the same length as \code{x},
#' giving the string(s) to add-in.
#' @param type the part of the string to extract. 3 options available: \cr
#'  * \code{type = "at"}: extracts the string part at the position range; \cr
#'  * \code{type = "before"}: extracts the string part before the position range; \cr
#'  * \code{type = "after"}: extracts the string part after the position range. \cr
#' @param fish although \code{tidyoperators} has zero-dependencies,
#' it does allow the internal functions to use the very fast \code{stringfish}
#' functions. To do so, set \code{fish=TRUE};
#' this requires \code{stringfish} to be installed.
#' @param ... only applicable if \code{fish=TRUE};
#' other arguments to be passed to the \code{stringfish} functions.
#'
#'
#' @details
#' These operators and functions serve as a way to provide straight-forward string subsetting,
#' is.null from base R. \cr
#'  \cr
#'
#'
#' @returns
#' The \code{%ss%} operator always returns a vector or matrix,
#' where each element is a single character. \cr
#' \cr
#' The \code{s_extract_ith()} and \code{s_repl_ith()} functions
#' return a character vector of the same length as \code{x}. \cr
#' \cr
#' The \code{s_locate_ith()} function
#' returns a numeric vector of the same length as \code{x}.
#'
#'
#' @examples
#'
#' # numerical substr ====
#'
#' x <- "12345678910"
#' start=1; stop=2
#' s_extract_substr(x, start=start, stop=stop)
#' s_extract_substr(x, type="before", start=start, stop=stop)
#' s_extract_substr(x, type="after", start=start, stop=stop)
#' s_repl_substr(x, "??", start=start, stop=stop)
#' s_chartr_substr(x, start=start, stop=stop)
#' s_addin_substr(x, " ", "after", at=stop)
#' s_addin_substr(x, " ", "before", at=start)
#'
#' start=10; stop=11
#' s_extract_substr(x, start=start, stop=stop)
#' s_extract_substr(x, type="before", start=start, stop=stop)
#' s_extract_substr(x, type="after", start=start, stop=stop)
#' s_repl_substr(x, "??", start=start, stop=stop)
#' s_chartr_substr(x, start=start, stop=stop)
#' s_addin_substr(x, " ", "after", at=stop)
#' s_addin_substr(x, " ", "before", at=start)
#'
#' start=5; stop=6
#' s_extract_substr(x, start=start, stop=stop)
#' s_extract_substr(x, type="before", start=start, stop=stop)
#' s_extract_substr(x, type="after", start=start, stop=stop)
#' s_repl_substr(x, "??", start=start, stop=stop)
#' s_chartr_substr(x, start=start, stop=stop)
#' s_addin_substr(x, " ", "after", at=stop)
#' s_addin_substr(x, " ", "before", at=start)
#'
#'
#' #############################################################################
#'
#' # simple pattern ====
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- rep("a|e|i|o|u",2)
#' loc <- s_locate_ith(x, c(-1, 1), p)
#' s_extract_substr(x, loc=loc)
#' s_extract_substr(x, type="before", loc=loc)
#' s_extract_substr(x, type="after", loc=loc)
#' s_repl_substr(x, "??", loc=loc)
#' s_chartr_substr(x, loc=loc)
#' s_addin_substr(x, " ", "after", loc=loc)
#' s_addin_substr(x, " ", "before", loc=loc)
#'
#' #############################################################################
#'
#' # ignore case pattern ====
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' # pattern with ignore.case=TRUE:
#' p <- s_pattern(regex = rep("A|E|I|O|U", 2), ignore.case=TRUE)
#' loc <- s_locate_ith(x, c(1, -1), p)
#' s_extract_substr(x, type="at", loc=loc)
#' s_extract_substr(x, type="before", loc=loc)
#' s_extract_substr(x, type="after", loc=loc)
#' s_repl_substr(x, "??", loc=loc)
#' s_chartr_substr(x, loc=loc)
#' s_addin_substr(x, " ", "after", loc=loc)
#' s_addin_substr(x, " ", "before", loc=loc)
#'
#' #############################################################################
#'
#' # multi-character pattern ====
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' # multi-character pattern:
#' p <- s_pattern(regex = rep("AB", 2), ignore.case=TRUE)
#' loc <- s_locate_ith(x, c(1,-1), p)
#' s_extract_substr(x, loc=loc)
#' s_extract_substr(x, type="before", loc=loc)
#' s_extract_substr(x, type="after", loc=loc)
#' s_repl_substr(x, "??", loc=loc)
#' s_chartr_substr(x, loc=loc)
#' s_addin_substr(x, " ", "after", loc=loc)
#' s_addin_substr(x, " ", "before", loc=loc)
#'

#' @rdname str_subset
#' @export
`%ss%` <- function(x, s){
  unlist(strsplit(x, split=""))[s]
}

#' @rdname str_subset
#' @export
`%sget%` <- function(x, ss) {
  ss <- matrix(ss, ncol=2)
  if(isTRUE(any(ss<0))){stop("ss cannot contain negative numbers")}
  n <- nchar(x)
  first <- substr(x, 1, ss[,1])
  last <- substr(x, n - ss[,2] + 1, n)
  out <- ifelse(rowSums(ss)>=n, x, paste(first, last, sep = ""))
  return(out)
}

#' @rdname str_subset
#' @export
`%strim%` <- function(x, ss) {
  ss <- matrix(ss, ncol=2)
  if(isTRUE(any(ss<0))){stop("ss cannot contain negative numbers")}
  n <- nchar(x)
  out <- ifelse(rowSums(ss)>=n, "", substr(x, 1+ss[,1], n-ss[,2]))
  return(out)
}

#' @rdname str_subset
#' @export
s_repl_substr <- function(
    x, rp, loc=NULL, start=NULL, stop=NULL, fish=FALSE, ...
) {
  check.args <- c(!is.null(loc), !is.null(start) & !is.null(stop))
  if(sum(check.args)!=1){
    stop("either loc OR start & stop must be filled in")
  }
  if(is.null(loc)){
    loc <- cbind(start, stop)
  }
  if(!fish){
    n <- nchar(x)
    prepart <- ifelse(loc[,1]<=1, "", substr(x, start=1, stop=loc[,1]-1))
    postpart <- ifelse(loc[,2]>=n, "", substr(x, start = loc[,2]+1, stop=n))
    mainpart <- rp
    out <- paste(prepart, mainpart, postpart, sep ="")
  }
  if(fish) {
    n <- stringfish::sf_nchar(x, ...)
    prepart <- ifelse(loc[,1]<=1, "", stringfish::sf_substr(x, start=1, stop=loc[,1]-1), ...)
    postpart <- ifelse(loc[,2]>=n, "", stringfish::sf_substr(x, start = loc[,2]+1, stop=n), ...)
    mainpart <- rp
    out <- paste(prepart, mainpart, postpart, sep = "")
  }

  if(sum(stats::complete.cases(loc))>0){
    out[!stats::complete.cases(loc)] <- NA
  }

  return(out)
}

#' @rdname str_subset
#' @export
s_chartr_substr <- function(
    x, old="a-zA-Z", new="A-Za-z", loc=NULL, start=NULL, stop=NULL, fish=FALSE, ...
) {
  check.args <- c(!is.null(loc), !is.null(start) & !is.null(stop))
  if(sum(check.args)!=1){
    stop("either loc OR start & stop must be filled in")
  }
  if(is.null(loc)){
    loc <- cbind(start, stop)
  }
  if(!fish) {
    n <- nchar(x)
    prepart <- ifelse(loc[,1]<=1, "", substr(x, start=1, stop=loc[,1]-1))
    postpart <- ifelse(loc[,2]>=n, "", substr(x, start = loc[,2]+1, stop=n))
    mainpart <- chartr(old=old, new=new, substr(x, loc[,1], loc[,2]))
    out <- paste(prepart, mainpart, postpart, sep ="")
  }
  if(fish) {
    n <- stringfish::sf_nchar(x, ...)
    prepart <- ifelse(loc[,1]<=1, "", stringfish::sf_substr(x, start=1, stop=loc[,1]-1), ...)
    postpart <- ifelse(loc[,2] >=n, "", stringfish::sf_substr(x, start = loc[,2]+1, stop=n), ...)
    mainpart <- chartr(old=old, new=new, stringfish::sf_substr(x, loc[,1], loc[,2]), ...)
    out <- paste(prepart, mainpart, postpart, sep = "")
  }

  if(sum(stats::complete.cases(loc))>0){
    out[!stats::complete.cases(loc)] <- NA
  }

  return(out)
}

#' @rdname str_subset
#' @export
s_addin_substr <- function(
    x, addin, side="after", loc=NULL, at=NULL, fish=FALSE, ...
) {
  check.args <- c(!is.null(loc), !is.null(at))
  if(sum(check.args)!=1) {
    stop("either loc OR at must be filled in")
  }
  if(is.null(loc)) loc <- cbind(at, at)
  if(side=="after"){
    prepart.stop <- loc[,1]
    postpart.start <- loc[,2] + 1
  }
  if(side=="before"){
    prepart.stop <- loc[,1] - 1
    postpart.start <- loc[,2]
  }
  if(!fish) {
    n <- nchar(x)
    prepart <- ifelse(loc[,1]<=1, "", substr(x, start=1, stop=prepart.stop))
    postpart <- ifelse(loc[,2] >=n, "", substr(x, start = postpart.start, stop=n))
    mainpart <- addin
    out <- paste(prepart, addin, postpart, sep ="")
  }
  if(fish) {
    n <- stringfish::sf_nchar(x, ...)
    prepart <- ifelse(loc[,1]<=1, "", stringfish::sf_substr(x, start=1, stop=prepart.stop, ...))
    postpart <- ifelse(loc[,2] >=n, "", stringfish::sf_substr(x, start = postpart.start, stop=n, ...))
    out <- paste(prepart, addin, postpart, sep = "")
  }

  if(sum(stats::complete.cases(loc))>0){
    out[!stats::complete.cases(loc)] <- NA
  }

  return(out)
}

#' @rdname str_subset
#' @export
s_extract_substr <- function(
    x, type="at",loc=NULL, start=NULL, stop=NULL, fish=FALSE, ...
) {
  check.args <- c(!is.null(loc), !is.null(start) & !is.null(stop))
  if(sum(check.args)!=1){
    stop("either loc OR start & stop must be filled in")
  }
  if(is.null(loc)){
    loc <- cbind(start, stop)
  }
  if(!fish) {
    n <- nchar(x, ...)
    out <- switch(
      type,
      "at" = substr(x, start=loc[,1], stop=loc[,2]),
      "before" = ifelse(loc[,1]<=1, "", substr(x, start=1, stop=loc[,1]-1, ...)),
      "after" = ifelse(loc[,2]>=n, "", substr(x, start = loc[,2]+1, stop=n, ...))
    )
  }
  if(fish) {
    n <- stringfish::sf_nchar(x, ...)
    out <- switch(
      type,
      "at" =  stringfish::sf_substr(x, start=loc[,1], stop=loc[,2], ...),
      "before" = ifelse(loc[,1]<=1, "",  stringfish::sf_substr(x, start=1, stop=loc[,1]-1, ...)),
      "after" = ifelse(loc[,2]>=n, "",  stringfish::sf_substr(x, start = loc[,2]+1, stop=n, ...))
    )
  }

  if(sum(stats::complete.cases(loc))>0){
    out[!stats::complete.cases(loc)] <- NA
  }

  return(out)
}
