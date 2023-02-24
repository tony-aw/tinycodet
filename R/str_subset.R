#' String subsetting
#'
#'@description
#' String subsetting operators and functions. \cr
#' \cr
#' The \code{x %ss% s } operator allows indexing a single string as-if it is an iterable object. \cr
#' \cr
#' The \code{x %sget% ss } operator gives a certain number of the first and last characters of \code{x}. \cr
#' \cr
#' The \code{x %strim% ss } operator removes a certain number of the first and last characters of \code{x}. \cr
#' \cr
#' The \code{s_extract(x, i, p, custom_mapply=NULL)} function extracts the ith occurrence of character/pattern \code{p}. \cr
#' \cr
#' The \code{s_repl(x, i, p, rp, custom_mapply=NULL)} function replaces the ith occurence of character/pattern \code{p} with \code{rp}. \cr
#' \cr
#'
#' @param s a numeric vector giving the subset indices.
#' @param x a string or character vector.
#' @param p a pattern (regular expression), or character vector of regular expressions of the same length as \code{x},
#' giving the pattern to look for. \cr
#' The default interpretation is a regular expression, using: \cr
#' \code{fixed=FALSE, ignore.case=FALSE, perl=FALSE} (see \link[base]{grep}).\cr
#' To evaluate pattern \code{p} with non-default options, simply replace \code{p} with \code{s_pattern(p, ...)}. \cr
#' See \code{\link{s_pattern}}.
#' @param ss a vector of length 2, or a matrix with 2 columns with \code{nrow(ss)==length(x)}.
#' ss should consist entirely of non-negative integers (thus 0, 1, 2, etc. are valid, but -1, -2, -3 etc are not valid).
#' The first element/column of \code{ss}
#' gives the number of characters counting from the left side to be extracted/removed from \code{x}.
#' The second element/column of \code{ss}
#' gives the number of characters counting from the right side to be extracted/removed from \code{x}.
#' @param i a number, or a numeric vector of the same length as \code{x}.
#' This gives the \eqn{i^th} instance to be replaced. \cr
#' Positive numbers are counting from the left. Negative numbers are counting from the right. \cr
#' Thus \code{s_repl(x, i=1, p, rp)} will replace the first instance of \code{p} with \code{rp},
#' and \code{s_repl(x, i=-1, p, rp)} will replace the last instance of \code{p} with \code{rp}.
#' And \code{s_repl(x, i=2, p, rp)} will replace the second instance of \code{p} with \code{rp},
#' and \code{s_repl(x, i=-2, p, rp)} will replace the second-last instance of \code{p} with \code{rp}, etc. \cr
#' If i is larger than the number of instances, the maximum instance will be given. \cr
#' For example: suppose a string has 3 instances of p;
#' then if i=4 the third instance will be replaced, and if i=-3 the first instance will be replaced.
#' @param rp a string, or a character vector of the same length as \code{x}, giving the character(s) to replace p with.
#' @param custom_mapply the \code{s_extract()} and \code{s_repl()} functions,
#' internally use \code{mapply()}. The user may choose to replace this with a custom functions,
#' for example for multi-threading purposes. The replacing function must have the same argument convention
#' as \code{mapply}. \cr
#' For example:\cr
#' s_extract(..., custom_mapply=future_mapply)
#'
#'
#' @details
#' These operators and functions serve as a way to provide straight-forward string subsetting,
#' missing from base R. \cr
#'  \cr
#'
#' The pattern \code{p} is by default understood to be characters or regular expressions evaluated with the default arguments
#' (\code{fixed=FALSE, ignore.case=FALSE, perl=FALSE}). \cr
#' To change this, simply replace \code{p} with \code{s_pattern(p, fixed, ignore.case, perl)}. \cr
#' For example, \code{x %s/% p } counts how often regular expression p occurs in x, \cr
#' whereas \code{x %s/% s_pattern(p, ignore.case=TRUE) } will do the same except it does not distinguish between capital/upper and lower characters.\cr
#' See \code{\link{s_pattern}} for more information.
#'
#'
#' @returns
#' The \code{%ss%} operator always returns a vector or matrix, where each element is a single character. \cr
#' \cr
#' The \code{s_extract()} and \code{s_repl()} functions return a character vector of the same length as \code{x}. \cr
#' \cr
#'
#'
#' @examples
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- "a|e|i|o|u" # same as p <- s_pattern("a|e|i|o|u", fixed=FALSE, ignore.case=FALSE, perl=FALSE)
#' ss <- c(2,2)
#'
#' x %ss% 3:4 # same as unlist(strsplit(x, split=""))[3:4]
#' s_extract(x, -1, p) # extracts the last vowel in each element of x.
#' s_repl(x, -1, p, "?") # replace last vowel in each element of x with a question mark ("?").
#'
#' x %sget% ss
#' x %strim% ss
#'
#'
#' #############################################################################
#'
#'
#' #' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' # pattern with ignore.case=TRUE:
#' p <- s_pattern("A|E|I|O|U", fixed=FALSE, ignore.case=TRUE, perl=FALSE)
#' fun <- function(x)sort(x, decreasing=TRUE)
#' ss <- c(2,2)
#'
#' x %ss% 3:4 # same as unlist(strsplit(x, split=""))[y]
#' s_extract(x, -1, p) # extracts the last vowel in each element of x.
#' s_repl(x, -1, p, "?") # replace last vowel in each element of x with a question mark ("?").
#'
#' x %sget% ss
#' x %strim% ss
#'
#'
#' #############################################################################
#'
#'
#' p <- s_pattern("\\v+", perl=TRUE) # perl expression; only works with perl=TRUE
#' x <- "line1 \n line2"
#' print(x)
#' s_repl(x, 1, p, " - ") # replace vertical line break with a minus line.
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
  first <- substr(x, 1, pmax(ss[,1], 0))
  last <- substr(x, nchar(x) - pmax(ss[,2], 0) + 1, nchar(x))
  return(paste(first, last, sep = ""))
}

#' @rdname str_subset
#' @export
`%strim%` <- function(x, ss) {
  ss <- matrix(ss, ncol=2)
  return(substr(x, 1+pmax(ss[,1], 0), nchar(x)-pmax(ss[,2], 0)))
}


#' @rdname str_subset
#' @export
s_extract <- function(x, i, p, custom_mapply=NULL) {
  fxd <- ic <- prl <- FALSE
  if(!is.null(attr(p, "fixed"))){fxd <- attr(p, "fixed")}
  if(!is.null(attr(p, "ignore.case"))){ic <- attr(p, "ignore.case")}
  if(!is.null(attr(p, "perl"))){prl <- attr(p, "perl")}

  if(is.null(custom_mapply)){
    return(mapply(s_extract_internal, x, i, p, MoreArgs=list(fixed=fxd, ignore.case=ic, perl=prl), USE.NAMES = FALSE))
  }
  if(!is.null(custom_mapply)) {
    return(custom_mapply(s_extract_internal, x, i, p, MoreArgs=list(fixed=fxd, ignore.case=ic, perl=prl)) |> unname())
  }
}

#' @rdname str_subset
#' @export
s_repl <- function(x, i, p, rp, custom_mapply=NULL) {
  fxd <- ic <- prl <- FALSE
  if(!is.null(attr(p, "fixed"))){fxd <- attr(p, "fixed")}
  if(!is.null(attr(p, "ignore.case"))){ic <- attr(p, "ignore.case")}
  if(!is.null(attr(p, "perl"))){prl <- attr(p, "perl")}

  if(is.null(custom_mapply)){
    return(mapply(s_repl_internal, x, i, p, rp, MoreArgs=list(fixed=fxd, ignore.case=ic, perl=prl), USE.NAMES = FALSE))
  }
  if(!is.null(custom_mapply)) {
    return(custom_mapply(s_repl_internal, x, i, p, rp, MoreArgs=list(fixed=fxd, ignore.case=ic, perl=prl)) |> unname())
  }
}



