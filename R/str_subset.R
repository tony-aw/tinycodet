#' String subsetting
#'
#'@description
#' String subsetting operators and functions. \cr
#' \cr
#' The \code{x %ss% s } operator allows indexing a single string as-if it is an iterable object. \cr
#' \cr
#' The \code{x %sget% ss } operator gives a certain number of the first and last characters of \code{x}. \cr
#' \cr
#' The \code{x %sgl% p } operator is equivalent to \code{grepl(pattern=p, x=x)}. \cr
#' \cr
#' The \code{x %sg=v%}
#'
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
#' See \code{\link{s_pattern_b}}.
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
#' p <- "a|e|i|o|u" # same as p <- s_pattern_b("a|e|i|o|u", fixed=FALSE, ignore.case=FALSE, perl=FALSE)
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
#' p <- s_pattern_b("A|E|I|O|U", fixed=FALSE, ignore.case=TRUE, perl=FALSE)
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
#' p <- s_pattern_b("\\v+", perl=TRUE) # perl expression; only works with perl=TRUE
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
  if(isTRUE(attr(p, "engine")=="base") | is.null(attr(p, "engine"))){
    p_attr <- s_get_pattern_attr_internal(p)
    if(is.null(custom_mapply)){
      return(mapply(
        s_extract_internal, x, i, p,
        MoreArgs=list(fixed = p_attr$fxd, ignore.case = p_attr$ic, perl = p_attr$prl, useBytes = p_attr$ub),
        USE.NAMES = FALSE, SIMPLIFY = TRUE
      ))
    }
    if(!is.null(custom_mapply)) {
      return(custom_mapply(
        s_extract_internal, x, i, p,
        MoreArgs=list(fixed = p_attr$fxd, ignore.case = p_attr$ic, perl = p_attr$prl, useBytes = p_attr$ub)
      ) |> unname())
    }
  }
  if(isTRUE(attr(p, "engine")=="stringi")){
    if(length(i)==1) i <- rep(i, length(x))
    if(length(i)!=length(x)){
      stop("i must be of length 1, or the same length as x")
    }
    p1 <- do.call(stringi::stri_locate_all, c(list(str=x), p))
    n <- sapply(p1, nrow)
    i[i<0] <- pmax(n - abs(i+1), 1)
    i[i>0] <- pmin(i, n)

    if(is.null(custom_mapply)) {
      p2 <- mapply(function(x, i)x[i,], x=p1, i=i, SIMPLIFY = FALSE)
    }
    if(!is.null(custom_mapply)){
      p2 <- custom_mapply(function(x, i)x[i,], x=p1, i=i, SIMPLIFY = FALSE)
    }
    p3 <- do.call(rbind, p2)
    x <- stringi::stri_sub(x, p3[,1], p3[,2], use_matrix = FALSE)
    return(x)
  }
}

#' @rdname str_subset
#' @export
s_repl <- function(x, i, p, rp, custom_mapply=NULL) {
  if(isTRUE(attr(p, "engine")=="base") | is.null(attr(p, "engine"))){
    p_attr <- s_get_pattern_attr_internal(p)
    if(is.null(custom_mapply)){
      return(mapply(
        s_repl_internal, x, i, p, rp,
        MoreArgs=list(fixed = p_attr$fxd, ignore.case = p_attr$ic, perl = p_attr$prl, useBytes = p_attr$ub),
        USE.NAMES = FALSE, SIMPLIFY = TRUE
      ))
    }
    if(!is.null(custom_mapply)) {
      return(custom_mapply(
        s_repl_internal, x, i, p, rp,
        MoreArgs=list(fixed = p_attr$fxd, ignore.case = p_attr$ic, perl = p_attr$prl, useBytes = p_attr$ub)
      ) |> unname())
    }
  }
  if(isTRUE(attr(p, "engine")=="stringi")){
    if(length(i)==1) i <- rep(i, length(x))
    if(length(rp)==1) rp <- rep(rp, length(x))
    if(length(i)!=length(x) | length(rp)!=length(x)){
      stop("i and/or rp must be of length 1, or the same length as x")
    }
    p1 <- do.call(stringi::stri_locate_all, c(list(str=x), p))
    n <- sapply(p1, nrow)
    i[i<0] <- pmax(n - abs(i+1), 1)
    i[i>0] <- pmin(i, n)

    if(is.null(custom_mapply)) {
      p2 <- mapply(function(x, i)x[i,], x=p1, i=i, SIMPLIFY = FALSE)
    }
    if(!is.null(custom_mapply)){
      p2 <- custom_mapply(function(x, i)x[i,], x=p1, i=i, SIMPLIFY = FALSE)
    }
    p3 <- do.call(rbind, p2)
    stringi::stri_sub(x, p3[,1], p3[,2], use_matrix = FALSE, omit_na=TRUE) <- rp
    return(x)
  }
}




