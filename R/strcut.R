#' Cut Strings
#'
#' @description
#'
#' The \code{strcut_loc()} function
#' cuts every string in a character vector around a location range \code{loc},
#' such that every string is cut into the following parts:
#'
#'  * the sub-string \bold{before} \code{loc};
#'  * the sub-string at \code{loc} itself;
#'  * the sub-string \bold{after} \code{loc}.
#'
#' The location range \code{loc} would usually be matrix with 2 columns,
#' giving the start and end points of some pattern match. \cr
#' \cr
#' The \code{strcut_brk()} function
#' (a wrapper around \link[stringi]{stri_split_boundaries})
#' cuts every string into individual text breaks
#' (like character, word, line, or sentence boundaries). \cr
#' \cr
#'
#'
#' @param str a string or character vector.
#' @param loc Either one of the following:
#'  * the result from the \link{stri_locate_ith} function.
#'  * a matrix of 2 integer columns, with \code{nrow(loc)==length(str)},
#'  giving the location range of the middle part.
#'  * a vector of length 2, giving the location range of the middle part.
#' @param type single string;
#' either the break iterator type,
#' one of \code{character}, \code{line_break}, \code{sentence}, \code{word},
#' or a custom set of ICU break iteration rules. Defaults to \code{"character"}. \cr
#' `r .mybadge_string("boundaries", "blue")` \cr
#'
#' @param ... additional settings for \link[stringi]{stri_opts_brkiter}
#'
#' @details
#' The main difference between the \code{strcut_} - functions
#' and \link[stringi]{stri_split} /  \link[base]{strsplit},
#' is that the latter generally removes the delimiter patterns in a string when cutting,
#' while the \code{strcut_}-functions do not attempt to remove parts of the string by default,
#' they only attempt to cut the strings into separate pieces.
#' Moreover, the \code{strcut_} - functions always return a matrix, not a list. \cr
#' \cr
#'
#'
#' @returns
#' For the \code{strcut_loc()} function: \cr
#' A character matrix with \code{length(str)} rows and 3 columns:
#'
#'  * the first column contains the sub-strings \bold{before} \code{loc},
#'  or \code{NA} if \code{loc} is \code{c(NA, NA)};
#'  * the second column contains the sub_strings at \code{loc},
#'  or the uncut string if \code{loc} is \code{c(NA, NA)};
#'  * the third and last column contains the sub-strings \bold{after} \code{loc},
#'  or \code{NA} if \code{loc} is \code{c(NA, NA)}. \cr
#'  \cr
#'
#' For the \code{strcut_brk()} function: \cr
#' A character matrix with \code{length(str)} rows and
#' a number of columns equal to the maximum number of pieces \code{str} was cut in. \cr
#' Empty places are filled with \code{NA}.
#'
#'
#'
#' @seealso [tinycodet_strings()]
#'
#'
#' @examples
#'
#' x <- rep(paste0(1:10, collapse=""), 10)
#' print(x)
#' loc <- stri_locate_ith(x, 1:10, fixed = as.character(1:10))
#' strcut_loc(x, loc)
#' strcut_loc(x, c(5,5))
#' strcut_loc(x, c(NA, NA))
#'
#' test <- "The\u00a0above-mentioned    features are very useful. " %s+%
#' "Spam, spam, eggs, bacon, and spam. 123 456 789"
#' strcut_brk(test, "line")
#' strcut_brk(test, "word")
#' strcut_brk(test, "sentence")
#' strcut_brk(test)


#' @rdname strcut
#' @export
strcut_loc <- function(str, loc) {
  # Error handling:
  loc <- matrix(loc, ncol=2)
  cc <- !is.na(str) & stats::complete.cases(loc)
  nstr <- length(str)
  nloc <- nrow(loc)
  if(nrow(loc)==1) {
    loc <- loc[rep(1, nstr), , drop=FALSE]
  }
  nloc <- nrow(loc)
  if(nloc != nstr) {
    stop("`nrow(loc)` must equal to `length(str)` or 1")
  }
  if(all(!cc)) {
    repNA <- rep(NA, nstr)
    out <- cbind(prepart = repNA, mainpart = str, postpart = repNA)
    return(out)
  }
  if(!is.character(str)){
    stop("`str` must be a character vector")
  }

  # FUNCTION:
  x <- str[cc]
  loc <- loc[cc, , drop=FALSE] # new

  nx <- length(x)
  nc <- stringi::stri_length(x)
  loc <- .check_loc(loc, cc, abortcall = sys.call())

  prepart <- mainpart <- postpart <- character(nstr) # not nx
  prepart[cc] <- .substr_prepart(x, loc, nx)
  postpart[cc] <- .substr_postpart(x, loc, nx, nc)
  mainpart[cc] <- stringi::stri_sub(
    x, from = loc[, 1], to = pmin(loc[, 2], nc)
  )
  out <- cbind(prepart, mainpart, postpart)
  out[!cc, ] <- c(NA, str[!cc], NA)

  return(out)
}



#' @rdname strcut
#' @export
strcut_brk <- function(str, type = "character", ...) {
  if(length(type) > 1) {
    stop("`type` must be a single string")
  }
  out <- stringi::stri_split_boundaries(
    str=str, type = type, n=-1L, tokens_only = FALSE, simplify = NA, ...
  )
  return(out)
}

#' @keywords internal
#' @noRd
.check_loc <- function(loc, cc, abortcall) {
  if(any(loc[cc] < 1)) {
    stop(simpleError("`loc` can only have strictly positive numbers", call = abortcall))
  }
  if(any(loc[,2] < loc[,1])) {
    stop(simpleError("`loc[, 2] < loc[, 1]`", call = abortcall))
  }
  return(loc)
}

#' @keywords internal
#' @noRd
.substr_prepart <- function(x, loc, nx) {
  out <- character(nx)
  ind <- loc[, 1] == 1
  out[ind] <- ""
  ind2 <- which(!ind)
  if(length(ind2) > 0) {
    out[ind2] <- stringi::stri_sub(
      x[ind2], from = 1, to = loc[ind2 ,1] - 1
    )
  }
  return(out)
}

#' @keywords internal
#' @noRd
.substr_postpart <- function(x, loc, nx, nc) {
  out <- character(nx)
  ind <- loc[,2] >= nc
  out[ind] <- ""
  ind2 <- which(!ind)
  if(length(ind2) > 0) {
    out[ind2] <- stringi::stri_sub(
      x[ind2], from = loc[ind2, 2] + 1, to = nc[ind2]
    )
  }
  return(out)
}
