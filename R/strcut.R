#' Cut strings
#'
#' @description
#'
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
#' The main difference between the \code{strcut_} - functions
#' and \link[stringi]{stri_split} /  \link[base]{strsplit},
#' is that the latter generally removes the delimiter patterns in a string when cutting,
#' while the \code{strcut_}-functions do not attempt to remove parts of the string by default,
#' they only attempt to cut the strings into separate pieces.
#' Moreover, the \code{strcut_} - functions always return a matrix, not a list.
#'
#'
#' @param str a string or character vector.
#' @param loc Either one of the following:
#'  * the result from the \link{stri_locate_ith} function.
#'  * a matrix of 2 integer columns, with \code{nrow(loc)==length(str)},
#'  giving the location range of the middle part.
#'  * a vector of length 2, giving the location range of the middle part.
#' @param fill_loc logical, indicating what should be done if
#' for some row \code{i},
#' \code{loc[i, ]} is \code{c(NA, NA)}. \cr
#'  * If \code{TRUE}, \code{c(NA, NA)} in \code{loc[i, ]}
#'  is translated to \code{c(1, nc[i])},
#' where \code{nc[i]} is the number of characters of \code{str[i]}
#'  * If \code{FALSE}, \code{strcut_loc()} will return \code{c(NA, NA, NA)}
#'  for when \code{loc[i,]} is \code{c(NA, NA)}.
#' @param brk a single string, giving one of the following:
#'
#'  * \code{"chr"} or \code{"character"}: attempts to split string into individual characters.
#'  * \code{"line"} or \code{"line_break"}: attempts to split string into individual lines
#'  (NOTE: this is somewhat locale dependent).
#'  * \code{"word"}: attempts to split string into individual words
#'  (NOTE: this is highly locale dependent!).
#'  * \code{"sentence"}: attempts to split string into individual sentences
#'  (NOTE: this is highly locale dependent!).
#'
#'  For information on the boundary rules and definitions, please see: \cr
#'  The ICU User Guide on Boundary Analysis \cr
#'  (\url{https://unicode-org.github.io/icu/userguide/boundaryanalysis/})
#' @param ... additional settings for \link[stringi]{stri_opts_brkiter}
#'
#'
#'
#' @returns
#' For the \code{strcut_loc()} function: \cr
#' A character matrix with \code{length(str)} rows and 3 columns:
#'
#'  * the first column contains the sub-strings \bold{before} \code{loc};
#'  * the second column contains the sub_strings at \code{loc};
#'  * the third and last column contains the sub-strings \bold{after} \code{loc}. \cr
#'  \cr
#'
#' For the \code{strcut_brk()} function: \cr
#' A character matrix with \code{length(str)} rows and
#' a number of columns equal to the maximum number of pieces \code{str} was cut in.
#'
#'
#' @seealso [tinycodet_strings()]
#'
#'
#' @examples
#'
#'
#' x <- rep(paste0(1:10, collapse=""), 10)
#' print(x)
#' loc <- stri_locate_ith(x, 1:10, fixed = as.character(1:10))
#' strcut_loc(x, loc)
#' strcut_loc(x, c(5,5))
#' strcut_loc(x, c(NA, NA), fill_loc = TRUE)
#' strcut_loc(x, c(NA, NA), fill_loc = FALSE)
#'
#' test <- "The\u00a0above-mentioned    features are very useful. " %s+%
#' "Spam, spam, eggs, bacon, and spam. 123 456 789"
#' strcut_brk(test, "line")
#' strcut_brk(test, "word")
#' strcut_brk(test, "sentence")
#' strcut_brk(test, "chr")


#' @rdname strcut
#' @export
strcut_loc <- function(str, loc, fill_loc = TRUE) {
  # Error handling:
  if(!isFALSE(fill_loc) && !isTRUE(fill_loc)) {
    stop("`fill_loc` must be either `TRUE` or `FALSE`")
  }
  loc <- matrix(loc, ncol=2)
  cc_str <- !is.na(str)
  nstr <- length(str)
  nloc <- nrow(loc)
  if(nrow(loc)==1) {
    loc <- loc[rep(1, nstr), , drop=FALSE]
  }
  nloc <- nrow(loc)
  if(nloc != nstr) {
    stop("`nrow(loc)` must equal to `length(str)` or 1")
  }
  if(all(!cc_str)) {
    repNA <- rep(NA, nstr)
    out <- cbind(prepart = repNA, mainpart=repNA, postpart=repNA)
    return(out)
  }
  if(!is.character(str)){
    stop("`str` must be a character vector")
  }


  # FUNCTION:
  x <- str[cc_str]
  cc_loc <- stats::complete.cases(loc)
  loc <- loc[cc_str, , drop=FALSE] # new
  cc <- stats::complete.cases(loc)

  nx <- length(x)
  nc <- stringi::stri_length(x)
  loc <- .substr_loc(loc, cc_str, cc, nx, nc, abortcall = sys.call())

  prepart <- mainpart <- postpart <- character(nstr) # not nx
  prepart[cc_str] <- .substr_prepart(x, loc, nx)
  postpart[cc_str] <- .substr_postpart(x, loc, nx, nc)
  mainpart[cc_str] <- stringi::stri_sub(
    x, from = loc[, 1], to = pmin(loc[, 2], nc)
  )
  out <- cbind(prepart, mainpart, postpart)
  out[!cc_str, ] <- c(NA, NA, NA)
  if(!fill_loc && any(!cc_loc)) {
    out[!cc_loc, ] <- c(NA, NA, NA)
  }

  return(out)
}



#' @rdname strcut
#' @export
strcut_brk <- function(str, brk="chr", ...) {
  if(length(brk) > 1) {
    stop("`brk` must be a single string")
  }
  lst <- list(...)
  if(any(names(lst) %in% c("n", "tokens_only", "simplify"))) {
    stop("arguments `n`, `tokens_only` and `simplify` not supported in this function;",
         "\n",
         "use `stringi::stri_split_boundaries()` instead")
  }
  if(brk == "chr") brk <- "character"
  if(brk == "line") brk <- "line_break"

  out <- stringi::stri_split_boundaries(
    str=str, type=brk, n=-1L, tokens_only = FALSE, simplify = TRUE, ...
  )
  return(out)
}

#' @keywords internal
#' @noRd
.substr_loc <- function(loc, cc_str, cc, nx, nc, abortcall) {
  loc[!cc, 1] <- 1
  loc[!cc, 2] <- nc[!cc] # added [!cc]
  # loc <- loc[cc_str, , drop=FALSE]
  if(any(loc[cc] <= 0)) {
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
  if(sum(!ind ) >0) {
    out[!ind] <- stringi::stri_sub(
      x[!ind], from = 1, to = loc[!ind ,1] - 1
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
  if(sum(!ind) > 0) {
    out[!ind] <- stringi::stri_sub(
      x[!ind], from = loc[!ind, 2] + 1, to = nc[!ind]
    )
  }
  return(out)
}
