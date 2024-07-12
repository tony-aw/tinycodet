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
#' (a wrapper around \link[stringi]{stri_split_boundaries}\code{(..., tokens_only = FALSE)})
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
#' @param type either one of the following:
#'  - a single string giving the break iterator type
#'  (i.e. \code{"character"}, \code{"line_break"}, \code{"sentence"}, \code{"word"},
#'  or a custom set of ICU break iteration rules).
#'  - a list with break iteration options,
#'  like a list produced by \link[stringi]{stri_opts_brkiter}.
#' 
#' `r .mybadge_string("boundaries", "blue")` \cr
#' 
#' @param tolist logical, indicating if \code{strcut_brk} should return a list (\code{TRUE}),
#' or a matrix (\code{FALSE}, default).
#' @param n see \link[stringi]{stri_split_boundaries}.
#' @param ... additional arguments to be passed to \link[stringi]{stri_split_boundaries}.
#'
#' @details
#' The `strcut_` functions provide a short and concise way to cut strings into pieces,
#' without removing the delimiters,
#' which is an operation that lies at the core of virtually all boundaries-operations in 'stringi'. \cr
#' \cr
#' The main difference between the \code{strcut_} - functions
#' and \link[stringi]{stri_split} /  \link[base]{strsplit},
#' is that the latter generally removes the delimiter patterns in a string when cutting,
#' while the \code{strcut_}-functions do not attempt to remove parts of the string by default,
#' they only attempt to cut the strings into separate pieces.
#' Moreover, the \code{strcut_} - functions return a matrix by default. \cr
#' \cr
#'
#'
#' @returns
#' For \code{strcut_loc()}: \cr
#' A character matrix with \code{length(str)} rows and 3 columns,
#' where for every row \code{i} it holds the following:
#'
#'  * the first column contains the sub-string \bold{before} \code{loc[i,]},
#'  or \code{NA} if \code{loc[i,]} contains \code{NA};
#'  * the second column contains the sub_string at \code{loc[i,]},
#'  or the uncut string if \code{loc[i,]} contains \code{NA};
#'  * the third and last column contains the sub-string \bold{after} \code{loc[i,]},
#'  or \code{NA} if \code{loc[i,]} contains \code{NA}. \cr \cr
#'
#' For \code{strcut_brk(..., tolist = FALSE)}: \cr
#' A character matrix with \code{length(str)} rows and
#' a number of columns equal to the maximum number of pieces \code{str} was cut in. \cr
#' Empty places are filled with \code{NA}. \cr
#' \cr
#' For \code{strcut_brk(..., tolist = TRUE)}: \cr
#' A list with \code{length(str)} elements,
#' where each element is a character vector containing the cut string. \cr
#' \cr
#'
#'
#'
#' @seealso \link{tinycodet_strings}
#'
#'
#' @example inst/examples/strcut.R


#' @rdname strcut
#' @export
strcut_loc <- function(str, loc) {
  # Error handling:
  
  loc <- .check_loc1(loc, sys.call())
  
  cc <- !is.na(str) & stats::complete.cases(loc)
  nstr <- length(str)
  nloc <- nrow(loc)
  if(nrow(loc) == 1) {
    loc <- loc[rep_len(1, nstr), , drop=FALSE]
  }
  nloc <- nrow(loc)
  if(nloc != nstr) {
    stop("`loc` has wrong length or dimensions")
  }
  if(all(!cc)) {
    repNA <- rep_len(NA, nstr)
    out <- cbind(prepart = repNA, mainpart = str, postpart = repNA)
    return(out)
  }
  if(!is.character(str)){ # placing this later in case all(is.na(str))
    stop("`str` must be a character vector")
  }

  # FUNCTION:
  x <- str[cc]
  loc <- loc[cc, , drop = FALSE] # new
  .check_loc2(loc, sys.call())

  nx <- length(x)
  nc <- stringi::stri_length(x)

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
strcut_brk <- function(str, type = "character", tolist = FALSE, n = -1L, ...) {
  
  if(isFALSE(tolist)) {
    if(is.list(type)) {
      return(stringi::stri_split_boundaries(
        str = str, simplify = NA, tokens_only = FALSE, n = n, ..., opts_brkiter = type
      ))
    }
    else if(is.character(type)) {
      return(stringi::stri_split_boundaries(
        str = str, type = type, simplify = NA, tokens_only = FALSE, n = n, ...
      ))
    }
  }
  if(isTRUE(tolist)) {
    if(is.list(type)) {
      return(stringi::stri_split_boundaries(
        str = str, simplify = FALSE, tokens_only = FALSE, n = n, ..., opts_brkiter = type
      ))
    }
    else if(is.character(type)) {
      return(stringi::stri_split_boundaries(
        str = str, type = type, simplify = FALSE, tokens_only = FALSE, n = n, ...
      ))
    }
  }
  stop("`tolist` must be either `TRUE` or `FALSE`")
}


#' @keywords internal
#' @noRd
.check_loc1 <- function(loc, abortcall) {
  if(!is.matrix(loc)) {
    if(length(loc) == 2) {
      loc <- matrix(loc, ncol = 2)
    }
    else {stop(simpleError(
      "`loc` has wrong length or dimensions", call = abortcall
    ))}
  }
  if(ncol(loc) != 2) {
    stop(simpleError(
      "`loc` has wrong length or dimensions", call = abortcall
    ))
  }
  
  return(loc)
}


#' @keywords internal
#' @noRd
.check_loc2 <- function(loc, abortcall) {
  
  # Note: when `loc` is passed here, NAs have already been removed
  # Therefore, no need to check for NAs
  
  if(.C_any_nonpos(as.integer(loc))) {
    stop(simpleError("`loc` can only have strictly positive numbers", call = abortcall))
  }
  if(.C_any_badloc(as.integer(loc[,1]), as.integer(loc[,2]))) {
    stop(simpleError("`loc[, 2] < loc[, 1]`", call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.substr_prepart <- function(x, loc, nx) {
  out <- character(nx)
  ind <- loc[, 1] == 1L
  out[ind] <- ""
  ind2 <- which(!ind)
  if(length(ind2) > 0L) {
    out[ind2] <- stringi::stri_sub(
      x[ind2], from = 1L, to = loc[ind2, 1] - 1L
    )
  }
  return(out)
}

#' @keywords internal
#' @noRd
.substr_postpart <- function(x, loc, nx, nc) {
  out <- character(nx)
  ind <- loc[, 2] >= nc
  out[ind] <- ""
  ind2 <- which(!ind)
  if(length(ind2) > 0L) {
    out[ind2] <- stringi::stri_sub(
      x[ind2], from = loc[ind2, 2] + 1L, to = nc[ind2]
    )
  }
  return(out)
}
