#' 'stringi' Pattern Detection Operators
#'
#' @description
#'
#' The \code{x %s{}% p} operator
#' checks for every string in character vector \code{x} if
#' the pattern defined in \code{p} is present. \cr
#' \cr
#' The \code{x %s!{}% p} operator
#' checks for every string in character vector \code{x} if
#' the pattern defined in \code{p} is NOT present. \cr
#'
#'
#' @param x a string or character vector.
#' @param p either a list with 'stringi' arguments (see \link{s_regex}),
#' or else a character vector of the same length as \code{x} with regular expressions. \cr
#' `r .mybadge_string("regex", "darkred")` \cr
#' `r .mybadge_string("fixed", "darkgreen")` \cr
#' `r .mybadge_string("coll", "pink")` \cr
#' `r .mybadge_string("charclass", "lightyellow")` \cr
#'
#'
#'
#' @returns
#' The \code{x %s{}% p} and \code{x %s!{}% p} operators
#' return logical vectors, where \code{TRUE} indicates a pattern was found,
#' and \code{FALSE} indicates a pattern was not found. \cr
#'
#' @seealso [tinycodet_strings()]
#'
#'
#' @examples
#'
#' # simple pattern ====
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s{}% "a"
#' x %s!{}% "a"
#' which(x %s{}% "a")
#' which(x %s!{}% "a")
#' x[x %s{}% "a"]
#' x[x %s!{}% "a"]
#' x[x %s{}% "a"] <- 1
#' x[x %s!{}% "a"] <- 1
#' print(x)
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' x %s{}% "1"
#' x %s!{}% "1"
#' which(x %s{}% "1")
#' which(x %s!{}% "1")
#' x[x %s{}% "1"]
#' x[x %s!{}% "1"]
#' x[x %s{}% "1"] <- "a"
#' x[x %s!{}% "1"] <- "a"
#' print(x)
#'
#'
#' #############################################################################
#'
#' # ignore case pattern ====
#'
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- list(regex = c("A", "A"), case_insensitive=TRUE)
#' x %s{}% p
#' x %s!{}% p
#' which(x %s{}% p)
#' which(x %s!{}% p)
#' x[x %s{}% p]
#' x[x %s!{}% p]
#' x[x %s{}% p] <- "hello"
#' x[x %s!{}% p] <- "hello"
#' print(x)
#'
#'
#' #############################################################################
#'
#' # multi-character pattern ====
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- list(regex = rep("AB", 2), case_insensitive=TRUE)
#' x %s{}% p
#' x %s!{}% p
#' which(x %s{}% p)
#' which(x %s!{}% p)
#' x[x %s{}% p]
#' x[x %s!{}% p]
#' x[x %s{}% p] <- "CD"
#' x[x %s!{}% p] <- "CD"
#' print(x)
#'
#'
#'


#' @rdname str_truth
#' @export
`%s{}%` <- function(x, p) {
  if(isTRUE(is.list(p))){
    return(do.call(stringi::stri_detect, c(list(str=x), p)))
  }
  if(isTRUE(is.character(p))) {
    return(stringi::stri_detect(x, regex=p))
  } else {
    stop("right hand side must be a character vector or list")
  }
}

#' @rdname str_truth
#' @export
`%s!{}%` <- function(x, p) {
  if(isTRUE(is.list(p))){
    return(!do.call(stringi::stri_detect, c(list(str=x), p)))
  }
  if(isTRUE(is.character(p))) {
    return(!stringi::stri_detect(x, regex=p))
  } else {
    stop("right hand side must be a character vector or list")
  }
}


