#' String detection operators
#'
#' @description
#'
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
#' @param p either a list with \code{stringi} arguments (see \link{stri_rgx}),
#' or else a character vector of the same length as \code{x} with regular expressions.
#'
#'
#'
#' @returns
#' The \code{x %s{}% p} and \code{x %s!{}% p} operators
#' return logical vectors. \cr
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
#'
#' x %s{}% "1"
#' x %s!{}% "1"
#' which(x %s{}% "1")
#' which(x %s!{}% "1")
#' x[x %s{}% "1"]
#' x[x %s!{}% "1"]
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


