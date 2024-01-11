#' String Arithmetic Operators
#'
#' @description
#'
#' String arithmetic operators. \cr
#' \cr
#' The \code{ x %s+% y } operator is exported from 'stringi',
#' and concatenates character vectors \code{x} and \code{y}. \cr
#' \cr
#' The \code{ x %s-% p } operator removes character/pattern
#' defined in \code{p} from \code{x}. \cr
#' \cr
#' The \code{ x %s*% n } operator is exported from 'stringi',
#' and duplicates each string in \code{x} \code{n} times,
#' and concatenates the results. \cr
#' \cr
#' The \code{ x %s/% p } operator counts how often character/pattern
#' defined in \code{p} occurs in each element of \code{x}. \cr
#' \cr
#' The \code{ x %s//% brk } operator counts how often the text boundary specified in list \code{brk}
#' occurs in each element of \code{x}. \cr
#' \cr
#' The \code{ e1 %s$% e2 } operator is exported from 'stringi',
#' and provides access to \link[stringi]{stri_sprintf} in the form of an infix operator. \cr
#' \cr
#' The \code{ x %ss% p } operator splits the strings in \code{x}
#' by a delimiter character/pattern defined in \code{p},
#' and removes \code{p} in the process. \cr
#' For cutting strings by text boundaries, or around a location,
#' see \link{strcut_brk} and \link{strcut_loc}.
#' \cr
#'
#'
#' @param x a string or character vector.
#' @param p either a list with 'stringi' arguments (see \link{s_pattern}),
#' or else a character vector of the same length as \code{x} or length 1
#' with regular expressions. \cr
#' `r .mybadge_string("regex", "darkred")` \cr
#' `r .mybadge_string("fixed", "darkgreen")` \cr
#' `r .mybadge_string("coll", "pink")` \cr
#' `r .mybadge_string("charclass", "lightyellow")` \cr
#' @param brk a list with arguments to be send to \link[stringi]{stri_count_boundaries}. \cr
#' see also \link[stringi]{stri_opts_brkiter}. \cr
#' `r .mybadge_string("boundaries", "blue")` \cr
#'
#'
#'
#'
#' @returns
#' The \code{%s+%}, \code{%s-%}, and \code{%s*%} operators
#' return a character vector of the same length as \code{x}. \cr
#' The \code{%s/%} and \code{%s//%} both return an integer vector of the same length as \code{x}. \cr
#' The \code{%s$%} operator returns a character vector.
#' The \code{%ss%} operator returns a list of the split strings - or,
#' if \code{simplify = TRUE} / \code{simplify = NA},
#' returns a matrix of the split strings.
#'
#'
#' @seealso \link{tinycodet_strings}
#'
#'
#' @examples
#' 
#' 
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' y <- c("a", "b")
#' p <- rep("a|e|i|o|u", 2) # same as p <- list(regex = rep("a|e|i|o|u", 2))
#' n <- c(3, 2)
#'
#' x %s+% y # = paste0(x,y)
#' x %s-% p # remove all vowels from x
#' x %s*% n
#' x %s/% p # count how often vowels appear in each string of vector x
#' x %ss% p # split x around vowels, removing the vowels in the process
#' x %ss% s_regex(p, simplify = NA) # same as above, but in matrix form
#'
#' test <- c(
#' paste0("The\u00a0above-mentioned    features are very useful. ",
#' "Spam, spam, eggs, bacon, and spam. 123 456 789"),
#' "good morning, good evening, and good night"
#' )
#' test %s//% list(type = "character")
#'
#'
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' y <- "a"
#' # pattern that ignores case:
#' p <- list(regex = rep("A|E|I|O|U", 2), case_insensitive = TRUE)
#' n <- c(2, 3)
#'
#' x %s+% y # = paste0(x,y)
#' x %s-% p # remove all vowels from x
#' x %s*% n
#' x %s/% p # count how often vowels appears in each string of vector x.
#' 
#' x <- c(paste(letters, collapse = ", "), paste(LETTERS, collapse = ", "))
#' print(x)
#' x %ss% ", "
#' t(x %ss% s_fixed(", ", simplify = NA))
#'


#' @name str_arithmetic
NULL

#' @importFrom stringi %s+%
#' @export
stringi::`%s+%`


#' @importFrom stringi %s*%
#' @export
stringi::`%s*%`


#' @importFrom stringi %s$%
#' @export
stringi::`%s$%`


#' @rdname str_arithmetic
#' @export
`%s-%` <- function(x, p) {
  if(is.list(p)){
    return(do.call(stringi::stri_replace_all, c(list(str=x, replacement=""), p)))
  } else if(is.character(p)) {
    return(stringi::stri_replace_all_regex(x, "", pattern = p))
  } else {
    stop("right hand side must be a character vector or list")
  }
}

#' @rdname str_arithmetic
#' @export
`%s/%` <- function(x, p) {
  if(is.list(p)){
    return(do.call(stringi::stri_count, c(list(str=x), p)))
  } else if(is.character(p)){
    return(stringi::stri_count_regex(x, p))
  } else {
    stop("right hand side must be a character vector or list")
  }
}

#' @rdname str_arithmetic
#' @export
`%s//%` <- function(x, brk) {
  return(do.call(stringi::stri_count_boundaries, c(list(str=x), brk)))
}


#' @rdname str_arithmetic
#' @export
`%ss%` <- function(x, p) {
  if(is.list(p)){
    return(do.call(stringi::stri_split, c(list(str=x), p)))
  } else if(is.character(p)){
    return(stringi::stri_split_regex(x, p))
  } else {
    stop("right hand side must be a character vector or list")
  }
}
