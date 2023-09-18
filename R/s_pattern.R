#' Pattern specifications for string related infix operators.
#'
#'@description
#'
#'
#' The \code{%s-%} and \code{%s/%} operators,
#' as well as the string detection operators (\code{%s{}%}, \code{%s!{}%}),
#' perform pattern matching for some purpose,
#' where the pattern is given on the right hand side. \cr
#' When a character vector or string is given on the right hand side,
#' this is interpreted as case-sensitive
#' \code{regex} patterns from `stringi`. \cr
#' \cr
#' Instead of giving a string or character vector of regex patterns,
#' one can also supply a list to specify exactly how the pattern should be interpreted.
#' The list should use the exact same argument convention as `stringi`. \cr
#' \cr
#' For example: \cr
#'
#' * \code{list(regex=p, case_insensitive=FALSE, ...)}
#' * \code{list(fixed=p, ...)}
#' * \code{list(coll=p, ...)}
#' * \code{list(charclass=p, ...)}
#'
#' All arguments in the list are simply passed to the
#' appropriate functions in \code{stringi}. \cr
#' For example: \cr
#' \code{x %s/% p } \cr
#' counts how often regular expression specified in character vector
#' \code{p} occurs in \code{x}, whereas the following, \cr
#' \code{x %s/% list(fixed=p, case_insensitive=TRUE) } \cr
#' will do the same,
#' except it uses fixed (i.e. literal) expression,
#' and it does not distinguish between upper case and lower case characters. \cr
#' \cr
#' Related to the above, \code{tinycodet} adds some convenience functions based on
#' the \code{stri_opts_} - functions in \code{stringi}
#' (convenient in the sense they already have argument names, thus allowing for auto code completion): \cr
#'
#' * \code{stri_rgx(p, ...)} is equivalent to \code{c(list(regex = p), ...)}
#' * \code{stri_fxd(p, ...)} is equivalent to \code{c(list(fixed = p), ...)}
#' * \code{stri_cll(p, ...)} is equivalent to \code{c(list(coll = p), ...)}
#' * \code{stri_chrcls(p, ...)} is equivalent to \code{c(list(charclass = p), ... )}
#'
#' With the ellipsis (\code{...})
#'  being passed to the appropriate
#'  \code{stri_opts}-functions
#'  when it matches their arguments.
#'
#'
#'
#'
#' @param p a character vector giving the pattern to search for.
#' @param case_insensitive see \link[stringi]{stri_opts_regex} and \link[stringi]{stri_opts_fixed}.
#' @param comments,dotall,multiline,time_limit,stack_limit see \link[stringi]{stri_opts_regex}.
#' @param overlap see \link[stringi]{stri_opts_fixed}.
#' @param locale,strength,alternate_shifted,french,uppercase_first,case_level,normalization,numeric see \link[stringi]{stri_opts_collator}.
#' @param ... additional arguments not part of the \code{stri_opts} - functions to be passed here. \cr
#' For example: \code{max_count}
#'
#'
#' @return
#' A list.
#'
#' @seealso [tinycodet_strings()]
#'
#'
#' @examples
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- rep("a|e|i|o|u", 2) # same as p <- list(regex=rep("a|e|i|o|u", 2))
#' x %s/% p # count how often vowels appear in each string of vector x.
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' x %s/% list(regex = rep("A|E|I|O|U", 2), case_insensitive = TRUE)
#' x %s/% stri_rgx(rep("A|E|I|O|U", 2), case_insensitive = TRUE)
#'
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- list(regex = c("A", "A"), case_insensitive=TRUE)
#' x %s{}% p
#' x %s!{}% p
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- list(fixed = c("A", "A"), case_insensitive=TRUE)
#' x %s{}% p
#' x %s!{}% p
#'
#'


#'


#' @rdname s_pattern
#' @export
stri_rgx <- function(
    p,
    case_insensitive,
    comments,
    dotall,
    multiline,
    time_limit,
    stack_limit,
    ...
){
  opts <- stringi::stri_opts_regex(
    case_insensitive = case_insensitive,
    comments = comments,
    dotall = dotall,
    multiline = multiline,
    time_limit = time_limit,
    stack_limit = stack_limit
  )

  out <- c(
    list(regex = p),
    opts,
    ...
  )
  return(out)
}

#' @rdname s_pattern
#' @export
stri_fxd <- function(
    p, case_insensitive, overlap, ...
) {
  opts <- stringi::stri_opts_fixed(
    case_insensitive = case_insensitive,
    overlap = overlap
  )
  out <- c(
    list(fixed = p),
    opts,
    ...
  )
  return(out)
}

#' @rdname s_pattern
#' @export
stri_cll <- function(
    p,
    locale,
    strength,
    alternate_shifted,
    french,
    uppercase_first,
    case_level,
    numeric,
    normalization,
    ...
) {
  opts <- stringi::stri_opts_collator(
    locale = locale,
    strength = strength,
    alternate_shifted = alternate_shifted,
    french = french,
    uppercase_first = uppercase_first,
    case_level = case_level,
    numeric = numeric,
    normalization = normalization
  )
  out <- c(
    list(coll=p),
    opts,
    ...
  )
}

#' @rdname s_pattern
#' @export
stri_chrcls <- function(
    p, ...
) {
  list(charclass = p, ...)
}

#
# `%s-%` <- function (x, p) {
#   check.list <- is.list(p)
#   check.chr <- is.character(p) & !.is.stringrpattern(p)
#   check.stringr <- .is.stringrpattern(p)
#   if(check.list){
#     return(do.call(stringi::stri_replace_all, c(list(str=x, replacement=""), p)))
#   }
#   if(check.chr) {
#     return(stringi::stri_replace_all(x, "", regex=p))
#   }
#   if(check.stringr) {
#     p <- .stringrpattern2list(p)
#     return(do.call(stringi::stri_replace_all, c(list(str=x, replacement=""), p)))
#   }
#   if(!check.list & !check.chr & !check.stringr) {
#     stop("right hand side must be a character vector or list")
#   }
# }
#
# `%s/%` <- function(x, p) {
#   check.list <- is.list(p)
#   check.chr <- is.character(p) & !.is.stringrpattern(p)
#   check.stringr <- .is.stringrpattern(p)
#   if(check.list){
#     return(do.call(stringi::stri_count, c(list(str=x), p)))
#   }
#   if(check.chr){
#     return(stringi::stri_count(x, regex=p))
#   }
#   if(check.stringr) {
#     p <- .stringrpattern2list(p)
#     return(do.call(stringi::stri_count, c(list(str=x), p)))
#   }
#   if(!check.list & !check.chr & !check.stringr) {
#     stop("right hand side must be a character vector or list")
#   }
# }
#
# .is.stringrpattern <- function(p) {
#   checks <- c(
#     is.character(p),
#     (!is.null(attr(p, "options"))),
#     stringi::stri_detect(class(p)[1], regex = "stringr_")
#   )
#   return(isTRUE(all(checks)))
# }
#
# .stringrpattern2list <- function(p) {
#   pattern_type <- stringi::stri_replace_all(class(p)[1], regex = "stringr_", "")
#   opts <- attr(p, "options")
#   out <- c(p, opts)
#   names(out)[[1]] <- pattern_type
#   return(out)
# }
