#' Pattern Specifications for String Related Operators
#'
#' @description
#'
#'
#' The \link[=str_arithmetic]{%s-%, %s/%, %ss%} operators,
#' as well as the string search operators (\link{str_search}),
#' perform pattern matching for some purpose,
#' where the pattern is given on the right hand side. \cr
#' When a character vector or string is given on the right hand side,
#' this is interpreted as case-sensitive
#' \code{regex} patterns from 'stringi'. \cr
#' \cr
#' Instead of giving a string or character vector of regex patterns,
#' one can also supply a list to specify exactly how the pattern should be interpreted.
#' The list should use the exact same argument convention as 'stringi'. \cr
#' \cr
#' For example:
#'
#' * \code{list(regex = p, case_insensitive = FALSE, ...)}
#' * \code{list(fixed = p, ...)}
#' * \code{list(coll = p, ...)}
#' * \code{list(charclass = p, ...)}
#'
#' All arguments in the list are simply passed to the
#' appropriate functions in 'stringi'. \cr
#' For example:
#'
#' ```{r, echo = TRUE, eval = FALSE}
#' x %s/% p
#' ```
#'
#' counts how often regular expression specified in character vector
#' \code{p} occurs in \code{x}, whereas the following,
#'
#' ```{r, echo = TRUE, eval = FALSE}
#' x %s/% list(fixed = p, case_insensitive = TRUE)
#' ```
#'
#' will do the same,
#' except it uses fixed (i.e. literal) expression,
#' and it does not distinguish between upper case and lower case characters. \cr
#' \cr
#' 'tinycodet' adds some convenience functions based on
#' the \code{stri_opts_} - functions in 'stringi':
#'
#' * \code{s_regex(p, ...)} is equivalent to \code{list(regex = p, ...)}
#' * \code{s_fixed(p, ...)} is equivalent to \code{list(fixed = p, ...)}
#' * \code{s_coll(p, ...)} is equivalent to \code{list(coll = p, ...)}
#' * \code{s_chrcls(p, ...)} is equivalent to \code{list(charclass = p, ... )}
#'
#' With the ellipsis (\code{...})
#' being passed to the appropriate
#' 'stringi'-functions
#' when it matches their arguments. \cr
#' \cr
#' 'stringi' infix operators start with "\code{%s}",
#' though they all have an alias starting with "\code{%stri}".
#' In analogy to that, the above functions start with "\code{s_}"
#' rather than "\code{stri_}", as they are all meant for operators only. \cr
#'
#'
#'
#' @param p a character vector giving the pattern to search for. \cr
#' `r .mybadge_string("regex", "darkred")` \cr
#' `r .mybadge_string("fixed", "darkgreen")` \cr
#' `r .mybadge_string("coll", "pink")` \cr
#' `r .mybadge_string("charclass", "lightyellow")` \cr
#' @param case_insensitive see \link[stringi]{stri_opts_regex} and \link[stringi]{stri_opts_fixed}.
#' @param comments,dotall,multiline see \link[stringi]{stri_opts_regex}.
#' @param time_limit,stack_limit see \link[stringi]{stri_opts_regex}.
#' @param overlap see \link[stringi]{stri_opts_fixed}.
#' @param locale,strength,alternate_shifted see \link[stringi]{stri_opts_collator}.
#' @param french,normalization,numeric see \link[stringi]{stri_opts_collator}.
#' @param uppercase_first,case_level see \link[stringi]{stri_opts_collator}.
#' @param ... additional arguments not part of the \code{stri_opts} - functions to be passed here. \cr
#' For example: the \code{at} argument for the \link{str_search} operators.
#' 
#'
#' @returns
#' A list with arguments to be passed to the appropriate operators.
#'
#' @seealso \link{tinycodet_strings}
#'
#'
#' @examples
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' p <- rep("a|e|i|o|u", 2) # same as p <- list(regex = rep("a|e|i|o|u", 2))
#' x %s/% p # count how often vowels appear in each string of vector x.
#'
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' x %s/% list(regex = rep("A|E|I|O|U", 2), case_insensitive = TRUE)
#' x %s/% s_regex(rep("A|E|I|O|U", 2), case_insensitive = TRUE)
#'
#'
#' x <- c(paste0(letters[1:13], collapse = ""),
#'        paste0(letters[14:26], collapse = ""))
#' print(x)
#' p <- list(fixed = c("A", "A"), case_insensitive = TRUE)
#' x %s{}% p
#' x %s!{}% p
#' p <- s_fixed(c("A", "A"), case_insensitive = TRUE)
#' x %s{}% p
#' x %s!{}% p
#'
#'x <- c(paste0(letters[1:13], collapse = ""),
#'       paste0(letters[14:26], collapse = ""), NA)
#' p <- s_fixed("abc", at = "start")
#' x %s{}% p
#' stringi::stri_startswith(x, fixed = "abc") # same as above
#' 
#' p <- s_fixed("xyz", at = "end")
#' x %s{}% p
#' stringi::stri_endswith(x, fixed = "xyz") # same as above


#' @name s_pattern
NULL


#' @rdname s_pattern
#' @export
s_regex <- function(
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
  addargs <- list(...)
  
  out <- c(
    list(regex = p),
    opts,
    addargs
  )
  # if(!missing(rp)) out <- c(out, list(replacement = rp))
  return(out)
}

#' @rdname s_pattern
#' @export
s_fixed <- function(
    p, case_insensitive, overlap, ...
) {
  opts <- stringi::stri_opts_fixed(
    case_insensitive = case_insensitive,
    overlap = overlap
  )
  addargs <- list(...)
  out <- c(
    list(fixed = p),
    opts,
    addargs
  )
  # if(!missing(rp)) out <- c(out, list(replacement = rp))
  return(out)
}

#' @rdname s_pattern
#' @export
s_coll <- function(
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
  addargs <- list(...)
  out <- c(
    list(coll=p),
    opts,
    addargs
  )
  # if(!missing(rp)) out <- c(out, list(replacement = rp))
  return(out)
}

#' @rdname s_pattern
#' @export
s_chrcls <- function(
    p, ...
) {
  addargs <- list(...)
  out <- c(
    list(charclass = p),
    addargs
  )
  # if(!missing(rp)) out <- c(out, list(replacement = rp))
  return(out)
}
