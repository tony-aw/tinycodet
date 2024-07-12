#' Additional Logic Operators
#'
#' @description
#' Additional logic operators: \cr
#' \cr
#' The \code{x %xor% y} operator is the "exclusive-or" operator,
#' the same as \link{xor}\code{(x, y)}. \cr
#' \cr
#' The \code{x %n&%} operator is the "not-and" operator,
#' the same as \code{(!x) & (!y)}. \cr
#' \cr
#' The \code{x %out% y} operator is the same as \code{!x %in% y}. \cr
#' \cr
#' The \code{x %?=% y} operator checks if \code{x} and \code{y}
#' are \bold{both} unreal or unknown (i.e. NA, NaN, Inf, -Inf). \cr
#' \cr
#' The \code{n %=numtype% numtype} operator checks
#' for every value of numeric vector \code{n}
#' if it can be considered a number belonging to type \code{numtype}. \cr
#' \cr
#' The \code{s %=strtype% strtype} operator checks
#' for every value of character vector \code{s}
#' if it can seen as a certain \code{strtype}. \cr
#' \cr
#'
#' @param x,y see \link[base]{Logic}.
#' @param n a numeric vector.
#' @param s a character vector.
#' @param numtype a single string giving the numeric type to be checked. \cr
#' See Details section for supported types.
#' @param strtype a single string giving the string type to be checked. \cr
#' See Details section for supported types.
#'
#'
#' @details
#'
#' For argument \code{numtype}, the following options are supported: \cr
#'  *  \code{"~0"}: zero, or else a number whose absolute value is smaller than the
#'  Machine tolerance (\code{sqrt(.Machine$double.eps)}).
#'  *  \code{"B"}: binary numbers (exactly 0 or exactly 1);
#'  *  \code{"prop"}: proportions - numbers between 0 and 1 (exactly 0 or 1 is also allowed);
#'  *  \code{"I"}: Integers;
#'  *  \code{"odd"}: odd integers;
#'  *  \code{"even"}: even integers;
#'  *  \code{"R"}: Real numbers;
#'  *  \code{"unreal"}: infinity, NA, or NaN; \cr \cr
#'
#'
#'
#' For argument \code{strtype}, the following options are supported: \cr
#'  *  \code{"empty"}: checks if the string only consists of empty spaces.
#'  *  \code{"unreal"}: checks if the string is NA, or if it has literal string "NA", "NaN" or "Inf",
#'  regardless if it has leading or trailing spaces.
#'  *  \code{"numeric"}: checks if the string can be converted to a number,
#'  disregarding leading and trailing spaces.
#'  I.e. the string "5.0" can be converted to the the actual number \code{5.0}.
#'  *  \code{"special"}: checks if the string consists of only special characters. \cr \cr
#'
#'
#' @returns
#' A logical vector.
#'
#'
#' @examples
#' x <- c(TRUE, FALSE, TRUE, FALSE, NA, NaN, Inf, -Inf, TRUE, FALSE)
#' y <- c(FALSE, TRUE, TRUE, FALSE, rep(NA, 6))
#' outcome <- data.frame(
#'   x = x, y = y,
#'   "x %xor% y" = x %xor% y, "x %n&% y" = x %n&% y, "x %?=% y" = x %?=% y,
#'   check.names = FALSE
#' )
#' print(outcome)
#'
#' 1:3 %out% 1:10
#' 1:10 %out% 1:3
#'
#'
#' n <- c(0:5, 0:-5, 0.1, -0.1, 0, 1, Inf, -Inf, NA, NaN)
#' 1e-20 %=numtype% "~0"
#' n[n %=numtype% "B"]
#' n[n %=numtype% "prop"]
#' n[n %=numtype% "I"]
#' n[n %=numtype% "odd"]
#' n[n %=numtype% "even"]
#' n[n %=numtype% "R"]
#' n[n %=numtype% "unreal"]
#'
#' s <- c(" AbcZ123 ", " abc ", " 1.3 ", " !#$%^&*() ", "  ", "  NA  ", "  NaN  ", " Inf ")
#' s[s %=strtype% "empty"]
#' s[s %=strtype% "unreal"]
#' s[s %=strtype% "numeric"]
#' s[s %=strtype% "special"]
#'
#'
#'


#' @name logic_ops
NULL

#' @rdname logic_ops
#' @export
`%xor%` <- function(x, y) {
  return(xor(x,y))
}

#' @rdname logic_ops
#' @export
`%n&%` <- function(x, y) {
  out <- (!x) & (!y)
  out[is.na(x) | is.na(y)] <- NA
  return(out)
}

#' @rdname logic_ops
#' @export
`%out%` <- function(x, y) {
  return(!(x %in% y))
}

#' @rdname logic_ops
#' @export
`%?=%` <- function(x, y) {
  check.x <- is.na(x) | is.nan(x) | is.infinite(x)
  check.y <- is.na(y) | is.nan(y) | is.infinite(y)
  return(check.x & check.y)
}

#' @rdname logic_ops
#' @export
`%=numtype%` <- function(n, numtype) {
  if(length(numtype) > 1){stop("`numtype` must be a single string")}
  if(!numtype %in% c("unreal", "~0", "B", "prop", "I", "odd", "even", "R")){
    stop("numtype not recognised")
  }
  if(!is.numeric(n)) { stop("`n` must be numeric") }
  check.unreal <- is.infinite(n) | is.nan(n) | is.na(n)
  return(switch(
    numtype,
    "unreal" = check.unreal,
    "~0" = ifelse(check.unreal, FALSE, abs(n) < sqrt(.Machine$double.eps)),
    "B" = ifelse(check.unreal, FALSE, n %in% c(0, 1)),
    "prop" = ifelse(check.unreal, FALSE, n >= 0L & n <= 1L),
    "I" = ifelse(check.unreal, FALSE, n == round(n)),
    "odd" = ifelse(check.unreal, FALSE, n == round(n) & !((n / 2L) == round(n / 2L))),
    "even" = ifelse(check.unreal, FALSE, n == round(n) & ((n / 2L) == round(n / 2L))),
    "R" = !check.unreal
  ))
}

#' @rdname logic_ops
#' @export
`%=strtype%` <- function(s, strtype) {
  if(length(strtype) > 1){stop("`strtype` must be a single string")}
  if(! strtype %in% c("unreal", "empty", "numeric", "special")){
    stop("strtype not recognised")
  }
  if(!is.character(s)) { stop("`s` must be character") }
  check.unreal <- is.na(s)
  s.clean <- trimws(s, which="both")
  
  return(switch(
    strtype,
    "empty" = ifelse(
      check.unreal, FALSE, s.clean==""
    ),
    "unreal" = ifelse(
      check.unreal,
      TRUE,
      s.clean == "NA" | s.clean == "NaN" | s.clean == "Inf" | s.clean == "-Inf"
    ),
    "numeric" = ifelse(
      check.unreal, FALSE, suppressWarnings(!is.na(as.numeric(s.clean)))
    ),
    "special" = ifelse(
      check.unreal, FALSE,
      (nchar(s.clean) == nchar(stringi::stri_replace_all_regex(s.clean, "[[:alnum:]]", ""))) & (nchar(s.clean) > 0L)
    )
  ))
}
