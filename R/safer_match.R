#' Set safer dollar, arguments, and attribute matching
#'
#' @description
#' The \code{safer_partialmatch()} function simply calls the following:
#' 
#' ```{r eval = FALSE}
#' options(
#'    warnPartialMatchDollar = TRUE,
#'    warnPartialMatchArgs = TRUE,
#'    warnPartialMatchAttr = TRUE
#'  )
#' 
#' ```
#' 
#' Thus it forces 'R' to give a warning when partial matching occurs when using
#' the dollar (\link{$}) operator,
#' or when other forms of partial matching occurs. \cr
#' The \code{safer_partialmatch()} function
#' is intended for when running R interactively
#' (see \link[base]{interactive}). \cr \cr
#'
#'
#' 
#' 
#'
#' @return
#' Sets the options. Returns nothing. \cr
#' 
#'
#'
#' @seealso \link{tinycodet_safer}
#'
#' @examplesIf interactive()
#' interactive()
#' 
#'
#' safer_partialmatch()
#' data(iris)
#' head(iris)
#' iris$Sepal.Length <- iris$Sepal.Length^2
#' head(iris)
#'

#' @rdname safer_partialmatch
#' @export
safer_partialmatch <- function() {
  options(
    warnPartialMatchDollar = TRUE,
    warnPartialMatchArgs = TRUE,
    warnPartialMatchAttr = TRUE
  )
}

