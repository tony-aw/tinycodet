#' Overview of the 'tinycodet' "Don't Repeat Yourself" Functionality
#'
#' @description
#' "Don't Repeat Yourself", sometimes abbreviated as "DRY",
#' is the coding principle not to write unnecessarily repetitive code.
#' To help in that effort, the 'tinycodet' R-package
#' introduces a few functions:
#'
#'
#'  * The \link{transform_if} function
#'  * The \link[=%unreal =%]{subset_if operators and the in-place unreal modifier operator}.
#'  * The \link[=%:=%]{general in-place (mathematical) modification operator}.
#'
#'
#' @seealso \link{tinycodet_help}
#'
#'
#' @examples
#'
#' object <- matrix(c(-9:8, NA, NA) , ncol=2)
#'
#' # in base R:
#' ifelse( # repetitive, and gives unnecessary warning
#'   is.na(object > 0), -Inf,
#'   ifelse(
#'     object > 0,  log(object), object^2
#'   )
#' )
#' mtcars$mpg[mtcars$cyl>6] <- (mtcars$mpg[mtcars$cyl>6])^2 # long
#'
#' # with tinycodet:
#' object |> transform_if(\(x) x > 0, log, \(x) x^2, \(x) -Inf) # compact & no warning
#' mtcars$mpg[mtcars$cyl > 6] %:=% \(x) x^2 # short
#'
#'


#' @rdname aaa4_tinycodet_dry
#' @name aaa4_tinycodet_dry
#' @aliases tinycodet_dry
NULL
