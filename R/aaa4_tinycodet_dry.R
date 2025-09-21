#' Overview of the 'tinycodet' "Don't Repeat Yourself" Functionality
#'
#' @description
#' "Don't Repeat Yourself", sometimes abbreviated as "DRY",
#' is the coding principle not to write unnecessarily repetitive code.
#' To help in that effort, the 'tinycodet' R-package
#' introduces a few features:
#'
#'
#'  * The \link{transform_if} function.
#'  * \link[=matrix_ops]{Operators} for short-hand re-ordering matrices Row- or Column-wise.
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
#'
#' # with tinycodet:
#' object |> transform_if(\(x) x > 0, log, \(x) x^2, \(x) -Inf) # compact & no warning
#'
#'


#' @rdname aaa4_tinycodet_dry
#' @name aaa4_tinycodet_dry
#' @aliases tinycodet_dry
NULL
