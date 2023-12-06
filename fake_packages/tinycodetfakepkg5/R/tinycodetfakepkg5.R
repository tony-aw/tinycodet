#' tinycodetfakepkg1
#'
#' @description
#' Bla
#' 
#' 
#' @param x whatever
#' @param y whatever
#' 
#' @name tinycodetfakepkg1
NULL

#' @rdname tinycodetfakepkg1
#' @export
fun_paste <- function(x, y) {
  paste0(x, y)
}


#' @rdname tinycodetfakepkg1
#' @export
`%paste0%` <- function(x, y) {
  paste0(x, y)
}