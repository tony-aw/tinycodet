#' tinycodetfakepkg1
#'
#' @description
#' Bla
#' 
#' 
#' @param A whatever
#' @param X whatever
#' 
#' @name tinycodetfakepkg1
NULL

#' @rdname tinycodetfakepkg1
#' @export
fun_overwritten <- function() {
  print("overfun of tinycodetfakepkg1")
}

#' @rdname tinycodetfakepkg1
#' @export
fun11 <- function() {
  print("function 1 of tinycodetfakepkg1")
}

#' @rdname tinycodetfakepkg1
#' @export
fun12 <- function() {
  print("function 2 of tinycodetfakepkg1")
}


#' @rdname tinycodetfakepkg1
#' @export
`%opover%` <- function(X, A) {
  print("overinop of tinycodetfakepkg1")
}

#' @rdname tinycodetfakepkg1
#' @export
`%op11%` <- function(X, A) {
  print("inop 1 of tinycodetfakepkg1")
}

#' @rdname tinycodetfakepkg1
#' @export
`%op12%` <- function(X, A) {
  print("inop 2 of tinycodetfakepkg1")
}
