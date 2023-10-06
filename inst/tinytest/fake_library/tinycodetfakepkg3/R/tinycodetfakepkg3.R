#' tinycodetfakepkg3ellaneous functions to help your coding etiquette
#'
#' @description
#' Bla
#' 
#' 
#' @param A whatever
#' @param X whatever
#' 
#' @name tinycodetfakepkg3
NULL

#' @rdname tinycodetfakepkg3
#' @export
fun_overwritten <- function() {
  print("overfun of tinycodetfakepkg3")
}

#' @rdname tinycodetfakepkg3
#' @export
#' 
fun31 <- function() {
  print("function 1 of tinycodetfakepkg3")
}

#' @rdname tinycodetfakepkg3
#' @export
fun32 <- function() {
  print("function 2 of tinycodetfakepkg3")
}


#' @rdname tinycodetfakepkg3
#' @export
`%opover%` <- function(X, A) {
  print("inop 1 of tinycodetfakepkg3")
}

#' @rdname tinycodetfakepkg3
#' @export
`%op31%` <- function(X, A) {
  print("inop 1 of tinycodetfakepkg3")
}

#' @rdname tinycodetfakepkg3
#' @export
`%op32%` <- function(X, A) {
  print("overinop of tinycodetfakepkg3")
}
