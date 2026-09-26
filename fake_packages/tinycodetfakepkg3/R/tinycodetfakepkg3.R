#' tinycodetfakepkg3ellaneous functions to help your coding etiquette
#'
#' @description
#' Bla
#' 
#' 
#' @param A whatever
#' @param X whatever
#' @param x description
#' @param value description
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
  print("overinop of tinycodetfakepkg3")
}

#' @rdname tinycodetfakepkg3
#' @export
`%op31%` <- function(X, A) {
  print("inop 1 of tinycodetfakepkg3")
}

#' @rdname tinycodetfakepkg3
#' @export
`%op32%` <- function(X, A) {
  print("inop 2 of tinycodetfakepkg3")
}


#' @rdname tinycodetfakepkg3
#' @export
rpbase_overwritten <- function(x) {
  print("overpbase of tinycodetfakepkg3")
}

#' @rdname tinycodetfakepkg3
#' @export
rpbase31 <- function(x) {
  print("rpbase 1 of tinycodetfakepkg3")
}

#' @rdname tinycodetfakepkg3
#' @export
rpbase32 <- function(x) {
  print("rpbase 2 of tinycodetfakepkg3")
}

#' @rdname tinycodetfakepkg3
#' @export
`rpbase_overwritten<-` <- function(x, value) {
  print("overrpop of tinycodetfakepkg3")
  x <- value
  return(x)
}

#' @rdname tinycodetfakepkg3
#' @export
`rpbase31<-` <- function(x, value) {
  print("rpop 1 of tinycodetfakepkg3")
  x <- value
  return(x)
}

#' @rdname tinycodetfakepkg3
#' @export
`rpbase32<-` <- function(x, value) {
  print("rpop 2 of tinycodetfakepkg3")
  x <- value
  return(x)
}


#' @rdname tinycodetfakepkg3
#' @export
const_overwritten <- "overconst of tinycodetfakepkg3"

#' @rdname tinycodetfakepkg3
#' @export
const31 <- "const 1 of tinycodetfakepkg3"

#' @rdname tinycodetfakepkg3
#' @export
const32 <- "const 2 of tinycodetfakepkg3"