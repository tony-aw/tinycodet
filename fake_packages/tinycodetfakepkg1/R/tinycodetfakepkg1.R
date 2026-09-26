#' tinycodetfakepkg1
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


#' @rdname tinycodetfakepkg1
#' @export
rpbase_overwritten <- function(x) {
  print("overpbase of tinycodetfakepkg1")
}

#' @rdname tinycodetfakepkg1
#' @export
rpbase11 <- function(x) {
  print("rpbase 1 of tinycodetfakepkg1")
}

#' @rdname tinycodetfakepkg1
#' @export
rpbase12 <- function(x) {
  print("rpbase 2 of tinycodetfakepkg1")
}

#' @rdname tinycodetfakepkg1
#' @export
`rpbase_overwritten<-` <- function(x, value) {
  print("overrpop of tinycodetfakepkg1")
  x <- value
  return(x)
}

#' @rdname tinycodetfakepkg1
#' @export
`rpbase11<-` <- function(x, value) {
  print("rpop 1 of tinycodetfakepkg1")
  x <- value
  return(x)
}

#' @rdname tinycodetfakepkg1
#' @export
`rpbase12<-` <- function(x, value) {
  print("rpop 2 of tinycodetfakepkg1")
  x <- value
  return(x)
}


#' @rdname tinycodetfakepkg1
#' @export
const_overwritten <- "overconst of tinycodetfakepkg1"

#' @rdname tinycodetfakepkg1
#' @export
const11 <- "const 1 of tinycodetfakepkg1"

#' @rdname tinycodetfakepkg1
#' @export
const12 <- "const 2 of tinycodetfakepkg1"