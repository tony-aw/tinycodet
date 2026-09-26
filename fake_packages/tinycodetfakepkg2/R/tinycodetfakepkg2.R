#' tinycodetfakepkg2
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
#' @name tinycodetfakepkg2
NULL


#' @rdname tinycodetfakepkg2
#' @export
fun_overwritten <- function() {
  print("overfun of tinycodetfakepkg2")
}

#' @rdname tinycodetfakepkg2
#' @export
fun21 <- function() {
  print("function 1 of tinycodetfakepkg2")
}

#' @rdname tinycodetfakepkg2
#' @export
fun22 <- function() {
  print("function 2 of tinycodetfakepkg2")
}


#' @rdname tinycodetfakepkg2
#' @export
`%opover%` <- function(X, A) {
  print("overinop of tinycodetfakepkg2")
}

#' @rdname tinycodetfakepkg2
#' @export
`%op21%` <- function(X, A) {
  print("inop 1 of tinycodetfakepkg2")
}

#' @rdname tinycodetfakepkg2
#' @export
`%op22%` <- function(X, A) {
  print("inop 2 of tinycodetfakepkg2")
}


#' @rdname tinycodetfakepkg2
#' @export
rpbase_overwritten <- function(x) {
  print("overpbase of tinycodetfakepkg2")
}

#' @rdname tinycodetfakepkg2
#' @export
rpbase21 <- function(x) {
  print("rpbase 1 of tinycodetfakepkg2")
}

#' @rdname tinycodetfakepkg2
#' @export
rpbase22 <- function(x) {
  print("rpbase 2 of tinycodetfakepkg2")
}

#' @rdname tinycodetfakepkg2
#' @export
`rpbase_overwritten<-` <- function(x, value) {
  print("overrpop of tinycodetfakepkg2")
  x <- value
  return(x)
}

#' @rdname tinycodetfakepkg2
#' @export
`rpbase21<-` <- function(x, value) {
  print("rpop 1 of tinycodetfakepkg2")
  x <- value
  return(x)
}

#' @rdname tinycodetfakepkg2
#' @export
`rpbase22<-` <- function(x, value) {
  print("rpop 2 of tinycodetfakepkg2")
  x <- value
  return(x)
}


#' @rdname tinycodetfakepkg2
#' @export
const_overwritten <- "overconst of tinycodetfakepkg2"

#' @rdname tinycodetfakepkg2
#' @export
const21 <- "const 1 of tinycodetfakepkg2"

#' @rdname tinycodetfakepkg2
#' @export
const22 <- "const 2 of tinycodetfakepkg2"