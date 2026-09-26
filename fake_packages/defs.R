defs <- "

#' @name tinycodetfakepkg<d>
NULL


#' @rdname tinycodetfakepkg<d>
#' @export
fun_overwritten <- function() {
  print(\"overfun of tinycodetfakepkg<d>\")
}

#' @rdname tinycodetfakepkg<d>
#' @export
fun<d>1 <- function() {
  print(\"function 1 of tinycodetfakepkg<d>\")
}

#' @rdname tinycodetfakepkg<d>
#' @export
fun<d>2 <- function() {
  print(\"function 2 of tinycodetfakepkg<d>\")
}


#' @rdname tinycodetfakepkg<d>
#' @export
`%opover%` <- function(X, A) {
  print(\"overinop of tinycodetfakepkg<d>\")
}

#' @rdname tinycodetfakepkg<d>
#' @export
`%op<d>1%` <- function(X, A) {
  print(\"inop 1 of tinycodetfakepkg<d>\")
}

#' @rdname tinycodetfakepkg<d>
#' @export
`%op<d>2%` <- function(X, A) {
  print(\"inop 2 of tinycodetfakepkg<d>\")
}


#' @rdname tinycodetfakepkg<d>
#' @export
rpbase_overwritten <- function(x) {
  print(\"overpbase of tinycodetfakepkg<d>\")
}

#' @rdname tinycodetfakepkg<d>
#' @export
rpbase<d>1 <- function(x) {
  print(\"rpbase 1 of tinycodetfakepkg<d>\")
}

#' @rdname tinycodetfakepkg<d>
#' @export
rpbase<d>2 <- function(x) {
  print(\"rpbase 2 of tinycodetfakepkg<d>\")
}

#' @rdname tinycodetfakepkg<d>
#' @export
`rpbase_overwritten<-` <- function(x, value) {
  print(\"overrpop of tinycodetfakepkg<d>\")
  x <- value
  return(x)
}

#' @rdname tinycodetfakepkg<d>
#' @export
`rpbase<d>1<-` <- function(x, value) {
  print(\"rpop 1 of tinycodetfakepkg<d>\")
  x <- value
  return(x)
}

#' @rdname tinycodetfakepkg<d>
#' @export
`rpbase<d>2<-` <- function(x, value) {
  print(\"rpop 2 of tinycodetfakepkg<d>\")
  x <- value
  return(x)
}


#' @rdname tinycodetfakepkg<d>
#' @export
const_overwritten <- \"overconst of tinycodetfakepkg<d>\"

#' @rdname tinycodetfakepkg<d>
#' @export
const<d>1 <- \"const 1 of tinycodetfakepkg<d>\"

#' @rdname tinycodetfakepkg<d>
#' @export
const<d>2 <- \"const 2 of tinycodetfakepkg<d>\"

"


stringi::stri_replace_all(defs, "3", fixed = "<d>") |> cat()
