#' Miscellaneous functions to help your coding etiquette
#'
#' @description
#' The \code{stricter_TrueFalse()} function re-assigns the \code{T} and \code{F} values to
#' \code{NULL}, and locks them, forcing the user to use \code{TRUE} and \code{FALSE}.
#' Removing the created \code{T} and \code{F} objects will restore their default behaviour. \cr
#' \cr
#' The \code{X %<-c% A} operator creates a \code{CONSTANT} \code{X}
#' and assigns \code{A} to it. \cr
#' Constants cannot be changed, only accessed or removed.
#' So if you have a piece of code that absolutely requires some \code{CONSTANT},
#' use this operator to create said \code{CONSTANT}. \cr
#' Removing object \code{X} also removes its binding lock.
#' Thus to change a \code{CONSTANT}, simply remove it and re-create it. \cr
#'
#'
#' @returns
#' For \code{stricter_TrueFalse()}: \cr
#' Two objects, namely \code{T} and \code{F}, both set to \code{NULL}.
#' Removing the created \code{T} and \code{F} objects will restore their default behaviour. \cr
#' \cr
#' For \code{X %<-c% A}: \cr
#' The object \code{X} containing \code{A} is created in the current environment,
#' and this object cannot be changed. It can only be accessed or removed.
#'
#' @param X a syntactically valid unquoted name of the object to be created.
#' @param A any kind of object to be assigned to \code{X}.
#'
#'
#' @examples
#'
#' stricter_TrueFalse()
#' X %<-c% data.frame(x=3, y=2) # this data.frame cannot be changed. Only accessed or removed.
#' X[1, ,drop=FALSE]
#'
#'

#' @name misc
NULL

#' @rdname misc
#' @export
stricter_TrueFalse <- function() {
  assign("T", NULL, envir = parent.frame(n = 1))
  lockBinding("T", env = parent.frame(n = 1))

  assign("F", NULL, envir = parent.frame(n = 1))
  lockBinding("F", env = parent.frame(n = 1))
}


#' @rdname misc
#' @export
`%<-c%` <- function(X, A) {
  x_chr <- as.character(substitute(X))
  check <- c(
    make.names(x_chr)==x_chr,
    length(x_chr)==1
  ) |> all()
  if(!check) {
    stop("syntactically invalid `x` given")
  }
  assign(x_chr, A, envir = parent.frame(n = 1))
  lockBinding(x_chr, env = parent.frame(n = 1))
}
