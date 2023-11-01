#' Lock T, Lock F, or Create Locked Constants
#'
#' @description
#' One can re-assign the values \code{T} and \code{F}.
#' One can even run things like \code{T <- FALSE} and \code{F <- TRUE} ! \cr
#' The \code{lock_TF()} function
#' locks the \code{T} and \code{F} values and sets them to \code{TRUE} and \code{FALSE},
#' respectively,
#' to prevent the user from re-assigning them. \cr
#' Removing the created \code{T} and \code{F} objects
#' allows re-assignment again. \cr
#' \cr
#' The \code{X %<-c% A} operator creates a \code{constant} \code{X}
#' and assigns \code{A} to it. \cr
#' Constants cannot be changed, only accessed or removed.
#' So if you have a piece of code that requires some unchangeable \code{constant},
#' use this operator to create said \code{constant}. \cr
#' Removing constant \code{X} also removes its binding lock.
#' Thus to change a \code{constant}, simply remove it and re-create it. \cr
#'
#'
#' @param X a syntactically valid unquoted name of the object to be created.
#' @param A any kind of object to be assigned to \code{X}.
#' @param env an optional environment to give,
#' determining in which environment \code{T} and \code{F} should be locked. \cr
#' When not specified, the current environment
#' (like the global environment, or the environment within a function)
#' is used. \cr
#'
#' @details
#' Note that following statement
#'
#' ```{r, eval = FALSE, echo = TRUE}
#' x %<-c% 2+2
#' print(x)
#' ```
#'
#' returns
#'
#' ```{r, eval = FALSE, echo = TRUE}
#' [1] 2
#' ```
#'
#' due to R's precedence rules.
#' Therefore, in such cases, the right hand side of
#' \code{X %<-c% A} need to be surrounded with brackets.
#' I.e.:
#'
#' ```{r, eval = FALSE, echo = TRUE}
#' x %<-c% (2+2)
#' ```
#'
#' @returns
#' For \code{lock_TF()}: \cr
#' Two \code{constants}, namely \code{T} and \code{F},
#' set to \code{TRUE} and \code{FALSE} respectively,
#' are created in the specified or else current environment,
#' and locked.
#' Removing the created \code{T} and \code{F} objects allows re-assignment again. \cr
#' \cr
#' For \code{X %<-c% A}: \cr
#' The object \code{X} containing \code{A} is created in the current environment,
#' and this object cannot be changed. It can only be accessed or removed.
#'
#' @seealso [tinycodet_safer()]
#'
#'
#' @examples
#'
#' lock_TF()
#' X %<-c% data.frame(x=3, y=2) # this data.frame cannot be changed. Only accessed or removed.
#' X[1, ,drop=FALSE]
#'
#'

#' @name lock
NULL

#' @rdname lock
#' @export
lock_TF <- function(env) {

  if(missing(env)){env <- parent.frame(n = 1)}

  assign("T", TRUE, envir = env)
  lockBinding("T", env = env)

  assign("F", FALSE, envir = env)
  lockBinding("F", env = env)
}


#' @rdname lock
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
