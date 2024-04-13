#' transform_if: Conditional Sub-set Transformation of Atomic objects
#'
#' @description
#'
#' The \code{transform_if()} function transforms an object \code{x},
#' based on the logical result (\code{TRUE, FALSE, NA})
#' of condition function \code{cond(x)} or logical vector \code{cond},
#' such that: \cr
#'
#' \itemize{
#'  \item For every value where \code{cond(x)==TRUE} / \code{cond==TRUE},
#'  function \code{yes(x)} is run or scalar \code{yes} is returned.
#'  \item For every value where \code{cond(x)==FALSE} / \code{cond==FALSE},
#'  function \code{no(x)} is run or scalar \code{no} is returned.
#'  \item For every value where \code{cond(x)==NA} / \code{cond==NA},
#'  function \code{other(x)} is run or scalar \code{other} is returned. \cr
#' }
#' 
#' For a more \code{ifelse}-like function where
#' \code{yes}, \code{no}, and \code{other} are vectors,
#' see \code{kit::}\link[kit]{iif}.
#'
#' @param x a vector, matrix, or array.
#' @param cond either an object of class \code{logical} with the same length as \code{x}, \cr
#' or a (possibly anonymous) function that returns an object of class \code{logical}
#' with the same length as \code{x}. \cr
#' For example: \code{\(x)x>0}. \cr
#' @param yes the (possibly anonymous) transformation function to use
#' when function \code{cond(x)==TRUE} / logical \code{cond==TRUE}. \cr
#' Alternatively, one can also supply an atomic scalar. \cr
#' If argument \code{yes} is not specified, it defaults to \code{\(x)x}.
#' @param no the (possibly anonymous) transformation function to use
#' when function \code{cond(x)==FALSE} / logical \code{cond==FALSE}. \cr
#' Alternatively, one can also supply an atomic scalar. \cr
#' If argument \code{no} is not specified, it defaults to \code{\(x)x}.
#' @param other the (possibly anonymous) transformation function to use
#' when function \code{cond(x)} / logical \code{cond} returns \code{NA}. \cr
#' Alternatively, one can also supply an atomic scalar. \cr
#' If argument \code{other} is not specified, it defaults to \code{NA}. \cr
#' Note that function  \code{other(x)} is run or scalar \code{other} is returned
#' when function \code{cond(x)} or logical \code{cond} is \code{NA},
#' not necessarily when \code{x} itself is \code{NA}.
#'
#'
#' @details
#' Be careful with coercion! For example the following code:
#'
#' ```{r echo = TRUE, eval = FALSE}
#' x <- c("a", "b")
#' transform_if(x, \(x) x == "a", as.numeric, as.logical)
#' ```
#' returns:
#'
#' ```{r echo=TRUE, eval = FALSE}
#' [1] NA NA
#' ```
#'
#' due to the same character vector being given 2 incompatible classes. \cr
#' \cr
#'
#'
#' @returns
#' The transformed vector, matrix, or array (attributes are conserved).
#'
#'
#' @seealso \link{tinycodet_dry}
#'
#' @examples
#' x <- c(-10:9, NA, NA)
#' object <- matrix(x, ncol = 2)
#' attr(object, "helloworld") <- "helloworld"
#' print(object)
#' y <- 0
#' z <- 1000
#'
#' object |> transform_if(\(x) x > y, log, \(x) x^2, \(x) -z)
#' object |> transform_if(object > y, log, \(x) x^2, -z) # same as previous line
#'


#' @rdname transform_if
#' @export
transform_if <- function(
    x, cond,
    yes = function(x) x, no = function(x) x, other = NA
) {

  # check x:
  n <- length(x)
  if(n == 0) {
    stop("`length(x)==0`")
  }

  # check transformations:
  if(!isTRUE(is.function(yes) || (is.atomic(yes) && length(yes) == 1))) {
    stop("improper `yes` given")
  }
  if(!isTRUE(is.function(no) || (is.atomic(no) && length(no) == 1))) {
    stop("improper `no` given")
  }
  if(!isTRUE(is.function(other) || (is.atomic(other) && length(other) == 1))) {
    stop("improper `other` given")
  }

  # make & check cond:
  if (is.function(cond)) {
    cond <- cond(x)
  }
  if (!is.logical(cond)) {
    stop(paste0(
      "`cond` must be of class logical,",
      "\n",
      "or a function that returns an object of class logical"
    ))
  }
  if (is.logical(cond)) {
    if (length(cond) != n) {
      stop(paste0(
        "`cond` must be the same length as `x`,",
        "\n",
        "or a function that returns an object with the same length as `x`"
      ))
    }
  }

  # make transformations:
  ind_T <- which(cond)
  x[ind_T] <- .internal_transform_if(yes, x, ind_T)
  ind_F <- which(!cond)
  x[ind_F] <- .internal_transform_if(no, x, ind_F)
  ind_NA <- which(is.na(cond))
  x[ind_NA] <- .internal_transform_if(other, x, ind_NA)

  return(x)
}


.internal_transform_if <- function(f, y, ind) {
  if(is.function(f)) {
    return(f(y[ind]))
  } else {
    return(f)
  }
}

