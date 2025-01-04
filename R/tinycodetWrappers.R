
#' @keywords Internal
#' @noRd
.C_any_nonpos <- function(x) {
  .Call("C_any_nonpos", x = x)
}

#' @keywords Internal
#' @noRd
.C_any_neg <- function(x) {
  .Call("C_any_neg", x = x)
}

#' @keywords Internal
#' @noRd
.C_any_badloc <- function(x, y) {
  .Call("C_any_badloc", x = x, y = y)
}

#' @keywords Internal
#' @noRd
.C_do_stri_locate_ith0 <- function(p1, i, dims) {
  .Call("C_do_stri_locate_ith0", p1 = p1, i = as.integer(i), dims = as.integer(dims))
}

#' @keywords Internal
#' @noRd
.C_do_stri_locate_ith1 <- function(p1, i, dims) {
  .Call("C_do_stri_locate_ith1", p1 = p1, i = as.integer(i), dims = as.integer(dims))
}

