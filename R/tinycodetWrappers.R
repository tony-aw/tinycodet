
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