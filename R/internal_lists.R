


#' @keywords internal
#' @noRd
.list_coreR <- function() {
  out <- c(
    "base", "compiler", "datasets", "grDevices", "graphics", "grid", "methods",
    "parallel", "splines", "stats", "stats4", "tcltk", "tools",
    "translations", "utils"
  )
  return(out)
}


#' @keywords internal
#' @noRd
.list_preinst <- function() {
  out <- c(
    "boot", "class", "cluster", "codetools", "foreign", "KernSmooth",
    "lattice", "MASS", "Matrix",  "mgcv", "nlme", "nnet",
    "rpart", "spatial", "survival"
  )
  return(out)
}


#' @keywords internal
#' @noRd
.list_semi <- function() {
  out <- c(
    "S7", "rstudioapi"
  )
  return(out)
}



#' @keywords internal
#' @noRd
.list_tidyshared <- function() {
  out <- c(
    "rlang", "lifecycle", "cli", "glue", "withr"
  )
  return(out)
}


#' @keywords internal
#' @noRd
.list_knownmeta <- function() {
  out <- c(
    "tidyverse", "fastverse", "tinyverse"
  )
  return(out)
}


