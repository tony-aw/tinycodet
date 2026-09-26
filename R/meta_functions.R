#' Internal functions
#'
#'
#'
#'
#'
#'
#' @keywords internal
#' @noRd
.internal_paste <- function(e1, e2) {
  return(paste0(e1, e2))
}

#' @keywords internal
#' @noRd
.mybadge_import <- function(x, y, color) {
  filepath <- paste0(gsub(" ", "", x), "-",
                     y, "-", color, ".svg")
  text <- sprintf("\\link[=tinycodet_import]{%s}: %s; ", x, y)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    filepath, toupper(y))
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}

#' @keywords internal
#' @noRd
.mybadge_string <- function(x, color) {
  filepath <- paste0("aboutsearch", "-", x, "-", color, ".svg")
  url <- paste0("https://stringi.gagolewski.com/rapi/about_search_", x, ".html")
  text <- sprintf("\\href{%s}{about search: %s}", url, x)
  html <- sprintf(
    "\\href{%s}{\\figure{%s}{options: alt='[%s]'}}",
    url, filepath, toupper(x))
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


.txt_searchenv_forbidden <- function() {
  txt <- c(
    " - a package path (their names start with 'package:')",
    " - a tools path (their names start with 'tools:')",
    " - the Global environment ('.GlobalEnv')",
    " - autoloads search path ('Autoloads')",
    " - a path at position 1 or `length(search())`"
  )
  txt <- paste0(txt, collapse = "\n")
  return(txt)
}

#' @keywords internal
#' @noRd
.create_fake_packages <- function(from.dir, to.dir) {
  file.copy(list.files(from.dir, full.names = TRUE),
            to.dir, recursive = TRUE)
  for(i in paste0("fake_lib", 1:3)) {
    for(j in paste0("tinycodetfakepkg", 1:5)) {
      if(dir.exists(file.path(to.dir, i, j))) {
        print(file.path(to.dir, i, j))
        dir2rename <- file.path(to.dir, i, j, "Poof")
        newdirname <- file.path(to.dir, i, j, "Meta")
        file.rename(dir2rename, newdirname)
      }
    }
  }
  
  i <- "newlib"
  j <- "tinycodetfakepkg1"
  print(file.path(to.dir, i, j))
  dir2rename <- file.path(to.dir, i, j, "Poof")
  newdirname <- file.path(to.dir, i, j, "Meta")
  file.rename(dir2rename, newdirname)
  
  
}



#' @keywords internal
#' @noRd
.as_bool <- function(x, ...) {
  
  out <- as.logical(x, ...)
  .typecast_attr(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
.as_int <- function(x, ...) {
  
  out <- as.integer(x, ...)
  .typecast_attr(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
.as_dbl <- function(x, ...) {
  
  out <- as.double(x, ...)
  .typecast_attr(out) <- x
  return(out)
}

#' @keywords internal
#' @noRd
.as_num <- .as_dbl


#' @keywords internal
#' @noRd
.as_chr <- function(x, ...) {
  
  out <- as.character(x, ...)
  .typecast_attr(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
.as_str <- .as_chr



#' @keywords internal
#' @noRd
.as_cplx <- function(x, ...) {
  
  out <- as.complex(x, ...)
  .typecast_attr(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
.as_raw <- function(x, ...) {
  
  out <- as.raw(x, ...)
  .typecast_attr(out) <- x
  return(out)
}


#' @keywords internal
#' @noRd
`.typecast_attr<-` <- function(x, value) {
  if(length(value) == length(x)) {
    dim(x) <- dim(value)
    dimnames(x) <- dimnames(value)
    names(x) <- names(value)
    comment(x) <- comment(value)
  }
  x
}



