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
  paste0(e1, e2)
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

#' @keywords internal
#' @noRd
.create_fake_packages <- function(from.dir, to.dir) {
  file.copy(list.files(from.dir, full.names = TRUE),
            to.dir, recursive = TRUE)
  for(i in paste0("fake_lib", 1:3)) {
    for(j in paste0("tinycodetfakepkg", 1:3)) {
      if(dir.exists(file.path(to.dir, i, j))) {
        print(file.path(to.dir, i, j))
        dir2rename <- file.path(to.dir, i, j, "Poof")
        newdirname <- file.path(to.dir, i, j, "Meta")
        file.rename(dir2rename, newdirname)
      }
    }
  }
}
