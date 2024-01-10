.onAttach <- function(libname, pkgname) {
  txt <- paste0(
    "Run `",
    "?tinycodet::tinycodet",
    "` to open the introduction help page of 'tinycodet'."
  )
  packageStartupMessage(txt)
}
