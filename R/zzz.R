.onAttach <- function(libname, pkgname) {
  txt <- paste0(
    "Run \033[4m",
    '?tinycodet::tinycodet',
    "\033[24m to open the introduction help page of tinycodet."
  )
  packageStartupMessage(txt)
}
