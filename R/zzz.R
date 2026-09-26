
.pkgenv_tinyimport <- new.env(parent=emptyenv())
for(i in c("import_as", "import_from", "source_as", "source_here")) {
  .pkgenv_tinyimport[[i]] <- new.env(parent = emptyenv())
}

.onLoad <- function(libname, pkgname) {
  
  for(i in c("import_as", "import_from", "source_as", "source_here")) {
    .pkgenv_tinyimport[[i]][["env"]] <- NULL
  }
}

.onAttach <- function(libname, pkgname) {
  txt <- paste0(
    "Run `",
    "?tinycodet::tinycodet",
    "` to open the introduction help page of 'tinycodet'."
  )
  packageStartupMessage(txt)
}
