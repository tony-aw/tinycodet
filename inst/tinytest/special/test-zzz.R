# remove fake package files from temporary directory:
to.dir <- tempdir() |> normalizePath()
dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
unlink(dir2remove, recursive = TRUE, force = TRUE)

expect_false(any(file.exists(dir2remove)))
