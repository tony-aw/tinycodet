
# set-up ====
from.dir <- file.path(getwd(), "fakelibs")
to.dir <- tempdir() |> normalizePath()
# tinycodet:::.create_fake_packages(from.dir, to.dir)
lib.loc1 <- file.path(to.dir, "fake_lib1")
lib.loc2 <- file.path(to.dir, "fake_lib2")
lib.loc3 <- file.path(to.dir, "fake_lib3")
print(lib.loc1)
print(lib.loc2)
print(lib.loc3)


# import_as ====
import_as(
  ~ p3., "tinycodetfakepkg3",
  re_exports = FALSE,
  dependencies = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
  lib.loc = lib.loc1
)
checklist <- as.list(p3.)
checklist <- checklist[order(names(checklist))]
expect <- rep("tinycodetfakepkg3", length(checklist))
expect[stringi::stri_detect(names(checklist), regex = "1\\d")] <- "tinycodetfakepkg1"
expect[stringi::stri_detect(names(checklist), regex = "2\\d")] <- "tinycodetfakepkg2"

out <- sapply(checklist, \(x)attr(x, "package")) |> unname()

expect_equal(
  expect, out
)


# import_inops ====

tempfun <- function() {
  import_as(
    ~ p3., "tinycodetfakepkg3",
    re_exports = FALSE,
    dependencies = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
    lib.loc = lib.loc1
  )
  import_inops(p3.)
  funnames <- setdiff(ls(), "p3.")
  funs <- mget(funnames, mode = "function")
  out <- sapply(funs, \(x)attr(x, "package"))
  return(out)
}

expect <- c(
  rep("tinycodetfakepkg1", 2),
  rep("tinycodetfakepkg2", 2),
  rep("tinycodetfakepkg3", 3)
)
out <- tempfun() |> unname()

expect_equal(expect, out)


# import_LL ====
tempfun <- function() {
  import_LL("tinycodetfakepkg1", "fun11", lib.loc = lib.loc1)
  import_LL("tinycodetfakepkg2", "fun21", lib.loc = lib.loc1)
  import_LL("tinycodetfakepkg3", "fun31", lib.loc = lib.loc1)
  out <- c(attr(fun11, "package"), attr(fun21, "package"), attr(fun31, "package"))
  return(out)
}

expect <- c("tinycodetfakepkg1", "tinycodetfakepkg2", "tinycodetfakepkg3")

out <- tempfun()

expect_equal(expect, out)



# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false




