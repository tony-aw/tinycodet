
# set-up ====
from.dir <- file.path(getwd(), "fakelibs")
to.dir <- tempdir() |> normalizePath()
# tinycodet:::.create_fake_packages(from.dir, to.dir)
lib.loc1 <- file.path(to.dir, "fake_lib1")
lib.loc2 <- file.path(to.dir, "fake_lib2")
lib.loc3 <- file.path(to.dir, "fake_lib3")
lib.locnew <- file.path(to.dir, "newlib")
print(lib.loc1)
print(lib.loc2)
print(lib.loc3)
print(lib.locnew)

# this is to be checked BEFORE loading tinycodetfakepkg1
# (hence the "aaa" in the test file name)

pkg <- "tinycodetfakepkg1"

expect_false(
  pkg %in% loadedNamespaces()
)

out <-  as.character(tinycodet:::.pversion_installed(pkg, c("foo1", lib.loc1, "foo2")))
expected <- as.character(utils::packageVersion(pkg, c("foo1", lib.loc1, "foo2")))
check <- utils::compareVersion(out, expected)

expect_true(
  check == 0L
)

expect_false(
  pkg  %in% loadedNamespaces()
)



loadNamespace(pkg, lib.loc = lib.loc1)
check <- pversion_check4mismatch(pkg, lib.loc = c(lib.locnew, .libPaths()))
expect_true(nrow(check) == 1)
expect_equal(
  lapply(check, class),
  list("package" = "character",
       "version_loaded" = "character",
       "version_lib.loc" = "character"
  )
)
expect_true(
  utils::compareVersion(check$version_loaded, check$version_lib.loc) == -1
)
expect_true(
  utils::compareVersion(check$version_lib.loc, "1.0") == 0
)
expect_true(
  utils::compareVersion(check$version_loaded, "0.0.0.9000") == 0
)
expect_true(
  utils::compareVersion(check$version_loaded, getNamespaceVersion(pkg)) == 0
)
expect_false(
  pversion_report(pkg, c(lib.locnew, lib.loc1))$versions_equal
)
expect_true(
  pversion_report(pkg, c(lib.loc1, lib.locnew))$versions_equal
)

expect_null(
  pversion_check4mismatch(pkg, lib.loc = c(lib.loc1, lib.locnew))
)


# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false
