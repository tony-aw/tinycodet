
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
  "tinycodetfakepkg1" %in% loadedNamespaces()
)


# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false
