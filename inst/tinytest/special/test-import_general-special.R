
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


# functional base functions from separate library ====
# single lib.loc
import_as(.new ~ tinycodetfakepkg5, lib.loc = c("foo1", lib.loc1, "foo2"))
expect_equal(.new$fun_paste("a", "b"), "ab")

temp.fun <- function() {
  ls <- import_ls("tinycodetfakepkg5", "infix", lib.loc = c("foo1", lib.loc1, "foo2"))
  import_from("tinycodetfakepkg5", ls, lib.loc = c("foo1", lib.loc1, "foo2"))
  "a" %paste0% "b"
}
expect_equal(temp.fun(), "ab")
temp.fun <- function() {
  import_from("tinycodetfakepkg5", "fun_paste", lib.loc = c("foo1", lib.loc1, "foo2"))
  fun_paste("a", "b")
}
expect_equal(temp.fun(), "ab")


# multi lib.loc
import_as(.new ~ tinycodetfakepkg5, lib.loc = c("foo", lib.loc1))
expect_equal(.new$fun_paste("a", "b"), "ab")

temp.fun <- function() {
  ls <- import_ls("tinycodetfakepkg5", "infix", lib.loc = c("foo", lib.loc1))
  import_from("tinycodetfakepkg5", ls, lib.loc = c("foo", lib.loc1))
  "a" %paste0% "b"
}
expect_equal(temp.fun(), "ab")
temp.fun <- function() {
  import_from("tinycodetfakepkg5", "fun_paste", lib.loc = c("foo", lib.loc1))
  fun_paste("a", "b")
}
expect_equal(temp.fun(), "ab")







# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false




