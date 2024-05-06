
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



# test import_inops - basics ====

temp.fun <- function() {
  import_as(
    ~to3., "tinycodetfakepkg3",
    dependencies = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
    lib.loc = lib.loc1
  )
  import_inops(
    expose=to3.
  )
  import_inops(
    unexpose=to3.
  )
  ls() |> sort()
}
expect_equal(temp.fun(), c("to3."))


# infix operators in environment but none in alias ====
temp.fun <- function() {
  import_as(~ to2., "tinycodetfakepkg2", lib.loc = lib.loc1)
  import_inops(to2.)
  import_as(~ to1., "tinycodetfakepkg1", lib.loc = lib.loc1)
  import_inops(
    unexpose = to1.
  )
  ls()
}

temp.fun2 <- function() {
  import_as(~ to2., "tinycodetfakepkg2", lib.loc = lib.loc1)
  import_inops(to2.)
  ls()
}

expect_message(
  temp.fun(),
  pattern = "No infix operators to unexpose",
  fixed = TRUE
)
expect_equal(
  sort(temp.fun()),
  sort(c(temp.fun2(), "to1."))
)


# unexpose infix operators in alias but none in environment ====
temp.fun <- function() {
  import_as(~ to2., "tinycodetfakepkg2", lib.loc = lib.loc1)
  import_inops(
    unexpose = to2.
  )
  ls()
}


expect_message(
  temp.fun(),
  pattern = "No infix operators to unexpose",
  fixed = TRUE
)
expect_equal(
  temp.fun(),
  "to2."
)


# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false
