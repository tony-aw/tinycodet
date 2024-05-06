
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
  import_inops(
    expose = "tinycodetfakepkg3", lib.loc = lib.loc1
  )
  import_inops(
    unexpose = "tinycodetfakepkg3", lib.loc = lib.loc1
  )
  ls() |> sort()
}
expect_equal(temp.fun(), character(0))


# infix operators in environment but none in package ====
temp.fun <- function() {
  import_inops("tinycodetfakepkg2", lib.loc = lib.loc1)
  import_inops(unexpose = "tinycodetfakepkg1", lib.loc = lib.loc1)
  ls()
}

temp.fun2 <- function() {
  import_inops("tinycodetfakepkg2", lib.loc = lib.loc1)
  ls()
}

expect_message(
  temp.fun(),
  pattern = "No infix operators to unexpose",
  fixed = TRUE
)
expect_equal(
  sort(temp.fun()),
  sort(temp.fun2())
)


# unexpose infix operators in alias but none in environment ====
temp.fun <- function() {
  import_inops(
    unexpose = "tinycodetfakepkg2", lib.loc = lib.loc1
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
  character(0)
)



# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false
