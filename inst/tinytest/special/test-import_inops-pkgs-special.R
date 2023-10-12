
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
    c("tinycodetfakepkg1"),
    lib.loc = lib.loc1
  )
  ls()
}
expect_equal(
  temp.fun(), c("%op11%", "%op12%", "%opover%")
)


# include.only ====
temp.fun <- function() {
  import_inops(
    c("tinycodetfakepkg1"),
    include.only = c("%op12%"),
    lib.loc = lib.loc1
  )
  ls()
}
expect_equal(
  temp.fun(), c("%op12%")
)


# exclude ====
temp.fun <- function() {
  import_inops(
    c("tinycodetfakepkg1"),
    exclude = c("%op12%"),
    lib.loc = lib.loc1
  )
  ls()
}
expect_equal(
  temp.fun(), c("%op11%", "%opover%")
)


temp.fun <- function() {
  pkgs_in <- c("tinycodetfakepkg1", "tinycodetfakepkg2", "tinycodetfakepkg3")
  pkgs_out <- c("tinycodetfakepkg2", "tinycodetfakepkg3")
  import_inops(pkgs_in[1], lib.loc = lib.loc1)
  import_inops(pkgs_in[2], lib.loc = lib.loc1)
  import_inops(pkgs_in[3], lib.loc = lib.loc1)
  import_inops(unexpose = pkgs_out[1], lib.loc = lib.loc1)
  import_inops(unexpose = pkgs_out[2], lib.loc = lib.loc1)
  rm(list = c("pkgs_in", "pkgs_out"))
  ls()
}
expect_equal(temp.fun(), c("%op11%", '%op12%'))



# import_inops but no infix operators ====
temp.fun <- function() {
  import_inops(
    c("tinycodetfakepkg1"),
    include.only = c("fun11"),
    lib.loc = lib.loc1
  )
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`include.only` must be names of infix operators"
)

temp.fun <- function() {
  import_inops(
    c("tinycodetfakepkg1"),
    include.only = c("%foo%"),
    lib.loc = lib.loc1
  )
  ls()
}
expect_message(
  temp.fun(),
  pattern = "No infix operators to expose"
)


temp.fun <- function() {
  import_inops(
    unexpose = c("tinycodetfakepkg1"),
    lib.loc = lib.loc1
  )
  ls()
}
expect_message(
  temp.fun(),
  pattern = "No infix operators from `import_inops()` to delete",
  fixed = TRUE
)
expect_equal(
  temp.fun(),
  character(0)
)


# test import_as - error handling ====
expect_error(
  import_inops("stringi", lib.loc=lib.loc1),
  pattern = "The following packages are not installed"
)


# test misc attributes ====
temp.fun <- function() {
  import_inops(
    "tinycodetfakepkg3",
    lib.loc=lib.loc1
  )
  return(attributes(`%op31%`)|> names())
}
expect_true("tinyimport" %in% temp.fun())


# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false
