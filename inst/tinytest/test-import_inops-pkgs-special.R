
# get library folder:
lib.loc <- file.path(getwd(), "fake_library")
print(lib.loc)


# test import_inops - basics ====
temp.fun <- function() {
  import_inops(
    c("tinyoperationsfakepkg1"),
    lib.loc = lib.loc
  )
  ls()
}
expect_equal(
  temp.fun(), c("%op11%", "%op12%", "%opover%")
)


# include.only ====
temp.fun <- function() {
  import_inops(
    c("tinyoperationsfakepkg1"),
    include.only = c("%op12%"),
    lib.loc = lib.loc
  )
  ls()
}
expect_equal(
  temp.fun(), c("%op12%")
)


# exclude ====
temp.fun <- function() {
  import_inops(
    c("tinyoperationsfakepkg1"),
    exclude = c("%op12%"),
    lib.loc = lib.loc
  )
  ls()
}
expect_equal(
  temp.fun(), c("%op11%", "%opover%")
)


temp.fun <- function() {
  pkgs_in <- c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2", "tinyoperationsfakepkg3")
  pkgs_out <- c("tinyoperationsfakepkg2", "tinyoperationsfakepkg3")
  import_inops(pkgs_in[1], lib.loc = lib.loc)
  import_inops(pkgs_in[2], lib.loc = lib.loc)
  import_inops(pkgs_in[3], lib.loc = lib.loc)
  import_inops(unexpose = pkgs_out[1], lib.loc = lib.loc)
  import_inops(unexpose = pkgs_out[2], lib.loc = lib.loc)
  rm(list = c("pkgs_in", "pkgs_out"))
  ls()
}
expect_equal(temp.fun(), c("%op11%", '%op12%'))



# import_inops but no infix operators ====
temp.fun <- function() {
  import_inops(
    c("tinyoperationsfakepkg1"),
    include.only = c("fun11"),
    lib.loc = lib.loc
  )
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`include.only` must be names of infix operators"
)

temp.fun <- function() {
  import_inops(
    c("tinyoperationsfakepkg1"),
    include.only = c("%foo%"),
    lib.loc = lib.loc
  )
  ls()
}
expect_message(
  temp.fun(),
  pattern = "No infix operators to expose"
)


temp.fun <- function() {
  import_inops(
    unexpose = c("tinyoperationsfakepkg1"),
    lib.loc = lib.loc
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
  import_inops("stringi", lib.loc=lib.loc),
  pattern = "The following packages are not installed"
)


# test misc attributes ====
temp.fun <- function() {
  import_inops(
    "tinyoperationsfakepkg3",
    lib.loc=lib.loc
  )
  return(attributes(`%op31%`)|> names())
}
expect_true("tinyimport" %in% temp.fun())

