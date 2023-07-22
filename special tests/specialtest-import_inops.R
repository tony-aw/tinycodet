
# THIS SCRIPT IS TO BE SOURCED FROM ELSEWHERE

library(tinyoperations)
library(tinytest)

# get library folder:
lib.loc <- file.path(getwd(), "fake_library")
print(lib.loc)


# error print function:
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

# test import_inops - basics
temp.fun <- function() {
  import_inops(
    c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2"),
    lib.loc = lib.loc
  )
  ls()
}
expect_equal(
  temp.fun(), c("%op11%", "%op12%", "%op21%", "%op22%", "%opover%")
) |> errorfun()

temp.fun <- function() {
  pkgs_in <- c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2", "tinyoperationsfakepkg3")
  pkgs_out <- c("tinyoperationsfakepkg2", "tinyoperationsfakepkg3")
  import_inops(pkgs_in, lib.loc = lib.loc)
  import_inops(delete = pkgs_out, lib.loc = lib.loc)
  rm(list = c("pkgs_in", "pkgs_out"))
  ls()
}
expect_equal(temp.fun(), c("%op11%", '%op12%')) |> errorfun()

# test import_as - error handling:
expect_error(
  import_inops("stringi", lib.loc=lib.loc),
  pattern = "The following packages are not installed"
) |> errorfun()


# test misc attributes:
temp.fun <- function() {
  import_inops(
    "tinyoperationsfakepkg3",
    lib.loc=lib.loc
  )
  return(attributes(`%op31%`)|> names())
}
expect_true("tinyimport" %in% temp.fun()) |> errorfun()
