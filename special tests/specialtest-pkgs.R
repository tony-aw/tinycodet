
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

# test %installed in% operator:
expect_false("foo" %installed in% lib.loc) |> errorfun()
expect_true("tinyoperationsfakepkg1" %installed in% lib.loc) |> errorfun()

# test pkg_get_deps:
expect_equal(
  pkg_get_deps("tinyoperationsfakepkg1", lib.loc, deps_type = "Enhances"),
  "tinyoperationsfakepkg3"
) |> errorfun()
expect_equal(
  pkg_get_deps("tinyoperationsfakepkg2", lib.loc, deps_type = "Enhances"),
  "tinyoperationsfakepkg3"
) |> errorfun()
expect_equal(
  pkg_get_deps("tinyoperationsfakepkg3", lib.loc, deps_type = "Depends"),
  "tinyoperationsfakepkg1"
) |> errorfun()
expect_equal(
  pkg_get_deps("tinyoperationsfakepkg3", lib.loc, deps_type = "Imports"),
  "tinyoperationsfakepkg2"
) |> errorfun()


