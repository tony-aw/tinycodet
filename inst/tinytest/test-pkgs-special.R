
# get library folder:
lib.loc <- file.path(getwd(), "fake_library")
print(lib.loc)


# test %installed in% operator:
expect_false("foo" %installed in% lib.loc)
expect_true("tinyoperationsfakepkg1" %installed in% lib.loc)

# test pkg_get_deps:
expect_equal(
  pkg_get_deps("tinyoperationsfakepkg1", lib.loc, deps_type = "Enhances"),
  "tinyoperationsfakepkg3"
)
expect_equal(
  pkg_get_deps("tinyoperationsfakepkg2", lib.loc, deps_type = "Enhances"),
  "tinyoperationsfakepkg3"
)
expect_equal(
  pkg_get_deps("tinyoperationsfakepkg3", lib.loc, deps_type = "Depends"),
  "tinyoperationsfakepkg1"
)
expect_equal(
  pkg_get_deps("tinyoperationsfakepkg3", lib.loc, deps_type = "Imports"),
  "tinyoperationsfakepkg2"
)


