
# get library folder:
lib.loc <- file.path(getwd(), "fake_library")
print(lib.loc)


# test %installed in% operator:
expect_false("foo" %installed in% lib.loc)
expect_true("tinycodetfakepkg1" %installed in% lib.loc)

# test pkg_get_deps:
expect_equal(
  pkg_get_deps("tinycodetfakepkg1", lib.loc, deps_type = "Enhances"),
  "tinycodetfakepkg3"
)
expect_equal(
  pkg_get_deps("tinycodetfakepkg2", lib.loc, deps_type = "Enhances"),
  "tinycodetfakepkg3"
)
expect_equal(
  pkg_get_deps("tinycodetfakepkg3", lib.loc, deps_type = "Depends"),
  "tinycodetfakepkg1"
)
expect_equal(
  pkg_get_deps("tinycodetfakepkg3", lib.loc, deps_type = "Imports"),
  "tinycodetfakepkg2"
)


