
# get library folder:
lib.loc <- file.path(getwd(), "fake_library")
print(lib.loc)


# test %installed in% operator:
expect_equal(
  "foo" %installed in% lib.loc,
  setNames(FALSE, 'foo')
)
expect_equal(
  "tinycodetfakepkg1" %installed in% lib.loc,
  setNames(TRUE, "tinycodetfakepkg1")
)
expect_error(
  "!@#$%^&*()" %installed in% lib.loc,
  pattern = "You have misspelled the following packages:"
)
expect_error(
  "tinycodetfakepkg1" %installed in% mean,
  pattern = "`lib.loc` must be a character vector with at least one library path"
)


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
expect_error(
  pkg_get_deps("!@#$%^&*()", lib.loc = lib.loc),
  pattern = "You have misspelled the following packages:"
)
expect_error(
  pkg_get_deps("tinycodetfakepkg1", lib.loc = mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  pkg_get_deps("foo", lib.loc = lib.loc),
  pattern = "The following packages are not installed"
)


# test pkg_lsf:
ns <- loadNamespace("stringi") |> as.list(all.names=TRUE, sorted=TRUE)
names_exported <- names(ns[[".__NAMESPACE__."]][["exports"]])
ns <- ns[names_exported]
ns <- ns[!is.na(names(ns))]
names_exported <- names(ns)
inops <- grep("%|:=", names_exported, value = TRUE)
regfuns <- grep("%|:=", names_exported, value = TRUE, invert = TRUE)
expect_equal(
  pkg_lsf("stringi", "inops"),
  inops
)
expect_equal(
  pkg_lsf("stringi", "regfuns"),
  regfuns
)
expect_error(
  pkg_lsf("!@#$%^&*()", "inops", lib.loc = lib.loc),
  pattern = "You have misspelled the following packages:"
)
expect_error(
  pkg_lsf("tinycodetfakepkg1", "inops", lib.loc = mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  pkg_get_deps("foo", "inops", lib.loc = lib.loc),
  pattern = "The following packages are not installed"
)

