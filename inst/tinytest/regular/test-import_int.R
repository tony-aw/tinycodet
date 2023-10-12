
# general checks ====
expect_equal(
  import_int(tinycodet ~ .internal_prep_Namespace),
  tinycodet:::.internal_prep_Namespace
)

expect_error(
  import_int(tinycodet ~ .foo),
  pattern = "is not an internal function of"
)

expect_error(
  import_int(tinycodetfakepkg3 ~ .foo),
  pattern = "The following packages are not installed:"
)


# formula error handling ====
expect_error(
  import_int(""),
  pattern = "`form` must be a formula"
)

expect_error(
  import_int(~ .foo),
  pattern = "`form` must be a two-sided formula"
)

expect_error(
  import_int(stringi + gamair ~ foo),
  pattern = "must give a single package"
)

expect_error(
  import_int(tinycodet ~ .internal_prep_Namespace + .internal_check_conflicting_inops),
  pattern = "must give a single internal function"
)


# package error handling ====
expect_error(
  import_int(stringi ~ stri_detect, lib.loc = "foo"),
  pattern = "The following packages are not installed"
)

expect_error(
  import_int(stringi ~ stri_detect, lib.loc = c("foo", "foo")),
  pattern = "The following packages are not installed"
)

expect_error(
  import_int(base ~ loadNamespace),
  pattern = 'The following "packages" are base/core R, which is not allowed:'
)

expect_error(
  import_int(`!foo` ~ foo),
  pattern = "You have misspelled the following"
)

expect_error(
  import_int(stringi ~ .foo, lib.loc = mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)


