
# test import_as - single package ====
stri <- loadNamespace("stringi") |> getNamespaceExports()
temp.fun <- function() {
  import_as(~stri., "stringi")
  out <- setdiff(names(stri.), ".__attributes__.") |> sort()
  return(out)
}
expect_equal(temp.fun(), sort(stri))


# test import_as - functional functions ====
temp.fun <- function() {
  import_as(~stri., "stringi")
  stri.$stri_c("a", "b")
}
expect_equal(temp.fun(), "ab")


# main_package error handling ====
expect_error(
  import_as(~stri., c("stringi", "tinycodet")),
  pattern = "`main_package` must be a single string"
)

expect_error(
  import_as(~stri., ~ stringi),
  pattern = "`main_package` must be a single string"
)

expect_error(
  import_as(~stri., "stringi", lib.loc="foo"),
  pattern = "The following packages are not installed"
)

expect_error(
  import_as(~stri., ""),
  pattern = "You have misspelled the following"
)

expect_error(
  import_as(~stri., "!@#$%^&*()"),
  pattern = "You have misspelled the following"
)

expect_error(
  import_as(~stri., "base"),
  pattern = 'The following "packages" are base/core R, which is not allowed:'
)


# alias error handling ====
expect_error(
  import_as(~ stri. + foo., "stringi"),
  pattern = "when `alias` is a formula, it must have 1 term"
)
expect_error(
  import_as(c("stri.", "foo."), "stringi"),
  pattern = "when `alias` is a character, it must be a single string"
)
expect_error(
  import_as("!@#$%^&*()", "stringi"),
  pattern = "Syntactically invalid name for object `alias`"
)
expect_error(
  import_as(~ .__foo__., "stringi"),
  pattern = "Syntactically invalid name for object `alias`"
)


# dependencies/extensions basic error handling ====
expect_error(
  import_as(~stri., "stringi", dependencies = ~foo),
  pattern = "`dependencies` must be a character vector",
  fixed = TRUE
)
expect_error(
  import_as(~stri., "stringi", dependencies = character(0)),
  pattern = "`dependencies` must be a character vector",
  fixed = TRUE
)

expect_error(
  import_as(~stri., "stringi", extensions = ~foo),
  pattern = "`extensions` must be a character vector",
  fixed = TRUE
)
expect_error(
  import_as(~stri., "stringi", extensions = character(0)),
  pattern = "`extensions` must be a character vector",
  fixed = TRUE
)

expect_error(
  import_as(~stri., "stringi", dependencies = character(11)),
  pattern = "more than 10 packages not allowed to be imported under a single alias",
  fixed = TRUE
)
expect_error(
  import_as(~stri., "stringi", extensions = character(11)),
  pattern = "more than 10 packages not allowed to be imported under a single alias",
  fixed = TRUE
)


# other error handling ====
expect_error(
  import_as(~stri., "stringi", re_exports = NA),
  pattern = "`re_exports` must be either `TRUE` or `FALSE`"
)

expect_error(
  import_as(~stri., "stringi", re_exports = c(TRUE, FALSE)),
  pattern = "`re_exports` must be either `TRUE` or `FALSE`"
)

expect_error(
  import_as(~stri., "stringi", import_order = letters[1:4]),
  pattern = "Improper `import_order` given"
)

expect_error(
  import_as(~stri., "stringi", import_order = c("main_package", "dependencies")),
  pattern = "Improper `import_order` given"
)

expect_error(
  import_as(~stri., "stringi", lib.loc=mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)

temp.fun <- function(){
  import_as(~stri., "stringi")
  stri_c("a", "b")
}
expect_error(temp.fun(), "could not find function")

