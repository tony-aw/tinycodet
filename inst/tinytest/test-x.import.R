
import_as(~stri., "stringi")

# attr.import ====
myattr <- attr.import(stri.)
expect_equal(
  myattr$tinyimport,
  "tinyimport"
)
expect_equal(
  attr.import(stri., "pkgs"),
  myattr$pkgs
)
expect_equal(
  attr.import(stri., "conflicts"),
  myattr$conflicts
)
expect_equal(
  attr.import(stri., "versions"),
  myattr$versions
)
expect_equal(
  attr.import(stri., "args"),
  myattr$args
)
expect_equal(
  attr.import(stri., "ordered_object_names"),
  myattr$ordered_object_names
)
expect_error(
  attr.import(stri., "foo"),
  pattern = "unknown which given"
)
expect_error(
  attr.import(environment()),
  pattern = "`alias` must be a locked environment as returned by `import_as()`",
  fixed = TRUE
)


# help.import ====
expect_error(
  help.import(package = "stringi", alias = stri.),
  pattern = "you cannot provide both `package`/`topic` AND `i`/`alias`"
)
expect_error(
  help.import(i = "string"),
  pattern = "if `i` is specified as a string, `alias` must also be supplied"
)

# is.tinyimport ====
expect_true(is.tinyimport(stri.))
expect_false(is.tinyimport(environment()))

tempfun <- function() {
  import_inops(expose = "stringi")
  return(is.tinyimport(`%stri===%`))
}
expect_true(tempfun())

tempfun <- function() {
  import_LL("stringi", "stri_detect")
  return(is.tinyimport(stri_detect))
}
expect_true(tempfun())

tempfun <- function() {
  `%stri===%` <- stringi:::`%stri===%`
  return(is.tinyimport(`%stri===%`))
}
expect_false(tempfun())
