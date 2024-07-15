
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
  attr.import(stri., "args"),
  myattr$args
)
expect_equal(
  attr.import(stri., "ordered_object_names"),
  myattr$ordered_object_names
)
expect_error(
  attr.import(stri., "foo"),
  pattern = "unknown `which` given"
)
expect_error(
  attr.import(environment()),
  pattern = "`alias` must be a locked environment as returned by `import_as()`",
  fixed = TRUE
)


# help.import - errors ====
# NOTE: non-error checks are performed in a separate script, and these checks are performed manually.
expect_error(
  help.import(i = letters),
  pattern = "`i` must be a function or a single string"
)
expect_error(
  help.import(i = ~ a),
  pattern = "`i` must be a function or a single string"
)
expect_error(
  help.import(package = "stringi", alias = stri.),
  pattern = "you cannot provide both `package`/`topic` AND `i`/`alias`"
)
expect_error(
  help.import(topic = "deparse", i = "deparse"),
  pattern = "you cannot provide both `package`/`topic` AND `i`/`alias`"
)
expect_error(
  help.import(i = "string"),
  pattern = "if `i` is specified as a string, `alias` must also be supplied"
)
expect_error(
  help.import(i = "stri_c", alias = as.list(stri.)),
  pattern = "`alias` must be a package alias object"
)
stri2 <- loadNamespace("stringi")
expect_error(
  help.import(i = "stri_c", alias = stri2),
  pattern = "`alias` must be a package alias object"
)


# is.tinyimport - error checks ====
# set-up
strip.attributes <- function(x) {
  attributes(x) <- NULL
  return(x)
}

expect_error(
  is.tinyimport(environment()),
  pattern = "only assigned objects (variables/constants) can be checked",
  fixed = TRUE
)

# is.tinyimport - alias checks ====
import_as(~stri., "stringi")
expect_true(is.tinyimport(stri.))
expect_true(tinycodet:::.is.tinyalias("stri.", environment()))
ns.stri <- loadNamespace("stringi")
expect_false(is.tinyimport(ns.stri))
expect_false(tinycodet:::.is.tinyalias("ns.stri", environment()))
stri. <- as.list(stri., all.names = TRUE) |> as.environment()
lockEnvironment(stri., bindings = TRUE)
expect_false(is.tinyimport(stri.))
expect_false(tinycodet:::.is.tinyalias("stri.", environment()))


# is.tinyimport - inops checks ====
tempfun <- function() {
  `%stri===%` <- stringi::`%stri===%`
  return(is.tinyimport(`%stri===%`))
}
expect_false(tempfun())

tempfun <- function() {
  `%stri===%` <- stringi::`%stri===%`
  return(tinycodet:::.is.tinyinop("%stri===%", environment()))
}
expect_false(tempfun())


# is.tinyimport - inops via package checks ====
tempfun <- function() {
  import_inops(expose = "stringi")
  return(is.tinyimport(`%stri===%`))
}
expect_true(tempfun())

tempfun <- function() {
  import_inops(expose = "stringi")
  return(tinycodet:::.is.tinyinop("%stri===%", environment()))
}
expect_true(tempfun())

tempfun <- function() {
  import_inops(expose = "stringi")
  `%stri===%` <- strip.attributes(`%stri===%`)
  return(is.tinyimport(`%stri===%`))
}
expect_false(tempfun())

tempfun <- function() {
  import_inops(expose = "stringi")
  `%stri===%` <- strip.attributes(`%stri===%`)
  return(tinycodet:::.is.tinyinop("%stri===%", environment()))
}
expect_false(tempfun())


# is.tinyimport - inops via alias checks ====
import_as(~stri., "stringi")
tempfun <- function() {
  import_as(~ stri., "stringi")
  import_inops(expose = stri.)
  return(is.tinyimport(`%stri===%`))
}
expect_true(tempfun())

tempfun <- function() {
  import_as(~ stri., "stringi")
  import_inops(expose = stri.)
  return(tinycodet:::.is.tinyinop("%stri===%", environment()))
}
expect_true(tempfun())

import_as(~stri., "stringi")
tempfun <- function() {
  import_as(~ stri., "stringi")
  import_inops(expose = stri.)
  `%stri===%` <- strip.attributes(`%stri===%`)
  return(is.tinyimport(`%stri===%`))
}
expect_false(tempfun())

tempfun <- function() {
  import_as(~ stri., "stringi")
  import_inops(expose = stri.)
  `%stri===%` <- strip.attributes(`%stri===%`)
  return(tinycodet:::.is.tinyinop("%stri===%", environment()))
}
expect_false(tempfun())


# is.tinyimport - LL checks ====
tempfun <- function() {
  import_LL("stringi", "stri_detect")
  return(is.tinyimport(stri_detect))
}
expect_true(tempfun())

tempfun <- function() {
  stri_detect <- stringi::stri_detect
  lockBinding("stri_detect", environment())
  return(is.tinyimport(stri_detect))
}
expect_false(tempfun())

tempfun <- function() {
  `%stri===%` <- stringi:::`%stri===%`
  return(is.tinyimport(`%stri===%`))
}
expect_false(tempfun())


