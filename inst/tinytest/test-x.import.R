
import_as(stri., "stringi")

expect_error(
  attr.import(stri., "foo"),
  pattern = "unknown which given"
)
expect_error(
  attr.import(environment()),
  pattern = "`alias` must be a locked environment as returned by `import_as()`",
  fixed = TRUE
)

expect_error(
  help.import(package = "stringi", alias = stri.),
  pattern = "you cannot provide both `package`/`topic` AND `i`/`alias`"
)
expect_error(
  help.import(i = "string"),
  pattern = "if `i` is specified as a string, `alias` must also be supplied"
)
