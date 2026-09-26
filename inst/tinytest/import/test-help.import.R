
import_as(.stri ~ stringi)


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
  help.import(package = "stringi", alias = .stri),
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
  help.import(i = "stri_c", alias = as.list(.stri)),
  pattern = "`alias` must be a package alias object"
)
stri2 <- loadNamespace("stringi")
expect_error(
  help.import(i = "stri_c", alias = stri2),
  pattern = "`alias` must be a package alias object"
)

