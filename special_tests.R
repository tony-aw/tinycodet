# special _import_as() tests
library(tinycodet)
library(tinytest)

# import_as ====
import_as(~ dpr., "dplyr", re_exports = TRUE)
out <- setdiff(names(dpr.), ".__attributes__.") |> sort()
foo <- loadNamespace("dplyr") |> getNamespaceExports()
check <- all(out == sort(foo))
expect_true(check)

expect_error(
  import_as(~ ms., "MASS", extensions = "ggplot2"),
  "The following given extensions were not found to be actual extensions:",
  fixed = TRUE
)

expect_error(
  import_as(~ rl., "rlang", extensions = "ggplot2"),
  "The following given extensions were not found to be actual extensions:",
  fixed = TRUE
)

expect_error(
  import_as(~ rsa., "rstudioapi", extensions = "tidyverse"),
  "The following given extensions were not found to be actual extensions:",
  fixed = TRUE
)


# is.tinyimport & help.import() ====
import_inops("magrittr")
import_as(~mr., "magrittr")
`:=` <- data.table::`:=`
expect_true(is.tinyimport(mr.))
expect_true(is.tinyimport(`%>%`))
expect_false(is.tinyimport(`:=`))
alias_attr <- mr.$.__attributes__.
str(alias_attr)
attr.import(mr., "pkgs")
attr.import(mr., "conflicts")
attr.import(mr., "versions")
attr.import(mr., "args")
attr.import(mr., "ordered_object_names")
expect_error(
  attr.import(mr., "foo"),
  pattern = "unknown which given"
)
help.import(i = `%>%`)
help.import(i = mr.$multiply_by)
help.import(i = "%>%", alias=mr.)
help.import(i = "add", alias=mr.)

