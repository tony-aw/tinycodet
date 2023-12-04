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


# .internal_list_* ====
expect_equal(
  sort(tinycodet:::.internal_list_coreR()),
  c(installed.packages(priority = "base") |> rownames(), "translations") |> unique() |> sort()
)
expect_equal(
  sort(tinycodet:::.internal_list_preinst()),
  installed.packages(priority = "recommended") |> rownames() |> sort()
)

n <- length(tinycodet:::.internal_list_tidyshared())
checks <- logical(n)
for(i in 1:n) checks[i] <- tinycodet:::.internal_list_tidyshared()[i] %installed in% .libPaths()
expect_true(all(checks))

n <- length(tinycodet:::.internal_list_knownmeta())
checks <- logical(n)
for(i in 1:n) checks[i] <- tinycodet:::.internal_list_knownmeta()[i] %installed in% .libPaths()
cbind(checks, tinycodet:::.internal_list_knownmeta())


# meta-verse error checks ====
expect_error(
  import_as(~tdy., "tidyverse"),
  pattern = "he following packages are known meta-verse packages, which is not allowed"
)

expect_error(
  import_as(~tny., "fastverse"),
  pattern = "he following packages are known meta-verse packages, which is not allowed"
)
