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
  import_as(~ rsa., "rstudioapi", extensions = "knitr"),
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
attr.import(mr., "args")
attr.import(mr., "ordered_object_names")
expect_error(
  attr.import(mr., "foo"),
  pattern = "unknown `which` given"
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
cbind(checks, tinycodet:::.internal_list_knownmeta()) |> print()



# empty packages checks ====
expect_warning(
  tinycodet:::.internal_prep_Namespace("spam64", .libPaths(), sys.call()),
  pattern = "the package `spam64` has no exported functions"
)

expect_warning(
  import_as(~ sp64., "spam64"),
  pattern = "the package `spam64` has no exported functions"
)

expect_warning(
  import_inops("spam64"),
  pattern = "the package `spam64` has no exported functions"
)

expect_warning(
  import_LL("spam64", 'foo'),
  pattern = "the package `spam64` has no exported functions"
)


# pversion checks ====
loadNamespace("boot")
templib <- tempdir()
remotes::install_version("boot", "1.3-25", lib = templib)

check <- pversion_check4mismatch("boot", lib.loc = templib)
expect_true(nrow(check) == 1)
expect_equal(
  lapply(check, class),
  list("package" = "character",
       "version_loaded" = "character",
       "version_lib.loc" = "character"
  )
)
expect_true(
  utils::compareVersion(check$version_loaded, check$version_lib.loc) == 1
)

unloadNamespace("boot")
remove.packages("boot", lib = templib)

dir2remove <- file.path(templib, list.files(templib)) |> normalizePath()
file.remove(dir2remove)
unlink(dir2remove, recursive = TRUE, force = TRUE)
expect_false(any(file.exists(dir2remove)))
