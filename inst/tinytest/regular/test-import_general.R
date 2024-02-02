
# set-up ====
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# package not installed ====
expect_error(
  import_as(~stri., "stringi", lib.loc="foo"),
  pattern = "The following packages are not installed"
)
expect_error(
  import_inops("stringi", lib.loc="foo"),
  pattern = "The following packages are not installed"
)
expect_error(
  import_LL("stringi", lib.loc="foo", selection = "foo"),
  pattern = "The following packages are not installed"
)
expect_error(
  import_int(stringi ~ foo, lib.loc="foo"),
  pattern = "The following packages are not installed"
)
expect_error(
  pversion_check4mismatch("foo", lib.loc="foo"),
  pattern = "The following packages are not installed"
)
expect_error(
  pversion_report("foo", lib.loc="foo"),
  pattern = "The following packages are not installed"
)


# package misspelled ====

pattern <- "You have misspelled the following"
loops <- loops + 1
for(i in c("", "!@#$%^&*()")) {
  expect_error(
    import_as(~stri., i),
    pattern = pattern
  ) |> errorfun()
  expect_error(
    import_inops(i),
    pattern = pattern
  )  |> errorfun()
  expect_error(
    import_LL(i, "foo"),
    pattern = pattern
  )  |> errorfun()
  expect_error(
    pversion_check4mismatch(i),
    pattern = pattern
  )  |> errorfun()
  expect_error(
    pversion_report(i),
    pattern = pattern
  )  |> errorfun()
  enumerate <- enumerate + 5
}
expect_error(
  import_int(`!@#$%^&*()` ~ foo),
  pattern = pattern
)


# package is core R ====
basepkgs <- c(
  "base", "compiler", "datasets", "grDevices", "graphics", "grid", "methods",
  "parallel", "splines", "stats", "stats4", "tcltk", "tools",
  "translations", "utils"
)
pattern <- 'The following "packages" are base/core R, which is not allowed:'
loops <- loops + 1
for(i in basepkgs) {
  expect_error(
    import_as(~stri., i),
    pattern = pattern
  ) |> errorfun()
  expect_error(
    import_inops(i),
    pattern = pattern
  )  |> errorfun()
  expect_error(
    import_LL(i, "foo"),
    pattern = pattern
  )  |> errorfun()
  form <- as.formula(paste(i, "~ foo"))
  expect_error(
    import_int(form),
    pattern = pattern
  )  |> errorfun()
  enumerate <- enumerate + 4
}


# package is metaverse ====
metapkgs <- c(
  "tidyverse", "fastverse", "tinyverse"
)
pattern <- "The following packages are known meta-verse packages, which is not allowed:"
loops <- loops + 1
for(i in metapkgs) {
  expect_error(
    import_as(~stri., i),
    pattern = pattern
  ) |> errorfun()
  expect_error(
    import_inops(i),
    pattern = pattern
  )  |> errorfun()
  expect_error(
    import_LL(i, "foo"),
    pattern = pattern
  )  |> errorfun()
  form <- as.formula(paste(i, "~ foo"))
  expect_error(
    import_int(form),
    pattern = pattern
  )  |> errorfun()
  expect_error(
    pversion_check4mismatch(i),
    pattern = pattern
  )  |> errorfun()
  expect_error(
    pversion_report(i),
    pattern = pattern
  )  |> errorfun()
  enumerate <- enumerate + 6
}


# bad library ====
expect_error(
  import_as(~stri., "stringi", lib.loc=mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  import_inops("stringi", lib.loc=mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  import_LL("stringi", "%stri==%", lib.loc=mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  import_int(stringi ~ foo, lib.loc=mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  import_data("stringi", 'foo', lib.loc=mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  pversion_report("stringi", lib.loc = mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  pversion_check4mismatch("stringi", lib.loc = mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
