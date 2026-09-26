
# set-up ====
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# package not installed ====
pattern <- "The following packages are not installed"
expect_error(
  import_as(.stri ~ stringi, lib.loc = c("foo1", "foo2")),
  pattern = pattern
)
expect_error(
  import_ls("stringi", lib.loc = c("foo1", "foo2")),
  pattern = pattern
)
expect_error(
  import_from("stringi", "stri_c", lib.loc = c("foo1", "foo2")),
  pattern = pattern
)
expect_error(
  import_as(.stri ~ stringi, deps = c("foo1", "foo2")),
  pattern = "The following minimal dependencies are not installed"
)


# package misspelled ====

pattern <- "You have misspelled the following"
loops <- loops + 1
for(i in c("_", "!@#$%^&*()")) {
  expect_error(
    import_as(paste0(".stri ~", "`", i, "`")),
    pattern = pattern
  ) |> errorfun()
  expect_error(
    import_ls(i),
    pattern = pattern
  ) |> errorfun()
  expect_error(
    import_from(i, i),
    pattern = pattern
  ) |> errorfun()
  expect_error(
    import_as(.stri ~ stringi, deps = i),
    pattern = pattern
  )
  enumerate <- enumerate + 4L
}

expect_error(
  import_as(.stri ~ stringi, deps = c("", "!@#$%^&*()")),
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
    import_as(paste0(".stri ~", "`", i, "`")),
    pattern = pattern
  ) |> errorfun()
  expect_error(
    import_ls(i),
    pattern = pattern
  ) |> errorfun()
  expect_error(
    import_from(i, i),
    pattern = pattern
  ) |> errorfun()
  expect_error(
    import_as(.stri ~ stringi, deps = i),
    pattern = pattern
  )
  enumerate <- enumerate + 4L
}

expect_error(
  import_as(.stri ~ stringi, deps = basepkgs[1:4]),
  pattern = pattern
)



# package is metaverse ====
metapkgs <- c(
  "tidyverse", "fastverse", "tinyverse"
)
pattern <- "The following packages are known meta-verse packages, which is not allowed:"
loops <- loops + 1
for(i in metapkgs) {
  expect_error(
    import_as(paste0(".stri ~", "`", i, "`")),
    pattern = pattern
  ) |> errorfun()
  expect_error(
    import_ls(i),
    pattern = pattern
  ) |> errorfun()
  expect_error(
    import_from(i, i),
    pattern = pattern
  ) |> errorfun()
  expect_error(
    import_as(.stri ~ stringi, deps = i),
    pattern = pattern
  )
  enumerate <- enumerate + 4L
}

expect_error(
  import_as(.stri ~ stringi, deps = metapkgs),
  pattern = pattern
)


# NO ERROR TRIGGER ====
# these tests check that the "pkg env" checks are NOT triggered usually

import_as2 <- function(...) {
  import_as(...)
}
import_ls2 <- function(...) {
  import_ls(...)
}
import_data2 <- function(...) {
  import_data(...)
}
import_from2 <- function(...) {
  import_from(...)
}
import_diagnose2 <- function(...) {
  import_diagnose(...)
}

expect_silent(
  import_as2(stri2. ~ stringi)
)
expect_silent(
  import_ls2("stringi")
)
expect_silent(
  import_data2("datasets", "cars")
)
expect_silent(
  import_from2("stringi", "stri_sub")
)
expect_silent(
  import_diagnose2()
)
