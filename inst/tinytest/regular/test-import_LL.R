
# test function selection ====
temp.fun <- function(){
  import_LL("stringi", "stri_detect") |> suppressMessages()
  ls()
}
expect_equal(temp.fun(), "stri_detect")


# test function works ====
temp.fun1 <- function(...){
  import_LL("stringi", "stri_detect") |> suppressMessages()
  stri_detect(...)
}
temp.fun2 <- function(...){
  stri_detect <- loadNamespace("stringi")$stri_detect
  stri_detect(...)
}
expect_equal(temp.fun1("hello", regex = "a|ei|o|u"), temp.fun2("hello", regex = "a|ei|o|u"))


# test function is locked ====
temp.fun <- function() {
  import_LL("stringi", "stri_detect") |> suppressMessages()
  bindingIsLocked("stri_detect", environment())
}
expect_true(temp.fun())

temp.fun <- function() {
  import_LL("stringi", "stri_detect") |> suppressMessages()
  stri_detect <- "foo"
}
expect_error(
  temp.fun(),
  pattern = "cannot change value of locked binding for 'stri_detect'"
)


# test function is removable ====
temp.fun <- function() {
  import_LL("stringi", "stri_detect") |> suppressMessages()
  rm(list = "stri_detect")
  ls()
}
expect_equal(temp.fun(), character(0))

temp.fun <- function() {
  import_LL("stringi", "%stri+%") |> suppressMessages()
  import_inops(unexpose = "stringi")
  ls()
}
expect_equal(temp.fun(), character(0))


# test tinyimport can overwrite ===
temp.fun <- function() {
  import_LL("stringi", "%stri+%") |> suppressMessages()
  import_inops(expose = "stringi", overwrite = TRUE) |> suppressMessages() |> suppressWarnings()
  ls()
}
expect_silent(temp.fun())


# check messaging ===
expect_message(
  import_LL("stringi", "stri_detect"),
  pattern = "exposing and locking functions to current environment ..."
)


# selection error handling ====
expect_error(
  import_LL("stringi", ""),
  pattern = "`selection` must be a non-empty character vector of unique function names"
)
expect_error(
  import_LL("stringi", character(0)),
  pattern = "`selection` must be a non-empty character vector of unique function names"
)
expect_error(
  import_LL("stringi", NA),
  pattern = "`selection` must be a non-empty character vector of unique function names"
)
expect_error(
  import_LL("stringi", c("stri_detect", "stri_detect")),
  pattern = "`selection` must be a non-empty character vector of unique function names"
)
expect_error(
  import_LL("stringi", "foo"),
  pattern = "specified functions not found in package namespace"
)


# package error handling ====
expect_error(
  import_LL(c("stringi", "gamair"), "foo"),
  pattern = "`package` must be a single string"
)

expect_error(
  import_LL("stringi", "stri_detect", lib.loc = "foo"),
  pattern = "The following packages are not installed"
)

expect_error(
  import_LL("stringi", "stri_detect", lib.loc = c("foo", "foo")),
  pattern = "The following packages are not installed"
)

expect_error(
  import_LL("base", "loadNamespace"),
  pattern = 'The following "packages" are base/core R, which is not allowed:'
)

expect_error(
  import_LL("!@#$%^&*()", "foo"),
  pattern = "You have misspelled the following"
)

expect_error(
  import_LL("", "foo"),
  pattern = "You have misspelled the following"
)

expect_error(
  import_LL("stringi", lib.loc = mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
