
# test function selection ====
temp.fun <- function(){
  import_from("stringi", "stri_detect") |> suppressMessages()
  ls()
}
expect_equal(temp.fun(), "stri_detect")


# test core function works ====
temp.fun1 <- function(...){
  import_from("stringi", "stri_detect") |> suppressMessages()
  stri_detect(...)
}
temp.fun2 <- function(...){
  stri_detect <- loadNamespace("stringi")$stri_detect
  stri_detect(...)
}
expect_equal(temp.fun1("hello", regex = "a|ei|o|u"), temp.fun2("hello", regex = "a|ei|o|u"))


# operators work ====
import_from("stringi", c("stri_sub", "stri_sub<-"), prefix = "alias_")
s1 <- c("spam, spam, bacon, and spam", "eggs and spam")
s2 <- c("spam, spam, bacon, and spam", "eggs and spam")
stringi::stri_sub(s1, 1, 4) <- 'stringi'
alias_stri_sub(s2, 1, 4) <- 'stringi'
expect_equal(
  s1, s2
)

import_from("stringi", "%s+%", prefix = "alias_") # alias should be ignored
expect_equal(
  c('abc', '123', 'xy') %s+% letters[1:6],
  stringi::`%s+%`(c('abc', '123', 'xy'), letters[1:6])
)

rm(list = c("alias_stri_sub", "alias_stri_sub<-", "%s+%"))



# test locked ====
temp.fun <- function() {
  import_from("stringi", "stri_detect", lock = TRUE) |> suppressMessages()
  bindingIsLocked("stri_detect", environment())
}
expect_true(temp.fun())
temp.fun <- function() {
  import_from("stringi", "stri_detect", lock = FALSE) |> suppressMessages()
  bindingIsLocked("stri_detect", environment())
}
expect_false(temp.fun())

temp.fun <- function() {
  import_from("stringi", "stri_detect", lock = TRUE) |> suppressMessages()
  stri_detect <- "foo"
}
expect_error(
  temp.fun(),
  pattern = "cannot change value of locked binding for 'stri_detect'"
)


# test function is removable ====
temp.fun <- function() {
  import_from("stringi", "stri_detect") |> suppressMessages()
  rm(list = "stri_detect")
  ls()
}
expect_equal(temp.fun(), character(0))


# test prefix ====
temp.fun <- function() {
  import_from("stringi", import_ls("stringi", "rp"), prefix = "alias_")
  ls()
}
expect_equal(
  temp.fun() |> sort(),
  stringi::stri_c("alias_", import_ls("stringi", "rp")) |> sort()
)

temp.fun <- function() {
  import_from("stringi", import_ls("stringi", "infix"), prefix = "alias_")
  ls()
}
expect_equal(
  temp.fun() |> sort(),
  import_ls("stringi", "infix") |> sort()
)


# nothing to import ====
expect_message(
  import_from("stringi", character(0L)),
  pattern = "nothing to import"
)

# selection error handling ====
expect_error(
  import_from("stringi", NA),
  pattern = "`ls` must be a character vector of object names"
)
pattern <-  "`ls` cannot have missing values, empty strings, or duplicate values"
expect_error(
  import_from("stringi", ""),
  pattern = pattern
)
expect_error(
  import_from("stringi", c("stri_detect", "stri_detect")),
  pattern = pattern
)
expect_error(
  import_from("stringi", "foo"),
  pattern = "The following object names do not exist in the given package"
)

# prexix error handling ====
expect_error(
  import_from("stringi", "stri_c", prefix = ~ foo),
  pattern = "`prefix` must be a single string"
)
expect_error(
  import_from("stringi", "stri_c", prefix = letters),
  pattern = "`prefix` must be a single string"
)
expect_error(
  import_from("stringi", "stri_c", prefix = "pre."),
  pattern = "`prefix` cannot end with a single dot"
)



# package error handling ====
expect_error(
  import_from(c("stringi", "gamair"), "foo"),
  pattern = "`package` must be a single string"
)

