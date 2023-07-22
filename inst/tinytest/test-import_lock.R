
# import_as() lock checks:
temp.fun <- function(){
  import_as(stri., "stringi")
  stri. <- "hello"
}
expect_error(temp.fun(), pattern = "cannot change value of locked binding")
temp.fun <- function(){
  import_as(stri., "stringi")
  stri.$stri_c <- "hello"
}
expect_error(temp.fun(), pattern = "cannot change value of locked binding")
temp.fun <- function(){
  import_as(stri., "stringi")
  attributes(stri.) <- list()
}
expect_error(temp.fun(), pattern = "cannot change value of locked binding")
temp.fun <- function(){ # this should work
  import_as(stri., "stringi")
  import_as(stri., "stringi")
}
expect_silent(temp.fun())



# test import_inops - lock checks:
temp.fun <- function() {
  import_inops("stringi")
  `%stri+%` <- "hello"
}
expect_error(temp.fun(), pattern = "cannot change value of locked binding")
temp.fun <- function() {
  import_inops("stringi")
  attributes(`%stri+%`) <- list()
}
expect_error(temp.fun(), pattern = "cannot change value of locked binding")
temp.fun1 <- function() { # this should work
  import_inops("stringi", overwrite = TRUE)
  import_inops("stringi", overwrite = TRUE)
  ls()
}
temp.fun2 <- function() { # this should work
  import_inops("stringi", overwrite = TRUE)
  ls()
}
expect_equal(temp.fun1(), temp.fun2())


