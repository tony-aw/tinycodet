
# import_as() lock checks:
temp.fun <- function(){
  import_as(~stri., "stringi")
  stri.$stri_c <- "hello"
}
expect_error(temp.fun(), pattern = "cannot change value of locked binding")
temp.fun <- function(){
  stri. %<-c% "hello"
  import_as(~stri., "stringi")
}
expect_error(temp.fun(), pattern = "cannot change value of locked binding")
temp.fun <- function(){
  import_as(~stri., "stringi")
  stri.$new_function <- function(x,y) paste0(x,y)
}
expect_error(temp.fun(), pattern = "cannot add bindings to a locked environment")
temp.fun <- function(){
  import_as(~stri., "stringi")
  stri.$stri_c <- NULL
}
expect_error(temp.fun(), pattern = "cannot change value of locked binding")
temp.fun <- function(){
  import_as(~stri., "stringi")
  stri.$.__attributes__. <- list()
}
expect_error(temp.fun(), pattern = "cannot change value of locked binding")
temp.fun <- function(){ # this should work
  import_as(~stri., "stringi")
  import_as(~stri., "stringi")
}
expect_silent(temp.fun())
temp.fun <- function(){ # this should also work
  stri. <- loadNamespace("stringi")
  import_as(~stri., "stringi")
}
expect_silent(temp.fun())


# test import_inops - lock checks:
temp.fun <- function() {
  `%stri+%` <- function(x, y) paste0(x, y)
  lockBinding("%stri+%", env = environment())
  import_inops("stringi")
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
temp.fun <- function() {
  import_LL("stringi", "%stri+%") |> suppressMessages() |> suppressWarnings()
  import_inops("stringi", overwrite = TRUE) |> suppressMessages() |> suppressWarnings()
}
expect_silent(temp.fun())

