
# test import_inops - importing:
stri <- grep(":=|%", loadNamespace("stringi") |> getNamespaceExports(), value = TRUE)
temp.fun <- function(){
  import_inops("stringi")
  ls()
}
expect_equal(temp.fun()|>sort(), sort(stri))

stri <- grep(":=|%", loadNamespace("stringi") |> getNamespaceExports(), value = TRUE)
temp.fun <- function(){
  import_inops("stringi", include.only = "%stri+%")
  ls()
}
expect_equal(temp.fun()|>sort(), "%stri+%")

stri <- grep(":=|%", loadNamespace("stringi") |> getNamespaceExports(), value = TRUE)
stri <- setdiff(stri, "%stri+%")
temp.fun <- function(){
  import_inops("stringi", exclude = "%stri+%")
  ls()
}
expect_equal(temp.fun()|>sort(), sort(stri))


# test import_inops - deleting:
temp.fun <- function(){
  import_inops("stringi")
  import_inops("stringi", action = "remove")
  ls()
}
expect_equal(temp.fun(), character(0))



# test import_inops() - functional functions:
temp.fun <- function(){
  import_inops("stringi")
  return("a" %stri+% "b")
}
expect_equal(temp.fun(), "ab")


# test import_inops() - deleting inops:
temp.fun <- function(){
  suppressWarnings(import_inops("stringi"))
  import_inops("stringi", action =  "remove")
  ls()
}


# test import_inops() - basic error checks:
temp.fun <- function(){
  import_inops("")
}
expect_error(temp.fun(),
             pattern = "You have misspelled the following")

temp.fun <- function() {
  import_inops("stringi", action = "boo")
}
expect_error(temp.fun(),
             pattern = "`action` must be one of the following: `expose`, or `delete`")

temp.fun <- function() {
  import_inops("stringi", lib.loc = mean)
 }
expect_error(
  temp.fun(),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)

temp.fun <- function() {
  import_inops("stringi", lib.loc = "foo")
}
expect_error(
  temp.fun(),
  pattern = "The following packages are not installed"
)


# test import_inops.control() error checks:
temp.fun <- function(){
  import_inops("stringi", overwrite = NA, inherits = FALSE)
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`overwrite` must be either `TRUE` or `FALSE`"
)

temp.fun <- function(){
  import_inops("stringi", overwrite = c(TRUE, FALSE), inherits = FALSE)
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`overwrite` must be either `TRUE` or `FALSE`"
)

temp.fun <- function(){
  import_inops("stringi", overwrite = TRUE, inherits = NA)
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`inherits` must be either `TRUE` or `FALSE`"
)

temp.fun <- function(){
  import_inops("stringi", overwrite = TRUE, inherits = c(TRUE, FALSE))
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`inherits` must be either `TRUE` or `FALSE`"
)

temp.fun <- function(){
  import_inops("stringi", include.only="%stri+%", exclude = "%stri*%")
  ls()
}
expect_error(
  temp.fun(),
  pattern = "canntot specify both `exclude` and `include.only`"
)

temp.fun <- function(){
  import_inops("stringi", exclude= "")
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`exclude` must be a character vector with function names"
)

temp.fun <- function(){
  import_inops("stringi", include.only = "")
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`include.only` must be a character vector with function names"
)

temp.fun <- function(){
  suppressWarnings(import_inops("stringi"))
  import_inops("stringi", overwrite = FALSE, inherits = FALSE)
  ls()
}
expect_error(
  temp.fun(),
  pattern = "ALL prepared infix operators already exist in the current environment"
)



# test import_inops - warning checks:
temp.fun <- function(){
  suppressWarnings(import_inops("stringi"))
  import_inops("stringi", overwrite = TRUE, inherits = FALSE)
  ls()
}
expect_warning(
  temp.fun(),
  pattern = "ALL prepared infix operators already exist in the current environment"
)

