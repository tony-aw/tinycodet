
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


# test import_inops() - error checks:
temp.fun <- function(){
  suppressWarnings(import_inops("stringi"))
  import_inops("stringi", overwrite = FALSE, inherits = FALSE)
  ls()
}
expect_error(
  temp.fun(),
  pattern = "ALL prepared infix operators already exist in the current environment"
)

temp.fun <- function(){
  import_inops("stringi", include.only="%stri+%", exclude = "%stri*%")
  ls()
}
expect_error(
  temp.fun(),
  pattern = "canntot specify both `exclude` and `include.only`"
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

