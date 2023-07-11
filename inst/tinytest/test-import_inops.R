
# test import_inops:
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

temp.fun <- function(){
  suppressWarnings(import_inops("stringi"))
  import_inops("stringi", overwrite = FALSE, inherits = FALSE)
  ls()
}
expect_error(temp.fun())

temp.fun <- function(){
  suppressWarnings(import_inops("stringi"))
  import_inops("stringi", overwrite = TRUE, inherits = FALSE)
  ls()
}
expect_warning(temp.fun())

