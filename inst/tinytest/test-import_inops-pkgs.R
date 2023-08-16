# NOTE: most of the tests I do in a separate script outside of the package folder,
# as R CMD CHECK gets angry when I put fake packages inside the package folder.


# test import_inops - importing ====
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


# test import_inops - deleting ====
temp.fun <- function(){
  import_inops("stringi")
  import_inops(unexpose="stringi")
  ls()
}
expect_equal(temp.fun(), character(0))



# test import_inops() - functional functions ====
temp.fun <- function(){
  import_inops("stringi")
  return("a" %stri+% "b")
}
expect_equal(temp.fun(), "ab")


# test import_inops() - deleting inops ====
temp.fun <- function(){
  suppressWarnings(import_inops("stringi"))
  import_inops(unexpose = "stringi")
  ls()
}


# test import_inops() - error checks ====
temp.fun <- function(){
  import_inops("")
}
expect_error(temp.fun(),
             pattern = "You have misspelled the following")

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


temp.fun <- function() {
  import_inops(expose = "stringi", unexpose = "stringi")
}
expect_error(
  temp.fun(),
  pattern = "Can only specify either `expose` or `unexpose`, not both",
  fixed = TRUE
)

temp.fun <- function() {
  import_inops(expose = NA)
}
expect_error(
  temp.fun(),
  pattern = "`expose` must be a package name (string) or an alias from `import_as()`",
  fixed = TRUE
)

temp.fun <- function() {
  import_inops(unexpose = NA)
}
expect_error(
  temp.fun(),
  pattern = "`unexpose` must be a package name (string) or an alias from `import_as()`",
  fixed = TRUE
)

temp.fun <- function() {
  import_inops(expose = c("stringi", "stringi2"))
}
expect_error(
  temp.fun(),
  pattern = "`expose` must be a package name (string) or an alias from `import_as()`",
  fixed = TRUE
)

temp.fun <- function() {
  import_inops(unexpose = c("stringi", "stringi2"))
}
expect_error(
  temp.fun(),
  pattern = "`unexpose` must be a package name (string) or an alias from `import_as()`",
  fixed = TRUE
)

# test import_inops - warning checks ====
temp.fun <- function(){
  suppressWarnings(import_inops("stringi"))
  import_inops("stringi", overwrite = TRUE, inherits = FALSE)
  ls()
}
expect_warning(
  temp.fun(),
  pattern = "ALL prepared infix operators already exist in the current environment"
)

