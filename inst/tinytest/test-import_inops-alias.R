
# test import_inops - importing ====
stri <- grep(":=|%", loadNamespace("stringi") |> getNamespaceExports(), value = TRUE)
temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose=stri.)
  ls() |> sort()
}
expect_equal(temp.fun()|>sort(), sort(c(stri, "stri.")))

stri <- grep(":=|%", loadNamespace("stringi") |> getNamespaceExports(), value = TRUE)
temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops( expose=stri., include.only = "%stri+%")
  ls() |> sort()
}
expect_equal(temp.fun()|>sort(), sort(c("%stri+%", "stri.")))

stri <- grep(":=|%", loadNamespace("stringi") |> getNamespaceExports(), value = TRUE)
stri <- setdiff(stri, "%stri+%")
temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops( expose=stri., exclude = "%stri+%")
  ls() |> sort()
}
expect_equal(temp.fun()|>sort(), sort(c(stri, "stri.")))


# test import_inops - deleting ====
temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri.)
  import_inops(unexpose = stri.)
  ls() |> sort()
}
expect_equal(temp.fun(), "stri.")


# test import_inops() - functional functions ====
temp.fun <- function(){
  import_as(~stri., "stringi")
  import_inops(expose = stri.)
  return("a" %stri+% "b")
}
expect_equal(temp.fun(), "ab")


# test import_inops - error checks ====
temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri., unexpose = stri.)
}
expect_error(
  temp.fun(),
  pattern = "Can only specify either `expose` or `unexpose`, not both",
  fixed = TRUE
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = NA)
}
expect_error(
  temp.fun(),
  pattern = "`expose` must be a package name (string) or an alias from `import_as()`",
  fixed = TRUE
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(unexpose = NA)
}
expect_error(
  temp.fun(),
  pattern = "`unexpose` must be a package name (string) or an alias from `import_as()`",
  fixed = TRUE
)

temp.fun <- function() {
  import_inops(expose = environment())
}
expect_error(
  temp.fun(),
  pattern = "The given environment is not an alias from `import_as()`",
  fixed = TRUE
)

temp.fun <- function() {
  import_inops(unexpose = environment())
}
expect_error(
  temp.fun(),
  pattern = "The given environment is not an alias from `import_as()`",
  fixed = TRUE
)


# test import_inops - warning checks ====
temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri.)  |> suppressWarnings()
  import_inops(expose = stri., overwrite = TRUE, inherits=FALSE)
}
expect_warning(
  temp.fun(),
  pattern = "ALL prepared infix operators already exist in the current environment"
)
