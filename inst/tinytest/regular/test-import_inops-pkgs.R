
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


# package error handling ====
expect_error(
  import_inops("stringi", lib.loc = "foo"),
  pattern = "The following packages are not installed"
)

expect_error(
  import_inops("base"),
  pattern = 'The following "packages" are base/core R, which is not allowed:'
)

expect_error(
  import_inops("!@#$%^&*()"),
  pattern = "You have misspelled the following"
)

expect_error(
  import_inops(""),
  pattern = "You have misspelled the following"
)


# other error handling ===
expect_error(
  import_inops("stringi", lib.loc = mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)

expect_error(
  import_inops(expose = "stringi", unexpose = "stringi"),
  pattern = "Can only specify either `expose` or `unexpose`, not both",
  fixed = TRUE
)

expect_error(
  import_inops(expose = NA),
  pattern = "`expose` must be a package name (string) or an alias from `import_as()`",
  fixed = TRUE
)

expect_error(
  import_inops(unexpose = NA),
  pattern = "`unexpose` must be a package name (string) or an alias from `import_as()`",
  fixed = TRUE
)

expect_error(
  import_inops(expose = c("stringi", "stringi2")),
  pattern = "`expose` must be a package name (string) or an alias from `import_as()`",
  fixed = TRUE
)

expect_error(
  import_inops(unexpose = c("stringi", "stringi2")),
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

