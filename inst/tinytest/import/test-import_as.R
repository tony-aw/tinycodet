
# test import_as - single package ====
stri <- loadNamespace("stringi") |> getNamespaceExports()
temp.fun <- function() {
  import_as(.stri ~ stringi)
  out <- setdiff(names(.stri), ".__attributes__.") |> sort()
  return(out)
}
expect_equal(temp.fun(), sort(stri))


# test import_as - functional functions ====
temp.fun <- function() {
  import_as(.stri ~ stringi)
  .stri$stri_c("a", "b")
}
expect_equal(temp.fun(), "ab")


# main_package error handling ====
expect_error(
  import_as(.stri ~ stringi + tinycodet),
  pattern = "improper formula given for `main`"
)


expect_error(
  import_as(.stri ~ base),
  pattern = 'The following "packages" are base/core R, which is not allowed:'
)


# alias error handling ====
expect_error(
  import_as(`!@#$%^&*()` ~ `stringi`),
  pattern = "Syntactically invalid name for alias"
)
expect_error(
  import_as( .__foo__. ~ stringi),
  pattern = "Syntactically invalid name for alias"
)


# dependencies/extensions basic error handling ====
expect_error(
  import_as(.stri ~ stringi, deps = ~foo),
  pattern = "`deps` must be a character vector",
  fixed = TRUE
)
expect_error(
  import_as(.stri ~ stringi, deps = character(0)),
  pattern = "`deps` must be a character vector",
  fixed = TRUE
)

expect_error(
  import_as(.stri ~ stringi, deps = letters),
  pattern = "no more than 5 packages allowed to be imported under a single alias",
  fixed = TRUE
)

expect_error(
  import_as(.stri ~ stringi, deps = "Rcpp"),
  pattern = "The following given minimal dependencies were not found to be actual minimal dependencies"
)

expect_error(
  import_as(.stri ~ stringi, deps = c("Rcpp", "Rcpp")),
  pattern = "The following duplicate minimal dependencies given:"
)

# other error handling ====
expect_error(
  import_as(.stri ~ stringi, re_exports = NA),
  pattern = "`re_exports` must be `TRUE` or `FALSE`"
)

expect_error(
  import_as(.stri ~ stringi, re_exports = c(TRUE, FALSE)),
  pattern = "`re_exports` must be `TRUE` or `FALSE`"
)


temp.fun <- function(){
  import_as(.stri ~ stringi)
  stri_c("a", "b")
}
expect_error(temp.fun(), "could not find function")

