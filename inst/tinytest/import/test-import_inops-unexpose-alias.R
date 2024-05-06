
# test import_inops - deleting ====
temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri.)
  import_inops(unexpose = stri.)
  ls() |> sort()
}
expect_equal(temp.fun(), "stri.")


# test import_inops - returning NULL value ====
import_as(~ stri., "stringi")
expect_equal(
  import_inops(unexpose = stri.),
  NULL
)
expect_silent(
  import_inops(unexpose = stri.)
)


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
  import_inops(unexpose = NA)
}
expect_error(
  temp.fun(),
  pattern = "`unexpose` must be a package name (string) or an alias from `import_as()`",
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


