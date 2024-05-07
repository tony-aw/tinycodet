
# test import_inops() - deleting inops ====
temp.fun <- function(){
  suppressWarnings(import_inops("stringi"))
  import_inops(unexpose = "stringi")
  ls()
}
expect_equal(
  temp.fun(),
  character(0)
)


# test import_inops - returning NULL value ====
expect_equal(
  import_inops(unexpose = "stringi"),
  NULL
)
expect_silent(
  import_inops(unexpose = "stringi")
)



# other error handling ===
expect_error(
  import_inops(expose = "stringi", unexpose = "stringi"),
  pattern = "Can only specify either `expose` or `unexpose`, not both",
  fixed = TRUE
)

expect_error(
  import_inops(unexpose = NA),
  pattern = "`unexpose` must be a package name (string) or an alias from `import_as()`",
  fixed = TRUE
)

expect_error(
  import_inops(unexpose = c("stringi", "stringi2")),
  pattern = "`unexpose` must be a package name (string) or an alias from `import_as()`",
  fixed = TRUE
)



