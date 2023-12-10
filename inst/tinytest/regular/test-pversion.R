
expect_equal(
  lapply(pversion_report("stringi"), class),
  list("package" = "character",
       "version_loaded" = "character",
       "version_lib.loc" = "character",
       "versions_equal" = "logical"
  )
)

expect_true(pversion_report("stringi")$versions_equal)

expect_null(pversion_check4mismatch("stringi"))
