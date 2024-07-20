
expect_equal(
  lapply(pversion_report("stringi"), class),
  list("package" = "character",
       "version_loaded" = "character",
       "version_lib.loc" = "character",
       "versions_equal" = "logical"
  )
)

check <- pversion_report("stringi")

expect_true(
  utils::compareVersion(check$version_loaded, getNamespaceVersion("stringi")) == 0
)
expect_true(
  utils::compareVersion(check$version_lib.loc, getNamespaceVersion("stringi")) == 0
)

expect_true(pversion_report("stringi")$versions_equal)

expect_null(pversion_check4mismatch("stringi"))
