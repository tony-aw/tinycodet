
# installed in ====
expect_equal(
  c("foo", "stringi", "stats") %installed in% .libPaths(),
  setNames(c(FALSE, TRUE, NA), c("foo", "stringi", "stats"))
)
expect_equal(
  "stringi" %installed in% .libPaths(),
  c("stringi" = TRUE)
)
expect_equal(
  "foo" %installed in% .libPaths(),
  c("foo" = FALSE)
)
expect_equal(
  "stats" %installed in% .libPaths(),
  c("stats" = NA)
)



# pkg_get_deps and related internal functions ====
expect_error(
  pkg_get_deps("stringi", base = "foo"),
  pattern = "arguments `base`, `recom`, `semi`, `shared_tidy` must each be either `TRUE` OR `FALSE`"
)

expect_error(
  pkg_get_deps("stringi", recom = "foo"),
  pattern = "arguments `base`, `recom`, `semi`, `shared_tidy` must each be either `TRUE` OR `FALSE`"
)

expect_error(
  pkg_get_deps("stringi", semi = "foo"),
  pattern = "arguments `base`, `recom`, `semi`, `shared_tidy` must each be either `TRUE` OR `FALSE`"
)

expect_error(
  pkg_get_deps("stringi", shared_tidy = "foo"),
  pattern = "arguments `base`, `recom`, `semi`, `shared_tidy` must each be either `TRUE` OR `FALSE`"
)

