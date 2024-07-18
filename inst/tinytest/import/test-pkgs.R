
# installed in ===
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


# pkg_lsf ====
ns <- loadNamespace("stringi") |> as.list(all.names=TRUE, sorted=TRUE)
names_exported <- names(ns[[".__NAMESPACE__."]][["exports"]])
ns <- ns[names_exported]
ns <- ns[!is.na(names(ns))]

out <- pkg_lsf("stringi", type = "inops")
expected <- grep("%|:=", names(ns), value = TRUE)
expect_equal(out, expected)

out <- pkg_lsf("stringi", type = "regfuns")
expected <- grep("%|:=", names(ns), value = TRUE, invert = TRUE)
expect_equal(out, expected)

out <- pkg_lsf("stringi", type = "all")
expected <- names(ns)
expect_equal(out, expected)


# pkg_get_deps and related internal functions ====
expect_error(
  pkg_get_deps("stringi", base = "foo"),
  pattern = "arguments `base`, `recom`, `rstudioapi`, `shared_tidy` must each be either `TRUE` OR `FALSE`"
)

expect_error(
  pkg_get_deps("stringi", recom = "foo"),
  pattern = "arguments `base`, `recom`, `rstudioapi`, `shared_tidy` must each be either `TRUE` OR `FALSE`"
)

expect_error(
  pkg_get_deps("stringi", rstudioapi = "foo"),
  pattern = "arguments `base`, `recom`, `rstudioapi`, `shared_tidy` must each be either `TRUE` OR `FALSE`"
)

expect_error(
  pkg_get_deps("stringi", shared_tidy = "foo"),
  pattern = "arguments `base`, `recom`, `rstudioapi`, `shared_tidy` must each be either `TRUE` OR `FALSE`"
)

