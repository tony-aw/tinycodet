
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
