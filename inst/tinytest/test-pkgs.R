# NOTE: most of the tests I do in a separate script outside of the package folder,
# as R CMD CHECK gets angry when I put fake packages inside the package folder.

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
