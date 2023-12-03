
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

expect_equal(
  tinycodet:::.internal_list_coreR(),
  c(
    "base", "compiler", "datasets", "grDevices", "graphics", "grid", "methods",
    "parallel", "splines", "stats", "stats4", "tcltk", "tools",
    "translations", "utils"
  )
)

expect_equal(
  tinycodet:::.internal_list_preinst(),
  c(
    "boot", "class", "cluster", "codetools", "foreign", "KernSmooth",
    "lattice", "MASS", "Matrix",  "mgcv", "nlme", "nnet",
    "rpart", "spatial", "survival"
  )
)

expect_equal(
  tinycodet:::.internal_list_knownmeta(),
  c("tidyverse", "fastverse", "tinyverse")
)

expect_equal(
  tinycodet:::.internal_list_commonshared(),
  c("rlang", "cli", "lifecycle")
)
