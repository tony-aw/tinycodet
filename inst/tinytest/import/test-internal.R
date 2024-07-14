
# lists ====

expect_equal(
  sort(tinycodet:::.internal_list_coreR()),
  sort(c(
    "base", "compiler", "datasets", "grDevices", "graphics", "grid", "methods",
    "parallel", "splines", "stats", "stats4", "tcltk", "tools",
    "translations", "utils"
  ))
)

expect_equal(
  sort(tinycodet:::.internal_list_preinst()),
  sort(c(
    "boot", "class", "cluster", "codetools", "foreign", "KernSmooth",
    "lattice", "MASS", "Matrix",  "mgcv", "nlme", "nnet",
    "rpart", "spatial", "survival"
  ))
)

expect_equal(
  sort(tinycodet:::.internal_list_knownmeta()),
  sort(c("tidyverse", "fastverse", "tinyverse"))
)

expect_equal(
  sort(tinycodet:::.internal_list_tidyshared()),
  sort(c("rlang", "lifecycle", "cli", "glue", "withr"))
)

expect_error(
  tinycodet:::.internal_check_forbidden_pkgs(c("tidyverse", "fastverse", "tinyverse"), lib.loc1, abortcall = sys.call()),
  pattern = paste0(
    "The following packages are known meta-verse packages, which is not allowed:",
    "\n",
    "tidyverse, fastverse, tinyverse"
  ),
  fixed = TRUE
)


# check rcpp internal function does NOT modify by reference ====
package <- "stringi"
ns <- as.list(loadNamespace(package, lib.loc = lib.loc), 
              all.names = TRUE, sorted = TRUE)
names_exported <- names(ns[[".__NAMESPACE__."]][["exports"]])
ns <- ns[names_exported]
ns <- ns[!is.na(names(ns))]
names_exported <- names(ns)
names_functions <- names(ns)[unlist(vapply(ns, is.function, 
                                           FUN.VALUE = logical(1)), use.names = FALSE)]
ns2 <- ns
ns <- tinycodet:::.rcpp_prep_ns(ns, names_functions, package)
out <- mapply(identical, ns, ns2)
expect_false(
  any(out)
)

