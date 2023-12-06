
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
  tinycodet:::.internal_check_pkgs(c("tidyverse", "fastverse", "tinyverse"), lib.loc1, abortcall = sys.call()),
  pattern = paste0(
    "The following packages are known meta-verse packages, which is not allowed:",
    "\n",
    "tidyverse, fastverse, tinyverse"
  ),
  fixed = TRUE
)

