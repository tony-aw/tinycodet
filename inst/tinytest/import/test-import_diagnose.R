
ns <- loadedNamespaces()
ns <- ns[!ns %in% tinycodet:::.list_coreR()]

expected <- data.frame(
  package = ns,
  installed_in_lib.loc = FALSE,
  version_loaded = lapply(ns, \(x)as.character(packageVersion(x))) |> unlist(),
  version_installed = NA_character_,
  versions_equal = NA
)
expected <- expected[order(expected$package),]

out <- import_diagnose("foo")
out <- out[order(out$package),]

expect_equal(
  expected, out 
)

