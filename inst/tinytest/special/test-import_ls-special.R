
# set-up ====
from.dir <- file.path(getwd(), "fakelibs")
to.dir <- tempdir() |> normalizePath()
# tinycodet:::.create_fake_packages(from.dir, to.dir)
lib.loc1 <- file.path(to.dir, "fake_lib1")
lib.loc2 <- file.path(to.dir, "fake_lib2")
lib.loc3 <- file.path(to.dir, "fake_lib3")
print(lib.loc1)
print(lib.loc2)
print(lib.loc3)

overwriters <- c("const_overwritten",  "rpbase_overwritten", "fun_overwritten", "rpbase_overwritten<-", "%opover%")
templates <- c("fun<d>", "%op<d>%", "rpbase<d><-", "rpbase<d>", "const<d>")


# return value, re-exports = TRUE ====
pkg <- "tinycodetfakepkg3"
ns <- loadNamespace(pkg, lib.loc = lib.loc1) |> as.list(all.names=TRUE, sorted=TRUE)
names_exported <- names(ns[[".__NAMESPACE__."]][["exports"]])

expected_infix <- c(
  stringi::stri_replace_all("%op<d>%", c("31", "32", "11"), fixed = "<d>"),
  "%opover%"
)
expected_rpop <- c(
  stringi::stri_replace_all("rpbase<d><-", c("31", "32", "11"), fixed = "<d>"),
  "rpbase_overwritten<-"
)
expected_rpbase <- c(
  stringi::stri_replace_all("rpbase<d>", c("31", "32", "11"), fixed = "<d>"),
  "rpbase_overwritten"
)
expected_const <- c(
  stringi::stri_replace_all("const<d>", c("31", "32", "11"), fixed = "<d>"),
  "const_overwritten"
)
expected_reg <- setdiff(
  names_exported, c(expected_infix, expected_rpop, expected_const)
)

out <- import_ls(pkg, type = "infix", lib.loc = lib.loc1) |> sort()
expect_equal(out, sort(expected_infix))

out <- import_ls(pkg, type = "rp", lib.loc = lib.loc1) |> sort()
expect_equal(out, sort(c(expected_rpbase, expected_rpop)))

out <- import_ls(pkg, type = "nonfun", lib.loc = lib.loc1) |> sort()
expect_equal(out, sort(expected_const))

out <- import_ls(pkg, type = "reg", lib.loc = lib.loc1) |> sort()
expect_equal(out, sort(expected_reg))

out <- import_ls(pkg, lib.loc = lib.loc1) |> sort()
expect_equal(out, sort(names_exported))


# return value, re-exports = FALSE ====
pkg <- "tinycodetfakepkg3"
ns <- loadNamespace(pkg, lib.loc = lib.loc1) |> as.list(all.names=TRUE, sorted=TRUE)
names_exported <- names(ns[[".__NAMESPACE__."]][["exports"]])
ns <- ns[names_exported]
ns <- ns[!is.na(names(ns))]
names_exported <- names(ns)

expected_infix <- c(
  stringi::stri_replace_all("%op<d>%", c("31", "32"), fixed = "<d>"),
  "%opover%"
)
expected_rpop <- c(
  stringi::stri_replace_all("rpbase<d><-", c("31", "32"), fixed = "<d>"),
  "rpbase_overwritten<-"
)
expected_rpbase <- c(
  stringi::stri_replace_all("rpbase<d>", c("31", "32"), fixed = "<d>"),
  "rpbase_overwritten"
)
expected_const <- c(
  stringi::stri_replace_all("const<d>", c("31", "32"), fixed = "<d>"),
  "const_overwritten"
)
expected_reg <- setdiff(
  names_exported, c(expected_infix, expected_rpop, expected_const)
)

out <- import_ls(pkg, type = "infix", re_exports = FALSE, lib.loc = lib.loc1) |> sort()
expect_equal(out, sort(expected_infix))

out <- import_ls(pkg, type = "rp", re_exports = FALSE, lib.loc = lib.loc1) |> sort()
expect_equal(out, sort(c(expected_rpbase, expected_rpop)))

out <- import_ls(pkg, type = "nonfun", re_exports = FALSE, lib.loc = lib.loc1) |> sort()
expect_equal(out, sort(expected_const))

out <- import_ls(pkg, type = "reg", re_exports = FALSE, lib.loc = lib.loc1) |> sort()
expect_equal(out, sort(expected_reg))

out <- import_ls(pkg, re_exports = FALSE, lib.loc = lib.loc1) |> sort()
expect_equal(out, sort(names_exported))




# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false

