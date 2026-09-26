
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

tempfun <- function(
    type = c("reg", "infix", "rp", "nonfun"),
    re_exports = TRUE,
    prefix = NULL
) {
  exports <- import_ls(
    "tinycodetfakepkg3", type,
    re_exports = re_exports, lib.loc = lib.loc1
  )
  import_from(
    "tinycodetfakepkg3", exports,
    re_exports = re_exports, prefix = prefix, lib.loc = lib.loc1
  )
  return(setdiff(ls(), c("type", "re_exports", "exports", "prefix")))
}


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

out <- tempfun("infix") |> sort()
expect_equal(out, sort(expected_infix))
out <- tempfun("infix", prefix = "alias_") |> sort()
expect_equal(out, sort(expected_infix))

out <- tempfun(type = "rp") |> sort()
expect_equal(out, sort(c(expected_rpbase, expected_rpop)))
out <- tempfun(type = "rp", prefix = "alias_") |> sort()
expect_equal(out, sort(paste0("alias_", c(expected_rpbase, expected_rpop))))

out <- tempfun(type = "nonfun") |> sort()
expect_equal(out, sort(expected_const))
out <- tempfun(type = "nonfun", prefix = "alias_") |> sort()
expect_equal(out, sort(expected_const))

out <- tempfun(type = "reg") |> sort()
expect_equal(out, sort(expected_reg))
out <- tempfun(type = "reg", prefix = "alias_") |> sort()
expect_equal(out, sort(paste0("alias_", expected_reg)))

out <- tempfun() |> sort()
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

out <- tempfun("infix", re_exports = FALSE) |> sort()
expect_equal(out, sort(expected_infix))
out <- tempfun("infix", re_exports = FALSE, prefix = "alias_") |> sort()
expect_equal(out, sort(expected_infix))

out <- tempfun(type = "rp", re_exports = FALSE) |> sort()
expect_equal(out, sort(c(expected_rpbase, expected_rpop)))
out <- tempfun(type = "rp", re_exports = FALSE, prefix = "alias_") |> sort()
expect_equal(out, sort(paste0("alias_", c(expected_rpbase, expected_rpop))))

out <- tempfun(type = "nonfun", re_exports = FALSE) |> sort()
expect_equal(out, sort(expected_const))
out <- tempfun(type = "nonfun", re_exports = FALSE, prefix = "alias_") |> sort()
expect_equal(out, sort(expected_const))

out <- tempfun(type = "reg", re_exports = FALSE) |> sort()
expect_equal(out, sort(expected_reg))
out <- tempfun(type = "reg", re_exports = FALSE, prefix = "alias_") |> sort()
expect_equal(out, sort(paste0("alias_", expected_reg)))

out <- tempfun(re_exports = FALSE) |> sort()
expect_equal(out, sort(names_exported))


# attempt to select re-exported objects when re-export = FALSE ====
exports_withre <- import_ls(
  "tinycodetfakepkg3",
  re_exports = TRUE, lib.loc = lib.loc1
)
exports_wore <- import_ls(
  "tinycodetfakepkg3",
  re_exports = FALSE, lib.loc = lib.loc1
)
re_exports <- setdiff(exports_withre, exports_wore)
expect_equal(
  re_exports,
  c("%op11%", "acf", "const11", "fun11", "rpbase11", "rpbase11<-")
)
expect_error(
  import_from(
    "tinycodetfakepkg3", exports_withre,
    re_exports = FALSE, lib.loc = lib.loc1
  ),
  pattern = "The following object names do not exist in the given package:",
  fixed = TRUE
)


# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false

