
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



# test %installed in% operator ====
expect_equal(
  "foo" %installed in% lib.loc1,
  setNames(FALSE, 'foo')
)
expect_equal(
  "tinycodetfakepkg1" %installed in% lib.loc1,
  setNames(TRUE, "tinycodetfakepkg1")
)
expect_error(
  "!@#$%^&*()" %installed in% lib.loc1,
  pattern = "You have misspelled the following packages:"
)
expect_error(
  "tinycodetfakepkg1" %installed in% mean,
  pattern = "`lib.loc` must be a character vector with at least one library path"
)


# test pkg_get_deps ====
expect_equal(
  pkg_get_deps("tinycodetfakepkg1", lib.loc1, deps_type = "Enhances"),
  "tinycodetfakepkg3"
)
expect_equal(
  pkg_get_deps("tinycodetfakepkg2", lib.loc1, deps_type = "Enhances"),
  "tinycodetfakepkg3"
)
expect_equal(
  pkg_get_deps("tinycodetfakepkg3", lib.loc1, deps_type = "Depends"),
  "tinycodetfakepkg1"
)
expect_equal(
  pkg_get_deps("tinycodetfakepkg3", lib.loc1, deps_type = "Imports"),
  "tinycodetfakepkg2"
)
expect_error(
  pkg_get_deps("!@#$%^&*()", lib.loc = lib.loc1),
  pattern = "You have misspelled the following packages:"
)
expect_error(
  pkg_get_deps("tinycodetfakepkg1", lib.loc = mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  pkg_get_deps("foo", lib.loc = lib.loc1),
  pattern = "The following packages are not installed"
)


# test pkg_get_deps - core, preinst, rstudioapi, and shared_tidy ====
expect_equal(
  sort(pkg_get_deps("tinycodetfakepkg4", "Depends", lib.loc = lib.loc1, base = TRUE)),
  sort(setdiff(tinycodet:::.internal_list_coreR(), "translations"))
)
expect_equal(
  sort(pkg_get_deps("tinycodetfakepkg4", "Depends", lib.loc = lib.loc1, base = FALSE)),
  character(0)
)

expect_equal(
  sort(pkg_get_deps("tinycodetfakepkg4", "Imports", lib.loc = lib.loc1, recom = TRUE)),
  sort(tinycodet:::.internal_list_preinst())
)
expect_equal(
  pkg_get_deps("tinycodetfakepkg4", "Imports", lib.loc = lib.loc1, recom = FALSE),
  character(0)
)

expect_equal(
  sort(pkg_get_deps("tinycodetfakepkg4", "Suggests", lib.loc = lib.loc1, rstudioapi = TRUE, shared_tidy = TRUE)),
  sort(c("rlang", "lifecycle", "cli", "glue", "withr", "rstudioapi"))
)
expect_equal(
  pkg_get_deps("tinycodetfakepkg4", "Suggests", lib.loc = lib.loc1, rstudioapi = FALSE, shared_tidy = FALSE),
  character(0)
)

expect_equal(
  pkg_get_deps("tinycodetfakepkg4",
               c("Depends", "Imports"),
               lib.loc = lib.loc1,
               base = FALSE, recom = FALSE, rstudioapi = FALSE, shared_tidy = FALSE),
  pkg_get_deps_minimal("tinycodetfakepkg4", lib.loc = lib.loc1)
)


# test pkg_lsf:
ns <- loadNamespace("stringi") |> as.list(all.names=TRUE, sorted=TRUE)
names_exported <- names(ns[[".__NAMESPACE__."]][["exports"]])
ns <- ns[names_exported]
ns <- ns[!is.na(names(ns))]
names_exported <- names(ns)
inops <- grep("%|:=", names_exported, value = TRUE)
regfuns <- grep("%|:=", names_exported, value = TRUE, invert = TRUE)
expect_equal(
  pkg_lsf("stringi", "inops"),
  inops
)
expect_equal(
  pkg_lsf("stringi", "regfuns"),
  regfuns
)
expect_error(
  pkg_lsf("!@#$%^&*()", "inops", lib.loc = lib.loc1),
  pattern = "You have misspelled the following packages:"
)
expect_error(
  pkg_lsf("tinycodetfakepkg1", "inops", lib.loc = mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  pkg_get_deps("foo", "inops", lib.loc = lib.loc1),
  pattern = "The following packages are not installed"
)


# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false

