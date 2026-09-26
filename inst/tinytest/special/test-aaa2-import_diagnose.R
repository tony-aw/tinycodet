
# set-up ====
from.dir <- file.path(getwd(), "fakelibs")
to.dir <- tempdir() |> normalizePath()
# tinycodet:::.create_fake_packages(from.dir, to.dir)
lib.loc1 <- file.path(to.dir, "fake_lib1")
lib.loc2 <- file.path(to.dir, "fake_lib2")
lib.loc3 <- file.path(to.dir, "fake_lib3")
lib.locnew <- file.path(to.dir, "newlib")
print(lib.loc1)
print(lib.loc2)
print(lib.loc3)
print(lib.locnew)

# this is to be checked BEFORE loading tinycodetfakepkg1
# (hence the "aaa" in the test file name)

# Main Function ====

pkg <- "tinycodetfakepkg1"

expect_false(
  pkg %in% loadedNamespaces()
)

expect_error(
  pkg_get_deps_minimal(pkg, lib.loc = NULL),
  pattern = "`lib.loc` is `NULL`, but namespace of given package is not loaded"
)

loadNamespace(pkg, lib.loc = lib.loc1)
check <- import_diagnose(lib.loc = c(lib.locnew, .libPaths()))
check <- check[check$package != "tinycodet",]
expect_true(nrow(check) == 1)
expect_equal(
  lapply(check, class),
  list(package = "character",
       installed_in_lib.loc = "logical",
       version_loaded = "character",
       version_installed = "character",
       versions_equal = "logical"
  )
)
expect_true(
  utils::compareVersion(check$version_loaded, check$version_installed) == -1
)
expect_true(
  utils::compareVersion(check$version_installed, "1.0") == 0
)
expect_true(
  utils::compareVersion(check$version_loaded, "0.0.0.9000") == 0
)
expect_true(
  utils::compareVersion(check$version_loaded, getNamespaceVersion(pkg)) == 0
)

check <- import_diagnose(c(lib.locnew, lib.loc1))
check <- check[check$package == pkg,]
expect_false(
  check$versions_equal[check$package != "tinycode"]
)

check <- import_diagnose(lib.loc = c(lib.loc1, lib.locnew))
check <- check[check$package == pkg,]
expect_true(nrow(check) == 0L)


# version mismatch warning ====
pattern1 <- "The following packages/dependencies have a different loaded version than the version installed in `lib.loc`"
pattern2 <- "one or more potential `lib.loc` issues found; it is recommended you run `import_diagnose()` to investigate!"

expect_warning(
  import_as(.p3 ~ tinycodetfakepkg3, lib.loc = c(lib.loc2, lib.locnew)),
  pattern = pattern1,
  fixed = TRUE
)
expect_warning(
  import_as(.p3 ~ tinycodetfakepkg3, lib.loc = c(lib.loc2, lib.locnew)),
  pattern = pattern2,
  fixed = TRUE
)

expect_warning(
  import_ls("tinycodetfakepkg3", lib.loc = c(lib.loc2, lib.locnew)),
  pattern = pattern1,
  fixed = TRUE
)
expect_warning(
  import_ls("tinycodetfakepkg3", lib.loc = c(lib.loc2, lib.locnew)),
  pattern = pattern2,
  fixed = TRUE
)

myls <- import_ls("tinycodetfakepkg3", "infix", lib.loc = c(lib.loc2, lib.locnew))
expect_warning(
  import_from("tinycodetfakepkg3", myls, lib.loc = c(lib.loc2, lib.locnew)),
  pattern = pattern1,
  fixed = TRUE
)
expect_warning(
  import_from("tinycodetfakepkg3", myls, lib.loc = c(lib.loc2, lib.locnew)),
  pattern = pattern2,
  fixed = TRUE
)


# missing dependencies warning ====

pattern1 <- "The following packages/dependencies do NOT exist in `lib.loc`, but were already loaded BEFORE import started"
pattern2 <- "one or more potential `lib.loc` issues found; it is recommended you run `import_diagnose()` to investigate!"

expect_warning(
  import_as(.p3 ~ tinycodetfakepkg3, lib.loc = c(lib.loc2)),
  pattern = pattern1,
  fixed = TRUE
)
expect_warning(
  import_as(.p3 ~ tinycodetfakepkg3, lib.loc = c(lib.loc2)),
  pattern = pattern2,
  fixed = TRUE
)

expect_warning(
  import_ls("tinycodetfakepkg3", lib.loc = c(lib.loc2)),
  pattern = pattern1,
  fixed = TRUE
)
expect_warning(
  import_ls("tinycodetfakepkg3", lib.loc = c(lib.loc2)),
  pattern = pattern2,
  fixed = TRUE
)

myls <- import_ls("tinycodetfakepkg3", "infix", lib.loc = c(lib.loc2))
expect_warning(
  import_from("tinycodetfakepkg3", myls, lib.loc = c(lib.loc2)),
  pattern = pattern1,
  fixed = TRUE
)
expect_warning(
  import_from("tinycodetfakepkg3", myls, lib.loc = c(lib.loc2)),
  pattern = pattern2,
  fixed = TRUE
)

# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false
