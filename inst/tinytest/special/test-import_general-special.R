
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


# can't load namespace ====
pattern <- paste0(
  "to load the namespace of package `",
  "tinycodetfakepkg3",
  "`, the following packages are required but not installed:"
)
expect_error(
  import_as(~ p3., "tinycodetfakepkg3", re_exports=TRUE, lib.loc=lib.loc2),
  pattern = pattern,
  fixed = TRUE
)

expect_error(
  import_as(~ p3., "tinycodetfakepkg3", re_exports=FALSE, lib.loc=lib.loc2),
  pattern = pattern,
  fixed = TRUE
)

expect_error(
  import_as(~ p3., "tinycodetfakepkg3", re_exports=TRUE, lib.loc=lib.loc2),
  pattern = pattern,
  fixed = TRUE
)

expect_error(
  import_inops("tinycodetfakepkg3", lib.loc=lib.loc2),
  pattern = pattern,
  fixed = TRUE
)

expect_error(
  import_LL("tinycodetfakepkg3", "foo", lib.loc=lib.loc2),
  pattern = pattern,
  fixed = TRUE
)

# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false




