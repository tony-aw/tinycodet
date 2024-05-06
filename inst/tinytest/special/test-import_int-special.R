
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


expect_equal(
  import_int(tinycodet ~ .internal_prep_Namespace),
  tinycodet:::.internal_prep_Namespace
)

expect_error(
  import_int(tinycodet ~ .foo),
  pattern = "is not an internal function of"
)

expect_error(
  import_int(tinycodetfakepkg3 ~ .foo),
  pattern = "The following packages are not installed:"
)

expect_error(
  import_int(tinycodetfakepkg3 ~ .foo, lib.loc = lib.loc2),
  pattern = "the following packages are required but not installed:"
)



# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false

