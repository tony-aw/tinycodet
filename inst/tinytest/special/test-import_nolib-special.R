
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


loadNamespace("tinycodetfakepkg3", lib.loc1)

expect_equal(
  pkg_get_deps_minimal("tinycodetfakepkg3", NULL),
  pkg_get_deps_minimal("tinycodetfakepkg3", lib.loc1),
)

expect_silent(
  import_as(.p3NULL ~ tinycodetfakepkg3, lib.loc = lib.loc1)
)



