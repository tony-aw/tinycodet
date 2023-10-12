# create fake packages in temporary directory:
from.dir <- file.path(getwd(), "fakelibs")
to.dir <- tempdir() |> normalizePath()
tinycodet:::.create_fake_packages(from.dir, to.dir)

expect_true(file.exists(file.path(to.dir, "fake_lib1")))
expect_true(file.exists(file.path(to.dir, "fake_lib2")))
expect_true(file.exists(file.path(to.dir, "fake_lib3")))
