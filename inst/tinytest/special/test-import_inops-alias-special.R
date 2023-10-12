
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



# test import_inops - basics ====
temp.fun <- function() {
  import_as(
    ~to3., "tinycodetfakepkg3",
    dependencies = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
    lib.loc = lib.loc1
  )
  import_inops(
    to3.
  )
  ls() |> sort()
}
expect_equal(
  temp.fun(),
  sort(c("%op11%", "%op12%", "%op21%", "%op22%", "%op31%", "%op32%", "%opover%", "to3."))
)


# include.only ====
temp.fun <- function() {
  import_as(
    ~to3., "tinycodetfakepkg3",
    dependencies = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
    lib.loc = lib.loc1
  )
  import_inops(
    to3.,
    include.only = c("%op12%", "%op21%")
  )
  ls() |> sort()
}
expect_equal(
  temp.fun(), c("%op12%", "%op21%", "to3.")
)


# exclude ====
temp.fun <- function() {
  import_as(
    ~to3., "tinycodetfakepkg3",
    dependencies = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
    lib.loc = lib.loc1
  )
  import_inops(
    to3.,
    exclude = c("%op12%", "%op21%")
  )
  ls() |> sort()
}
expect_equal(
  temp.fun(),
  sort(c("%op11%", "%op22%", "%op31%", "%op32%", "%opover%", "to3."))
)


# unexpose ====
temp.fun <- function() {
  import_as(
    ~to3., "tinycodetfakepkg3",
    dependencies = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
    lib.loc = lib.loc1
  )
  import_inops(
    expose=to3.
  )
  import_inops(
    unexpose=to3.
  )
  ls() |> sort()
}
expect_equal(temp.fun(), c("to3."))


# test misc attributes ====
temp.fun <- function() {
  import_as(
    ~to3., "tinycodetfakepkg3",
    dependencies = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
    lib.loc = lib.loc1
  )
  import_inops(
    to3.
  )
  return(attributes(`%op31%`)|> names())
}
expect_true("tinyimport" %in% temp.fun())


# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false
