
# get library folder:
lib.loc <- file.path(getwd(), "fake_library")
print(lib.loc)


# test import_inops - basics ====
temp.fun <- function() {
  import_as(
    to3, "tinyoperationsfakepkg3",
    dependencies = c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2"),
    lib.loc = lib.loc
  )
  import_inops(
    to3
  )
  ls() |> sort()
}
expect_equal(
  temp.fun(),
  sort(c("%op11%", "%op12%", "%op21%", "%op22%", "%op31%", "%op32%", "%opover%", "to3"))
)


# include.only ====
temp.fun <- function() {
  import_as(
    to3, "tinyoperationsfakepkg3",
    dependencies = c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2"),
    lib.loc = lib.loc
  )
  import_inops(
    to3,
    include.only = c("%op12%", "%op21%")
  )
  ls() |> sort()
}
expect_equal(
  temp.fun(), c("%op12%", "%op21%", "to3")
)


# exclude ====
temp.fun <- function() {
  import_as(
    to3, "tinyoperationsfakepkg3",
    dependencies = c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2"),
    lib.loc = lib.loc
  )
  import_inops(
    to3,
    exclude = c("%op12%", "%op21%")
  )
  ls() |> sort()
}
expect_equal(
  temp.fun(),
  sort(c("%op11%", "%op22%", "%op31%", "%op32%", "%opover%", "to3"))
)


# unexpose ====
temp.fun <- function() {
  import_as(
    to3, "tinyoperationsfakepkg3",
    dependencies = c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2"),
    lib.loc = lib.loc
  )
  import_inops(
    expose=to3
  )
  import_inops(
    unexpose=to3
  )
  ls() |> sort()
}
expect_equal(temp.fun(), c("to3"))


# test misc attributes ====
temp.fun <- function() {
  import_as(
    to3, "tinyoperationsfakepkg3",
    dependencies = c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2"),
    lib.loc = lib.loc
  )
  import_inops(
    to3
  )
  return(attributes(`%op31%`)|> names())
}
expect_true("tinyimport" %in% temp.fun())
