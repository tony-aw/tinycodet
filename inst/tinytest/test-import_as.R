# NOTE: most of the tests I do in a separate script outside of the package folder,
# as R CMD CHECK gets angry when I put fake packages inside the package folder.

# test import_as - single package:
stri <- loadNamespace("stringi") |> getNamespaceExports()
temp.fun <- function() {
  import_as(stri., "stringi")
  out <- setdiff(names(stri.), ".__attributes__.") |> sort()
  return(out)
}
expect_equal(temp.fun(), sort(stri))


# test import_as - functional functions:
temp.fun <- function() {
  import_as(stri., "stringi")
  stri.$stri_c("a", "b")
}
expect_equal(temp.fun(), "ab")


# test import_as - error handling:
expect_error(
  import_as(stri., "stringi", lib.loc=""),
  pattern = "The following packages are not installed"
) |> print()
expect_error(
  import_as(p1., "stringi", dependencies = "data.table"),
  pattern = "The following given dependencies were not found to be actual dependencies"
) |> print()
expect_error(
  import_as(p1., "stringi", enhances = "data.table"),
  pattern = "The following given enhances were not found to be actual enhances"
) |> print()
expect_error(
  import_as(p1., "stringi", extensions = "data.table"),
  pattern = "The following given extensions were not found to be actual reverse dependencies"
) |> print()
temp.fun <- function(){
  import_as(stri., "stringi")
  stri_c("a", "b")
}
expect_error(temp.fun(), "could not find function")
