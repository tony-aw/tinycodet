# NOTE: most of the tests I do in a separate script outside of the package folder,
# as R CMD CHECK gets angry when I put fake packages inside the package folder.

# test import_as - single package:
stri <- loadNamespace("stringi") |> getNamespaceExports()
import_as(stri., "stringi")
expect_equal(names(stri.)|>sort(), sort(stri))


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
