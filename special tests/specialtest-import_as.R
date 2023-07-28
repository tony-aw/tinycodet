
# THIS SCRIPT IS TO BE SOURCED FROM ELSEWHERE

library(tinyoperations)
library(tinytest)

# get library folder:
lib.loc <- file.path(getwd(), "fake_library")
print(lib.loc)


# error print function:
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

# test import_as - single package:
stri <- loadNamespace("stringi") |> getNamespaceExports()
import_as(stri., "stringi")
out <- setdiff(names(stri.), ".__attributes__.") |> sort()
expect_equal(out, sort(stri)) |> errorfun()


# test import_as - error handling:
expect_error(
  import_as(stri., "stringi", lib.loc=lib.loc),
  pattern = "The following packages are not installed"
) |> errorfun()
expect_error(
  import_as(p1., "tinyoperationsfakepkg1", lib.loc=c(lib.loc, .libPaths()), dependencies = "data.table"),
  pattern = "The following given dependencies were not found to be actual dependencies"
) |> errorfun()
expect_error(
  import_as(p1., "tinyoperationsfakepkg1", lib.loc=c(lib.loc, .libPaths()), enhances = "data.table"),
  pattern = "The following given enhances were not found to be actual enhances"
) |> errorfun()
expect_error(
  import_as(p1., "tinyoperationsfakepkg1", lib.loc=c(lib.loc, .libPaths()), extensions = "data.table"),
  pattern = "The following given extensions were not found to be actual reverse dependencies"
) |> errorfun()


# test import_as - foreign exports:
import_as(
  p3., "tinyoperationsfakepkg3",
  foreign_exports=TRUE,
  lib.loc=lib.loc
)
p3 <- c(
  "fun_overwritten", "%opover%",
  "fun11",
  "fun31", "fun32", "%op31%", "%op32%"
)
out <- setdiff(names(p3.), ".__attributes__.") |> sort()
expect_equal(out,  sort(p3)) |> errorfun()
expect_true(p3.$.__attributes__.$args$foreign_exports) |> errorfun()


# test import_as - Dependencies:
import_as(
  p3., "tinyoperationsfakepkg3",
  foreign_exports = TRUE,
  dependencies=c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2"),
  lib.loc=lib.loc
)
p3 <- c(
  "fun_overwritten", "%opover%",
  "fun11", "fun12", "%op11%", "%op12%",
  "fun21", "fun22", "%op21%", "%op22%",
  "fun31", "fun32", "%op31%", "%op32%"
)
out <- setdiff(names(p3.), ".__attributes__.") |> sort()
expect_equal(out,  sort(p3)) |> errorfun()
expect_equal(
  p3.$.__attributes__.$conflicts$package,
  c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2", "tinyoperationsfakepkg3 + foreign exports")
) |> errorfun()
expect_equal(
  p3.$.__attributes__.$packages_order,
  c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2", "tinyoperationsfakepkg3")
) |> errorfun()


# test import_as - enhances:
import_as(
  p3., "tinyoperationsfakepkg1",
  enhances = "tinyoperationsfakepkg3",
  lib.loc=lib.loc
)
p3 <- c(
  "fun_overwritten", "%opover%",
  "fun11", "fun12", "%op11%", "%op12%",
  "fun31", "fun32", "%op31%", "%op32%"
)
out <- setdiff(names(p3.), ".__attributes__.") |> sort()
expect_equal(out,  sort(p3)) |> errorfun()
expect_equal(
  p3.$.__attributes__.$conflicts$package,
  c("tinyoperationsfakepkg1 + foreign exports", "tinyoperationsfakepkg3")
) |> errorfun()
expect_equal(
  p3.$.__attributes__.$packages_order,
  c("tinyoperationsfakepkg1", "tinyoperationsfakepkg3")
) |> errorfun()


# test import_as - extensions:
import_as(
  p3., "tinyoperationsfakepkg1",
  extensions = "tinyoperationsfakepkg3",
  lib.loc=lib.loc
)
p3 <- c(
  "fun_overwritten", "%opover%",
  "fun11", "fun12", "%op11%", "%op12%",
  "fun31", "fun32", "%op31%", "%op32%"
)
out <- setdiff(names(p3.), ".__attributes__.") |> sort()
expect_equal(out,  sort(p3)) |> errorfun()
expect_equal(
  p3.$.__attributes__.$conflicts$package,
  c("tinyoperationsfakepkg1 + foreign exports", "tinyoperationsfakepkg3")
) |> errorfun()
expect_equal(
  p3.$.__attributes__.$packages_order,
  c("tinyoperationsfakepkg1", "tinyoperationsfakepkg3")
) |> errorfun()


# test import_as - conflicts:
import_as(
  p3., "tinyoperationsfakepkg3",
  foreign_exports = TRUE,
  dependencies=c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2"),
  lib.loc=lib.loc
)
p3 <- data.frame(
  package=c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2", "tinyoperationsfakepkg3 + foreign exports"),
  winning_conflicts = c("", "", paste0(c("fun_overwritten", "%opover%", "fun11"), collapse = ", "))
)
expect_equal(
  p3.$.__attributes__.$conflicts, p3
) |> errorfun()

# test import_as - overwriting locked environment:
# the following should simply run without issues:
import_as(
  new., "tinyoperationsfakepkg3",
  foreign_exports = TRUE,
  lib.loc=lib.loc
)

# test misc attributes:
import_as(
  new., "tinyoperationsfakepkg3",
  foreign_exports = TRUE,
  lib.loc=lib.loc
)
expect_true("tinyimport" %in% names(new.$.__attributes__.)) |> errorfun()
