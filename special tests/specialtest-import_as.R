
# THIS SCRIPT IS TO BE SOURCED

library(tinyoperations)
library(tinytest)

# get fake pkgs library folders:
lib.loc <- file.path(getwd(), "fake_library")
lib.loc2 <- file.path(getwd(), "fake_library2")
lib.loc3 <- file.path(getwd(), "fake_library3")
print(lib.loc)
print(lib.loc2)
print(lib.loc3)


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


# incorrect package error handling:
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


# missing package error handling:
expect_error(
  import_as( p3., "tinyoperationsfakepkg3", re_exports=TRUE, lib.loc=lib.loc2),
  pattern = "The following dependent packages (for the re-exports) are not installed",
  fixed = TRUE
)

expect_error(
  import_as( p3., "tinyoperationsfakepkg3", dependencies = "tinyoperationsfakepkg1", lib.loc=lib.loc2),
  pattern = "The following dependencies are not installed",
  fixed = TRUE
)

expect_error(
  import_as( p3., "tinyoperationsfakepkg3", dependencies = "tinyoperationsfakepkg2", lib.loc=lib.loc2),
  pattern = "The following dependencies are not installed",
  fixed = TRUE
)

expect_error(
  import_as(p1., "tinyoperationsfakepkg1", extensions = "tinyoperationsfakepkg3", lib.loc=lib.loc2),
  pattern = "The following extensions are not installed",
  fixed = TRUE
)

expect_error(
  import_as(p2., "tinyoperationsfakepkg2", extensions = "tinyoperationsfakepkg3", lib.loc=lib.loc2),
  pattern = "The following extensions are not installed",
  fixed = TRUE
)

expect_error(
  import_as(p1., "tinyoperationsfakepkg1", enhances = "tinyoperationsfakepkg3", lib.loc=lib.loc2),
  pattern = "The following enhances are not installed",
  fixed = TRUE
)

expect_error(
  import_as(p2., "tinyoperationsfakepkg2", enhances = "tinyoperationsfakepkg3", lib.loc=lib.loc2),
  pattern = "The following enhances are not installed",
  fixed = TRUE
)


# test import_as - re-exports:
import_as(
  p3., "tinyoperationsfakepkg3",
  re_exports=TRUE,
  lib.loc=lib.loc
)
p3 <- c(
  "fun_overwritten", "%opover%",
  "fun11",
  "fun31", "fun32", "%op31%", "%op32%"
)
out <- setdiff(names(p3.), ".__attributes__.") |> sort()
expect_equal(out,  sort(p3)) |> errorfun()
expect_true(p3.$.__attributes__.$args$re_exports) |> errorfun()


# test import_as - Dependencies:
import_as(
  p3., "tinyoperationsfakepkg3",
  re_exports = TRUE,
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
  c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2", "tinyoperationsfakepkg3 + re-exports")
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
  c("tinyoperationsfakepkg1 + re-exports", "tinyoperationsfakepkg3")
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
  c("tinyoperationsfakepkg1 + re-exports", "tinyoperationsfakepkg3")
) |> errorfun()

expect_equal(
  p3.$.__attributes__.$packages_order,
  c("tinyoperationsfakepkg1", "tinyoperationsfakepkg3")
) |> errorfun()


# test import_as - conflicts:
import_as(
  p3., "tinyoperationsfakepkg3",
  re_exports = TRUE,
  dependencies=c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2"),
  lib.loc=lib.loc
)
p3 <- data.frame(
  package=c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2", "tinyoperationsfakepkg3 + re-exports"),
  winning_conflicts = c("", "", paste0(c("fun_overwritten", "%opover%", "fun11"), collapse = ", "))
)

expect_equal(
  p3.$.__attributes__.$conflicts, p3
) |> errorfun()


# test import_as - overwriting locked environment:
# the following should simply run without issues:
expect_silent(import_as(
  new., "tinyoperationsfakepkg3",
  re_exports = TRUE,
  lib.loc=lib.loc
) |> suppressMessages()) |> errorfun()


# test loadorder:
import_as(
  new., "tinyoperationsfakepkg3", dependencies = c("tinyoperationsfakepkg2", "tinyoperationsfakepkg1"),
  loadorder = c("main_package", "dependencies", "enhances", "extensions"),
  lib.loc = lib.loc
)
expect_equal(
  new.$.__attributes__.$packages_order,
  c("tinyoperationsfakepkg3", "tinyoperationsfakepkg2",  "tinyoperationsfakepkg1")
) |> errorfun()

import_as(
  new., "tinyoperationsfakepkg3", dependencies = c("tinyoperationsfakepkg1", "tinyoperationsfakepkg2"),
  loadorder = c("main_package", "dependencies", "enhances", "extensions"),
  lib.loc = lib.loc
)
expect_equal(
  new.$.__attributes__.$packages_order,
  c("tinyoperationsfakepkg3", "tinyoperationsfakepkg1",  "tinyoperationsfakepkg2")
) |> errorfun()


# test misc attributes:
import_as(
  new., "tinyoperationsfakepkg3",
  re_exports = TRUE,
  lib.loc=lib.loc
)
expect_true("tinyimport" %in% names(new.$.__attributes__.)) |> errorfun()


