
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


# test import_as - single package ====
stri <- loadNamespace("stringi") |> getNamespaceExports()
import_as(~stri., "stringi")
out <- setdiff(names(stri.), ".__attributes__.") |> sort()
expect_equal(out, sort(stri))


# incorrect package error handling ====
expect_error(
  import_as(~p1., "tinycodetfakepkg1", lib.loc=c(lib.loc1, .libPaths()), dependencies = "stringi"),
  pattern = "The following given dependencies were not found to be actual dependencies"
)


expect_error(
  import_as(~p1., "tinycodetfakepkg1", lib.loc=c(lib.loc1, .libPaths()), extensions = "stringi"),
  pattern = "The following given extensions were not found to be actual extensions"
)


# duplicate package error handling ====
dupli_pkg <- "stringi"
expect_error(
  import_as(~p1., "tinycodetfakepkg1", lib.loc=c(lib.loc1, .libPaths()),
            dependencies = rep(dupli_pkg, 2)),
  pattern = paste0("The following duplicate dependencies given:", "\n",
                   dupli_pkg)
)


expect_error(
  import_as(~p1., "tinycodetfakepkg1", lib.loc=c(lib.loc1, .libPaths()),
            extensions = rep(dupli_pkg, 2)),
  pattern = paste0("The following duplicate extensions given:", "\n",
                   dupli_pkg)
)

dupli_pkg <- "tidytable"
expect_error(
  import_as(~p1., "tinycodetfakepkg1", lib.loc=c(lib.loc1, .libPaths()),
            dependencies = rep(dupli_pkg, 2)),
  pattern = paste0("The following duplicate dependencies given:", "\n",
                   dupli_pkg)
)


expect_error(
  import_as(~p1., "tinycodetfakepkg1", lib.loc=c(lib.loc1, .libPaths()),
            extensions = rep(dupli_pkg, 2)),
  pattern = paste0("The following duplicate extensions given:", "\n",
                   dupli_pkg)
)


# missing package error handling ====
expect_error(
  import_as(~ p3., "tinycodetfakepkg3", dependencies = "tinycodetfakepkg1", lib.loc=lib.loc2),
  pattern = "The following dependencies are not installed",
  fixed = TRUE
)

expect_error(
  import_as(~ p3., "tinycodetfakepkg3", dependencies = "tinycodetfakepkg2", lib.loc=lib.loc2),
  pattern = "The following dependencies are not installed",
  fixed = TRUE
)

expect_error(
  import_as(~p1., "tinycodetfakepkg1", extensions = "tinycodetfakepkg3", lib.loc=lib.loc3),
  pattern = "The following extensions are not installed",
  fixed = TRUE
)

expect_error(
  import_as(~p2., "tinycodetfakepkg2", extensions = "tinycodetfakepkg3", lib.loc=lib.loc3),
  pattern = "The following extensions are not installed",
  fixed = TRUE
)


# test import_as - re-exports ====
import_as(
  ~p3., "tinycodetfakepkg3",
  re_exports=TRUE,
  lib.loc = lib.loc1
)
p3 <- c(
  "fun_overwritten", "%opover%",
  "fun11",
  "fun31", "fun32", "%op31%", "%op32%"
)
out <- setdiff(names(p3.), ".__attributes__.") |> sort()
expect_equal(out,  sort(p3))
expect_true(p3.$.__attributes__.$args$re_exports)
expect_false("acf" %in% names(p3.)) # expect base packages to be NOT re-exported
expect_equal(
  import_as(~ p3., "tinycodetfakepkg3", re_exports = FALSE, dependencies = "tinycodetfakepkg1", lib.loc = lib.loc1),
  import_as(~ p3., "tinycodetfakepkg1", re_exports = FALSE, extensions = "tinycodetfakepkg3", lib.loc = lib.loc1),
)


# test import_as - Dependencies ====
import_as(
  ~p3., "tinycodetfakepkg3",
  re_exports = TRUE,
  dependencies=c("tinycodetfakepkg1", "tinycodetfakepkg2"),
  lib.loc=lib.loc1
)
p3 <- c(
  "fun_overwritten", "%opover%",
  "fun11", "fun12", "%op11%", "%op12%",
  "fun21", "fun22", "%op21%", "%op22%",
  "fun31", "fun32", "%op31%", "%op32%"
)
out <- setdiff(names(p3.), ".__attributes__.") |> sort()

expect_equal(out,  sort(p3))

expect_equal(
  p3.$.__attributes__.$conflicts$package,
  c("tinycodetfakepkg1", "tinycodetfakepkg2", "tinycodetfakepkg3 + re-exports")
)

expect_equal(
  p3.$.__attributes__.$pkgs$packages_order,
  c("tinycodetfakepkg1", "tinycodetfakepkg2", "tinycodetfakepkg3")
)


# test import_as - extensions ====
import_as(
  ~ p3., "tinycodetfakepkg1",
  extensions = "tinycodetfakepkg3",
  lib.loc=lib.loc1
)
p3 <- c(
  "fun_overwritten", "%opover%",
  "fun11", "fun12", "%op11%", "%op12%",
  "fun31", "fun32", "%op31%", "%op32%"
)
out <- setdiff(names(p3.), ".__attributes__.") |> sort()

expect_equal(out,  sort(p3))

expect_equal(
  p3.$.__attributes__.$conflicts$package,
  c("tinycodetfakepkg1 + re-exports", "tinycodetfakepkg3")
)

expect_equal(
  p3.$.__attributes__.$pkgs$packages_order,
  c("tinycodetfakepkg1", "tinycodetfakepkg3")
)


# test import_as - conflicts ====
import_as(
  ~ p3., "tinycodetfakepkg3",
  re_exports = TRUE,
  dependencies=c("tinycodetfakepkg1", "tinycodetfakepkg2"),
  lib.loc=lib.loc1
) |> suppressMessages()
p3 <- data.frame(
  package=c("tinycodetfakepkg1", "tinycodetfakepkg2", "tinycodetfakepkg3 + re-exports"),
  winning_conflicts = c("", "", paste0(c("fun_overwritten", "%opover%", "fun11"), collapse = ", "))
)
expect_equal(
  p3.$.__attributes__.$conflicts, p3
)

import_as(
  ~ p3., "tinycodetfakepkg3",
  re_exports = FALSE,
  dependencies=c("tinycodetfakepkg1", "tinycodetfakepkg2"),
  lib.loc=lib.loc1
) |> suppressMessages()
p3 <- data.frame(
  package=c("tinycodetfakepkg1", "tinycodetfakepkg2", "tinycodetfakepkg3"),
  winning_conflicts = c("", "", paste0(c("fun_overwritten", "%opover%"), collapse = ", "))
)
expect_equal(
  p3.$.__attributes__.$conflicts, p3
)

import_as(
  ~ p3., "tinycodetfakepkg3",
  re_exports = TRUE,
  dependencies=c("tinycodetfakepkg1", "tinycodetfakepkg2"),
  lib.loc=lib.loc1,
  import_order = c("main_package", "dependencies", "extensions")
) |> suppressMessages()
p3 <- data.frame(
  package=c("tinycodetfakepkg3 + re-exports", "tinycodetfakepkg1", "tinycodetfakepkg2"),
  winning_conflicts = c("", "", paste0(c("fun_overwritten", "%opover%"), collapse = ", "))
)
expect_equal(
  p3.$.__attributes__.$conflicts, p3
)


# test import_as - overwriting locked environment ====
# the following should simply run without issues:
expect_silent(import_as(
  ~ new., "tinycodetfakepkg3",
  re_exports = TRUE,
  lib.loc=lib.loc1
) |> suppressMessages())


# test import_as - import_order ====
import_as(
  ~ new., "tinycodetfakepkg3",
  re_exports = FALSE,
  dependencies = c("tinycodetfakepkg2", "tinycodetfakepkg1"),
  import_order = c("main_package", "dependencies", "extensions"),
  lib.loc = lib.loc1
)  |> suppressMessages()
expect_equal(
  new.$.__attributes__.$pkgs$packages_order,
  c("tinycodetfakepkg3", "tinycodetfakepkg2",  "tinycodetfakepkg1")
)
ordered_object_names <- sapply(
  c("tinycodetfakepkg3", "tinycodetfakepkg2",  "tinycodetfakepkg1"),
  \(x)pkg_lsf(x, type = "all", lib.loc = lib.loc1)
) |> as.character() |> unique()
expect_equal(
  new.$.__attributes__.$ordered_object_names,
  ordered_object_names
)

import_as(
  ~ new., "tinycodetfakepkg1",
  re_exports = FALSE,
  extensions = c("tinycodetfakepkg3"),
  import_order = c("extensions", "main_package", "dependencies"),
  lib.loc = lib.loc1
) |> suppressMessages()
expect_equal(
  new.$.__attributes__.$pkgs$packages_order,
  c("tinycodetfakepkg3", "tinycodetfakepkg1")
)
ordered_object_names <- sapply(
  c("tinycodetfakepkg3", "tinycodetfakepkg1"),
  \(x)pkg_lsf(x, type = "all", lib.loc = lib.loc1)
) |> as.character() |> unique()
expect_equal(
  new.$.__attributes__.$ordered_object_names,
  ordered_object_names
)

import_as(
  ~ new., "tinycodetfakepkg1",
  re_exports = FALSE,
  import_order = c("main_package", "dependencies", "extensions"),
  lib.loc = lib.loc1
) |> suppressMessages()
expect_equal(
  new.$.__attributes__.$pkgs$packages_order,
  c("tinycodetfakepkg1")
)
ordered_object_names <- sapply(
  c("tinycodetfakepkg1"),
  \(x)pkg_lsf(x, type = "all", lib.loc = lib.loc1)
) |> as.character() |> unique()
expect_equal(
  new.$.__attributes__.$ordered_object_names,
  ordered_object_names
)


# test misc attributes ====
import_as(
  ~ new., "tinycodetfakepkg3",
  re_exports = TRUE,
  lib.loc=lib.loc1
)  |> suppressMessages()
expect_true("tinyimport" %in% names(new.$.__attributes__.))


# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false
