
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

overwriters <- c("const_overwritten",  "rpbase_overwritten", "fun_overwritten", "rpbase_overwritten<-", "%opover%")
templates <- c("fun<d>", "%op<d>%", "rpbase<d><-", "rpbase<d>", "const<d>")


# test import_as - single package ====
stri <- loadNamespace("stringi") |> getNamespaceExports()
import_as(.stri ~ stringi)
out <- setdiff(names(.stri), ".__attributes__.") |> sort()
expect_equal(out, sort(stri))


# missing package error handling ====
expect_error(
  import_as(.p3 ~ tinycodetfakepkg3, deps =  "tinycodetfakepkg1", lib.loc = c("foo1", lib.loc2, "foo2")),
  pattern = "The following minimal dependencies are not installed",
  fixed = TRUE
)

expect_error(
  import_as(.p3 ~ tinycodetfakepkg3, deps =  "tinycodetfakepkg2", lib.loc = c("foo1", lib.loc2, "foo2")),
  pattern = "The following minimal dependencies are not installed",
  fixed = TRUE
)


# test import_as - re-exports ====
import_as(
  .p3 ~ tinycodetfakepkg3,
  re_exports=TRUE,
  lib.loc = c("foo1", lib.loc1, "foo2")
)

p3 <- c(
  overwriters,
  "acf",
  stringi::stri_replace_all(templates, "31", fixed = "<d>"),
  stringi::stri_replace_all(templates, "32", fixed = "<d>"),
  stringi::stri_replace_all(templates, "11", fixed = "<d>")
)
out <- setdiff(names(.p3), ".__attributes__.") |> sort()
expect_equal(out,  sort(p3))



# test import_as - deps ====
import_as(
  .p3 ~ tinycodetfakepkg3,
  re_exports = TRUE,
  deps = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
  lib.loc = c("foo1", lib.loc1, "foo2")
)
p3 <- c(
  overwriters,
  "acf",
  stringi::stri_replace_all(templates, "11", fixed = "<d>"),
  stringi::stri_replace_all(templates, "12", fixed = "<d>"),
  stringi::stri_replace_all(templates, "21", fixed = "<d>"),
  stringi::stri_replace_all(templates, "22", fixed = "<d>"),
  stringi::stri_replace_all(templates, "31", fixed = "<d>"),
  stringi::stri_replace_all(templates, "32", fixed = "<d>")
)
out <- setdiff(names(.p3), ".__attributes__.") |> sort()

expect_equal(out,  sort(p3))

expect_equal(
  attr.import(.p3, "conflicts")$package,
  c("tinycodetfakepkg1", "tinycodetfakepkg2", "tinycodetfakepkg3 + re-exports")
)

expect_equal(
  .p3$.__attributes__.$pkgs$packages_order,
  c("tinycodetfakepkg1", "tinycodetfakepkg2", "tinycodetfakepkg3")
)


# test import_as - conflicts ====
import_as(
  .p3 ~ tinycodetfakepkg3,
  re_exports = TRUE,
  deps = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
  lib.loc = c("foo1", lib.loc1, "foo2")
) |> suppressMessages()

winning_conflicts = c(overwriters, stringi::stri_replace_all(templates, "11", fixed = "<d>"))
p3 <- data.frame(
  package=c("tinycodetfakepkg1", "tinycodetfakepkg2", "tinycodetfakepkg3 + re-exports"),
  winning_conflicts = c("", "", paste0(winning_conflicts, collapse = ", "))
)

expect_equal(p3$package, attr.import(.p3, "conflicts")$package)
expect_equal(p3$winning_conflicts[1:2], attr.import(.p3, "conflicts")$winning_conflicts[1:2])

expected <- c(overwriters, stringi::stri_replace_all(templates, "11", fixed = "<d>"))
out <- strsplit(attr.import(.p3, "conflicts")$winning_conflicts[[3L]], ", ")[[1L]]

expect_equal(
  sort(expected), sort(out)
)

import_as(
  .p3 ~ tinycodetfakepkg3,
  re_exports = FALSE,
  deps = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
  lib.loc = c("foo1", lib.loc1, "foo2")
) |> suppressMessages()
p3 <- data.frame(
  package=c("tinycodetfakepkg1", "tinycodetfakepkg2", "tinycodetfakepkg3"),
  winning_conflicts = c("", "", paste0(c(overwriters), collapse = ", "))
)
expect_equal(
  attr.import(.p3, "conflicts"), p3
)



# test import_as - conflicts (different deps order) ====
import_as(
  .p3 ~ tinycodetfakepkg3,
  re_exports = TRUE,
  deps = c("tinycodetfakepkg2", "tinycodetfakepkg1"),
  lib.loc = c("foo1", lib.loc1, "foo2")
) |> suppressMessages()

winning_conflicts = c(overwriters, stringi::stri_replace_all(templates, "11", fixed = "<d>"))
p3 <- data.frame(
  package=c("tinycodetfakepkg2", "tinycodetfakepkg1", "tinycodetfakepkg3 + re-exports"),
  winning_conflicts = c("", "", paste0(winning_conflicts, collapse = ", "))
)

expect_equal(p3$package, attr.import(.p3, "conflicts")$package)
expect_equal(p3$winning_conflicts[1:2], attr.import(.p3, "conflicts")$winning_conflicts[1:2])

expected <- c(overwriters, stringi::stri_replace_all(templates, "11", fixed = "<d>"))
out <- strsplit(attr.import(.p3, "conflicts")$winning_conflicts[[3L]], ", ")[[1L]]

expect_equal(
  sort(expected), sort(out)
)

import_as(
  .p3 ~ tinycodetfakepkg3,
  re_exports = FALSE,
  deps = c("tinycodetfakepkg2", "tinycodetfakepkg1"),
  lib.loc = c("foo1", lib.loc1, "foo2")
) |> suppressMessages()
p3 <- data.frame(
  package=c("tinycodetfakepkg2", "tinycodetfakepkg1", "tinycodetfakepkg3"),
  winning_conflicts = c("", "", paste0(c(overwriters), collapse = ", "))
)
expect_equal(
  attr.import(.p3, "conflicts"), p3
)




# test misc attributes ====
import_as(
  .new ~ tinycodetfakepkg3,
  re_exports = TRUE,
  lib.loc = c("foo1", lib.loc1, "foo2")
)  |> suppressMessages()
expect_true("tinyimport" %in% names(.new$.__attributes__.))


# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false

