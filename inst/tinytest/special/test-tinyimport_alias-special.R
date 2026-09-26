
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


# attr.import ====
import_as(
  .p3 ~ tinycodetfakepkg3,
  deps = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
  lib.loc = lib.loc1
)

myattr <- attr.import(.p3)
expect_equal(
  myattr$tinyimport,
  "tinyimport"
)
expect_equal(
  attr.import(.p3, "pkgs"),
  myattr$pkgs
)
expect_equal(
  attr.import(.p3, "conflicts"),
  myattr$conflicts
)
expect_equal(
  attr.import(.p3, "ordered_object_names"),
  myattr$ordered_object_names
)
expect_error(
  attr.import(.p3, "args"),
  pattern = "unknown `which` given"
)
expect_error(
  attr.import(environment()),
  pattern = "`alias` must be a locked environment as returned by `import_as()`",
  fixed = TRUE
)


# attr.import (different deps order) ====
import_as(
  .p3 ~ tinycodetfakepkg3,
  deps = c("tinycodetfakepkg2", "tinycodetfakepkg1"),
  lib.loc = lib.loc1
)

myattr <- attr.import(.p3)
expect_equal(
  myattr$tinyimport,
  "tinyimport"
)
expect_equal(
  attr.import(.p3, "pkgs"),
  myattr$pkgs
)
expect_equal(
  attr.import(.p3, "conflicts"),
  myattr$conflicts
)

expect_equal(
  attr.import(.p3, "ordered_object_names"),
  myattr$ordered_object_names
)
expect_error(
  attr.import(.p3, "foo"),
  pattern = "unknown `which` given"
)
expect_error(
  attr.import(environment()),
  pattern = "`alias` must be a locked environment as returned by `import_as()`",
  fixed = TRUE
)


# is.tinyimport - error checks ====
# set-up
strip.attributes <- function(x) {
  attributes(x) <- NULL
  return(x)
}

expect_false(is.tinyimport_alias(environment()))



# is.tinyimport - alias checks ====
import_as(
  .p3 ~ tinycodetfakepkg3,
  deps = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
  lib.loc = lib.loc1
)
expect_true(is.tinyimport_alias(.p3))

ns.p3 <- loadNamespace("tinycodetfakepkg3")
expect_false(is.tinyimport_alias(ns.p3))

.p3 <- as.list(.p3, all.names = TRUE) |> as.environment()
lockEnvironment(.p3, bindings = TRUE)
expect_false(is.tinyimport_alias(.p3))




# is.tinyimport - alias checks (different deps order) ====
import_as(
  .p3 ~ tinycodetfakepkg3,
  deps = c("tinycodetfakepkg2", "tinycodetfakepkg1"),
  lib.loc = lib.loc1
)
expect_true(is.tinyimport_alias(.p3))

ns.p3 <- loadNamespace("tinycodetfakepkg3")
expect_false(is.tinyimport_alias(ns.p3))

.p3 <- as.list(.p3, all.names = TRUE) |> as.environment()
lockEnvironment(.p3, bindings = TRUE)
expect_false(is.tinyimport_alias(.p3))


