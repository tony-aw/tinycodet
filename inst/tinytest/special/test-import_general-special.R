
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


# functional base functions from separate library ====
# single lib.loc
import_as(~new., "tinycodetfakepkg5", lib.loc = lib.loc1)
expect_equal(new.$fun_paste("a", "b"), "ab")

temp.fun <- function() {
  import_inops("tinycodetfakepkg5", lib.loc = lib.loc1)
  "a" %paste0% "b"
}
expect_equal(temp.fun(), "ab")
temp.fun <- function() {
  import_LL("tinycodetfakepkg5", "fun_paste", lib.loc = lib.loc1)
  fun_paste("a", "b")
}
expect_equal(temp.fun(), "ab")


# multi lib.loc
import_as(~new., "tinycodetfakepkg5", lib.loc = c("foo", lib.loc1))
expect_equal(new.$fun_paste("a", "b"), "ab")

temp.fun <- function() {
  import_inops("tinycodetfakepkg5", lib.loc = c("foo", lib.loc1))
  "a" %paste0% "b"
}
expect_equal(temp.fun(), "ab")
temp.fun <- function() {
  import_LL("tinycodetfakepkg5", "fun_paste", lib.loc = c("foo", lib.loc1))
  fun_paste("a", "b")
}
expect_equal(temp.fun(), "ab")



# can't load namespace ====
pattern <- paste0(
  "to load the namespace of package `",
  "tinycodetfakepkg3",
  "`, the following packages are required but not installed:"
)

# single lib.loc
expect_error(
  import_as(~ p3., "tinycodetfakepkg3", re_exports=TRUE, lib.loc = c("foo1", lib.loc2, "foo2")),
  pattern = pattern,
  fixed = TRUE
)

expect_error(
  import_as(~ p3., "tinycodetfakepkg3", re_exports=FALSE, lib.loc = c("foo1", lib.loc2, "foo2")),
  pattern = pattern,
  fixed = TRUE
)

expect_error(
  import_as(~ p3., "tinycodetfakepkg3", re_exports=TRUE, lib.loc = c("foo1", lib.loc2, "foo2")),
  pattern = pattern,
  fixed = TRUE
)

expect_error(
  import_inops("tinycodetfakepkg3", lib.loc = c("foo1", lib.loc2, "foo2")),
  pattern = pattern,
  fixed = TRUE
)

expect_error(
  import_LL("tinycodetfakepkg3", "foo", lib.loc = c("foo1", lib.loc2, "foo2")),
  pattern = pattern,
  fixed = TRUE
)

# multi lib.loc
expect_error(
  import_as(~ p3., "tinycodetfakepkg3", re_exports=TRUE, lib.loc = c("foo", lib.loc2)),
  pattern = pattern,
  fixed = TRUE
)

expect_error(
  import_as(~ p3., "tinycodetfakepkg3", re_exports=FALSE, lib.loc = c("foo", lib.loc2)),
  pattern = pattern,
  fixed = TRUE
)

expect_error(
  import_as(~ p3., "tinycodetfakepkg3", re_exports=TRUE, lib.loc = c("foo", lib.loc2)),
  pattern = pattern,
  fixed = TRUE
)

expect_error(
  import_inops("tinycodetfakepkg3", lib.loc = c("foo", lib.loc2)),
  pattern = pattern,
  fixed = TRUE
)

expect_error(
  import_LL("tinycodetfakepkg3", "foo", lib.loc = c("foo", lib.loc2)),
  pattern = pattern,
  fixed = TRUE
)


# function attributes - import_as ====

# single lib.loc
import_as(
  ~ p3., "tinycodetfakepkg3",
  re_exports = FALSE,
  dependencies = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
  lib.loc = lib.loc1
)
checklist <- as.list(p3.)
checklist <- checklist[order(names(checklist))]
expect <- rep("tinycodetfakepkg3", length(checklist))
expect[stringi::stri_detect(names(checklist), regex = "1\\d")] <- "tinycodetfakepkg1"
expect[stringi::stri_detect(names(checklist), regex = "2\\d")] <- "tinycodetfakepkg2"

out <- sapply(checklist, \(x)attr(x, "package")) |> unname()

expect_equal(
  expect, out
)

# multi lib.loc
import_as(
  ~ p3., "tinycodetfakepkg3",
  re_exports = FALSE,
  dependencies = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
  lib.loc = c("foo", lib.loc1)
)
checklist <- as.list(p3.)
checklist <- checklist[order(names(checklist))]
expect <- rep("tinycodetfakepkg3", length(checklist))
expect[stringi::stri_detect(names(checklist), regex = "1\\d")] <- "tinycodetfakepkg1"
expect[stringi::stri_detect(names(checklist), regex = "2\\d")] <- "tinycodetfakepkg2"

out <- sapply(checklist, \(x)attr(x, "package")) |> unname()

expect_equal(
  expect, out
)


# function attributes - import_inops ====
# single lib.loc
tempfun <- function() {
  import_as(
    ~ p3., "tinycodetfakepkg3",
    re_exports = FALSE,
    dependencies = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
    lib.loc = lib.loc1
  )
  import_inops(p3.)
  funnames <- setdiff(ls(), "p3.")
  funs <- mget(funnames, mode = "function")
  out <- sapply(funs, \(x)attr(x, "package"))
  return(out)
}

expect <- c(
  rep("tinycodetfakepkg1", 2),
  rep("tinycodetfakepkg2", 2),
  rep("tinycodetfakepkg3", 3)
)
out <- tempfun() |> unname()

expect_equal(expect, out)

# multi lib.loc
tempfun <- function() {
  import_as(
    ~ p3., "tinycodetfakepkg3",
    re_exports = FALSE,
    dependencies = c("tinycodetfakepkg1", "tinycodetfakepkg2"),
    lib.loc = c("foo", lib.loc1)
  )
  import_inops(p3.)
  funnames <- setdiff(ls(), "p3.")
  funs <- mget(funnames, mode = "function")
  out <- sapply(funs, \(x)attr(x, "package"))
  return(out)
}

expect <- c(
  rep("tinycodetfakepkg1", 2),
  rep("tinycodetfakepkg2", 2),
  rep("tinycodetfakepkg3", 3)
)
out <- tempfun() |> unname()

expect_equal(expect, out)


# function attributes - import_LL ====
# single lib.loc
tempfun <- function() {
  import_LL("tinycodetfakepkg1", "fun11", lib.loc = lib.loc1)
  import_LL("tinycodetfakepkg2", "fun21", lib.loc = lib.loc1)
  import_LL("tinycodetfakepkg3", "fun31", lib.loc = lib.loc1)
  out <- c(attr(fun11, "package"), attr(fun21, "package"), attr(fun31, "package"))
  return(out)
}

expect <- c("tinycodetfakepkg1", "tinycodetfakepkg2", "tinycodetfakepkg3")

out <- tempfun()

expect_equal(expect, out)

# multi lib.loc
tempfun <- function() {
  import_LL("tinycodetfakepkg1", "fun11", lib.loc = c("foo", lib.loc1))
  import_LL("tinycodetfakepkg2", "fun21", lib.loc = c("foo", lib.loc1))
  import_LL("tinycodetfakepkg3", "fun31", lib.loc = c("foo", lib.loc1))
  out <- c(attr(fun11, "package"), attr(fun21, "package"), attr(fun31, "package"))
  return(out)
}

expect <- c("tinycodetfakepkg1", "tinycodetfakepkg2", "tinycodetfakepkg3")

out <- tempfun()

expect_equal(expect, out)



# clean-up ====
# dir2remove <- file.path(to.dir, list.files(to.dir)) |> normalizePath()
# unlink(dir2remove, recursive = TRUE, force = TRUE)
# file.exists(dir2remove) # <- should be false




