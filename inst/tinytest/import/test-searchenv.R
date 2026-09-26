
# set-up ====
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# bad name ====
pattern <- "search name must be a single string"
expect_error(
  searchenv_add(letters),
  pattern = pattern
)
expect_error(
  searchenv_get(letters),
  pattern = pattern
)
expect_error(
  searchenv_rm(letters, 2L),
  pattern = pattern
)
expect_error(
  searchenv_rm(letters),
  pattern = pattern
)

# bad pos ====
pattern <- "search position must be a single positive integer"
expect_error(
  searchenv_add("foo", 1:10),
  pattern = pattern
)
expect_error(
  searchenv_get(pos = 1:10),
  pattern = pattern
)
# expect_error(
#   searchenv_rm("foo", 1:10),
#   pattern = pattern
# )


# bad combination of name and pos ====
searchenv_add("foo1", 2L)
searchenv_add("foo2", 3L)
expect_error(
  searchenv_get("foo1", 3L),
  pattern = "search environment at position `pos` does not have name `name`"
)
expect_error(
  searchenv_get("foo2", 2L),
  pattern = "search environment at position `pos` does not have name `name`"
)
expect_error(
  searchenv_rm("foo1", 3L),
  pattern = "search environment at position `pos` does not have name `name`"
)
expect_error(
  searchenv_rm("foo2", 2L),
  pattern = "search environment at position `pos` does not have name `name`"
)
searchenv_rm("foo1")
searchenv_rm("foo2")


# duplicate search paths ====
env1 <- new.env()
env1$test1 <- "test1"
env2 <- new.env()
env2$test2 <- "test2"
attach(env1, name = "foo")
attach(env2, name = "foo")
expect_error(
  searchenv_rm("foo"),
  pattern = "duplicate search path names found; `pos` and `name` must both be specified"
)
expect_error(
  searchenv_rm(pos = 2L),
  pattern = "duplicate search path names found; `pos` and `name` must both be specified"
)
expect_error(
  searchenv_get("foo"),
  pattern = "duplicate search path names found; `pos` and `name` must both be specified"
)
expect_error(
  searchenv_get(pos = 2L),
  pattern = "duplicate search path names found; `pos` and `name` must both be specified"
)
expect_equal(
  searchenv_get("foo", 2L)$test2,
  env2$test2
)
expect_equal(
  searchenv_get("foo", 3L)$test1,
  env2$test1
)
expect_silent(
  searchenv_rm("foo", 3L)
)
expect_silent(
  searchenv_rm("foo", 2L)
)
expect_false(
  "foo" %in% search()
)


# bad env - search name not found ====
pattern <- "search name \"foo\" not found"
env <- "foo"
expect_error(
  searchenv_get(env),
  pattern = pattern
)
expect_error(
  searchenv_rm(env, 2L),
  pattern = pattern
)
expect_error(
  searchenv_rm(env),
  pattern = pattern
)


# bad env - reserved search ====
pattern <- "search name \".GlobalEnv\" is reserved for `globalenv()`"
envname <- ".GlobalEnv"
envpos <- which(search() == envname)
expect_error(
  searchenv_add(envname, envpos),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  searchenv_add(envname),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  searchenv_get(envname, envpos),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  searchenv_get(envname),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  searchenv_rm(envname, envpos),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  searchenv_rm(envname),
  pattern = pattern,
  fixed = TRUE
)

pattern <- "search name \"Autoloads\" is reserved for `.AutoloadEnv`"
envname <- "Autoloads"
envpos <- which(search() == envname)
expect_error(
  searchenv_add(envname, envpos),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  searchenv_add(envname),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  searchenv_get(envname, envpos),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  searchenv_get(envname),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  searchenv_get(pos =envpos),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  searchenv_rm(envname, envpos),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  searchenv_rm(envname),
  pattern = pattern,
  fixed = TRUE
)

search_pkgs <- search()[stringi::stri_detect(search(), fixed = "package:")]
if(length(search_pkgs) > 1L) {
  pattern <- "search names with prefix \"package:\" are reserved for `loadNamespace()`"
  envname <- search_pkgs[1]
  envpos <- which(search() == envname)
  expect_error(
    searchenv_add(envname, envpos),
    pattern = pattern,
    fixed = TRUE
  ) |> print()
  expect_error(
    searchenv_add(envname),
    pattern = pattern,
    fixed = TRUE
  ) |> print()
  expect_error(
    searchenv_get(envname, envpos),
    pattern = pattern,
    fixed = TRUE
  ) |> print()
  expect_error(
    searchenv_get(envname),
    pattern = pattern,
    fixed = TRUE
  ) |> print()
  expect_error(
    searchenv_get(pos = envpos),
    pattern = pattern,
    fixed = TRUE
  ) |> print()
  expect_error(
    searchenv_rm(envname, envpos),
    pattern = pattern,
    fixed = TRUE
  ) |> print()
  expect_error(
    searchenv_rm(envname),
    pattern = pattern,
    fixed = TRUE
  ) |> print()
}



# good search env ====
searchenv_add("my_ops")
expect_true("my_ops" %in% search())
expect_error(
  searchenv_add("my_ops"),
  pattern = "search name \"my_ops\" already exists",
  fixed = TRUE
)

exports <- import_ls("stringi", c("infix", "rp"))
import_from("stringi", exports, env = "my_ops")
foo <- searchenv_get("my_ops")
expect_true(all(exports %in% names(foo)))
expect_false(any(exports %in% ls()))

searchenv_rm("my_ops", which(search() == "my_ops"))
expect_false("my_ops" %in% search())

expect_error(
  searchenv_rm("my_ops", which(search() == "my_ops")),
  pattern = "search name \"my_ops\" not found"
)

expect_false(
  any(stringi::stri_detect(search(), fixed = "foo"))
)
expect_false(
  any(stringi::stri_detect(search(), fixed = "my_ops"))
)

