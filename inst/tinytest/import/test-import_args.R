
# set-up ====
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}



# bad library ====
expect_error(
  import_as(.stri ~ stringi, lib.loc=mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  import_ls("stringi", lib.loc=mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  import_from("stringi", "stri_c", lib.loc=mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  import_data("stringi", 'foo', lib.loc=mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
expect_error(
  import_diagnose(mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)


# bad env ====
pattern <- "`env` must be `NULL``, an environment, single string, or numeric scalar"
expect_error(
  import_as(.stri ~ stringi, env = list()),
  pattern = pattern
)
expect_error(
  import_from("stringi", "stri_c", env = list()),
  pattern = pattern
)
expect_error(
  import_as(.stri ~ stringi, env = letters),
  pattern = "search name must be a single string"
)
expect_error(
  import_from("stringi", "stri_c", env = letters),
  pattern = "search name must be a single string"
)
expect_error(
  import_as(.stri ~ stringi, env = 1:10),
  pattern = "search position must be a single positive integer"
)
expect_error(
  import_from("stringi", "stri_c", env = 1:10),
  pattern = "search position must be a single positive integer"
)



# bad env - search name not found ====
pattern <- "search name \"foo\" not found"
env <- "foo"
expect_error(
  import_as(.stri ~ stringi, env = env),
  pattern = pattern
)
expect_error(
  import_from("stringi", "stri_c", env = env),
  pattern = pattern
)


# bad env - reserved search ====
pattern <- "search name \".GlobalEnv\" is reserved for `globalenv()`"
envname <- ".GlobalEnv"
envpos <- which(search() == envname)
expect_error(
  import_as(.stri ~ stringi, env = envname),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  import_from("stringi", "stri_c", env = envname),
  pattern = pattern,
  fixed = TRUE
)

pattern <- "search name \"Autoloads\" is reserved for `.AutoloadEnv`"
envname <- "Autoloads"
envpos <- which(search() == envname)
expect_error(
  import_as(.stri ~ stringi, env = envname),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  import_from("stringi", "stri_c", env = envname),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  import_as(.stri ~ stringi, env = envpos),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  import_from("stringi", "stri_c", env = envpos),
  pattern = pattern,
  fixed = TRUE
)

search_pkgs <- search()[stringi::stri_detect(search(), fixed = "package:")]
if(length(search_pkgs) > 1L) {
  pattern <- "search names with prefix \"package:\" are reserved for `loadNamespace()`"
  envname <- search_pkgs[1]
  envpos <- which(search() == envname)
  expect_error(
    import_as(.stri ~ stringi, env = envname),
    pattern = pattern,
    fixed = TRUE
  )
  expect_error(
    import_from("stringi", "stri_c", env = envname),
    pattern = pattern,
    fixed = TRUE
  )
  expect_error(
    import_as(.stri ~ stringi, env = envpos),
    pattern = pattern,
    fixed = TRUE
  )
  expect_error(
    import_from("stringi", "stri_c", env = envpos),
    pattern = pattern,
    fixed = TRUE
  )
}


# bad env - out of bound position ====
pattern <- "search position out of bounds"
envpos <- 1L
expect_error(
  import_as(.stri ~ stringi, env = envpos),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  import_from("stringi", "stri_c", env = envpos),
  pattern = pattern,
  fixed = TRUE
)
envpos <- length(search())
expect_error(
  import_as(.stri ~ stringi, env = envpos),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  import_from("stringi", "stri_c", env = envpos),
  pattern = pattern,
  fixed = TRUE
)


# bad env - duplicate names ====
env1 <- new.env()
env1$test1 <- "test1"
env2 <- new.env()
env2$test2 <- "test2"
attach(env1, name = "foo")
attach(env2, name = "foo")
expect_error(
  import_as(.stri ~ stringi, env = "foo"),
  pattern = "duplicate search names found; fix this first"
)
expect_error(
  import_from("stringi", "stri_c", env = "foo"),
  pattern = "duplicate search names found; fix this first"
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


# good local env ====
localfun1 <- function() {
  exports <- import_ls("stringi", c("infix", "rp"))
  import_from("stringi", exports)
  out <- ls()
  return(out)
}
localfun2 <- function() {
  import_as(.stri ~ stringi)
  out <- names(.stri)
  return(out)
}
exports <- import_ls("stringi", c("infix", "rp"))
expect_true(all(exports %in% localfun1()))
expect_true(all(exports %in% localfun2()))


# good custom env ====
myenv <- new.env()
exports <- import_ls("stringi", c("infix", "rp"))
import_from("stringi", exports, env = myenv)
expect_true(all(exports %in% ls(myenv)))
expect_false(any(exports %in% ls()))
rm(myenv)
myenv <- new.env()
import_as(.stri ~ stringi, env = myenv)
expect_true(is.tinyimport_alias(myenv$.stri))
expect_true(all(exports %in% ls(myenv$.stri)))
expect_false(any(exports %in% ls()))


# good search env ====
searchenv_add("my_ops")
exports <- import_ls("stringi", c("infix", "rp"))
import_from("stringi", exports, env = "my_ops")
foo <- searchenv_get("my_ops")
expect_true(all(exports %in% names(foo)))
expect_false(any(exports %in% ls()))

searchenv_add("my_aliases")
import_as(.stri ~ stringi, env = "my_aliases")
foo <- searchenv_get("my_aliases")
expect_true(all(exports %in% names(foo$.stri)))
expect_false(any(exports %in% ls()))

# clean-up: 
searchenv_rm("my_ops", which(search() == "my_ops"))
searchenv_rm("my_aliases", which(search() == "my_aliases"))



