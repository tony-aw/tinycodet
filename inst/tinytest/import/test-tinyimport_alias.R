
# set-up ====
import_as(.stri ~ stringi)


# accessors ====
# regular function:
foo <- "stri_c"
expect_equal(
  .stri$stri_c,
  .stri[[foo]]
)
expect_equal(
  .stri$stri_c,
  get(foo, envir = .stri)
)

# infix:
foo <- "%s+%"
expect_equal(
  .stri$`%s+%`,
  .stri[[foo]]
)
expect_equal(
  .stri$`%s+%`,
  get(foo, envir = .stri)
)

# replacement:
foo <- "stri_sub<-"
expect_equal(
  .stri$`stri_sub<-`,
  .stri[[foo]]
)
expect_equal(
  .stri$`stri_sub<-`,
  get(foo, envir = .stri)
)

# errors
expect_error(
  .stri$foo,
  pattern = "'foo' is not an exported object in alias '.stri'"
)
bar <- "foo"
expect_error(
  .stri[[bar]],
  pattern = "'foo' is not an exported object in alias '.stri'"
)


# attr.import ====
myattr <- attr.import(.stri)
expect_equal(
  myattr$tinyimport,
  "tinyimport"
)
expect_equal(
  attr.import(.stri, "pkgs"),
  myattr$pkgs
)
expect_equal(
  attr.import(.stri, "conflicts"),
  myattr$conflicts
)
expect_equal(
  attr.import(.stri, "ordered_object_names"),
  myattr$ordered_object_names
)
expect_error(
  attr.import(.stri, "args"),
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
expect_true(is.tinyimport_alias(.stri))

ns.stri <- loadNamespace("stringi")
expect_false(is.tinyimport_alias(ns.stri))

.stri <- as.list(.stri, all.names = TRUE) |> as.environment()
lockEnvironment(.stri, bindings = TRUE)
expect_false(is.tinyimport_alias(.stri))


