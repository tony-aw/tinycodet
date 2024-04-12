# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}



# warnings ====
x <- "hello"
p <- "a|e|i|o|u"
expect_warning(strfind(x, p, rt = "dict"))
expect_warning(strfind(x, p, i = "all") <- "a")


# errors ====
x <- "hello"
p <- "a|e|i|o|u"
expect_error(
  strfind(x, p, i = "bah"),
  pattern = "improper `i` given",
)
expect_error(
  strfind(x, p, i = letters),
  pattern = "improper `i` given",
  fixed = TRUE
)
expect_error(
  strfind(x, p, rt = letters) <- "a",
  pattern = "improper `rt` given",
  fixed = TRUE
)
expect_error(
  strfind(x, p, rt = "foo") <- "a",
  pattern = "improper `rt` given",
  fixed = TRUE
)
expect_error(
  strfind(x, p, rt = "dict") <- list(~ a + b),
  pattern = "right-hand side must be atomic",
  fixed = TRUE
)
  

expect_error(
  strfind(x, 1),
  pattern = "`p` must be a character vector or list"
)
expect_error(
  strfind(x, 1, i = "all"),
  pattern = "`p` must be a character vector or list"
)
expect_error(
  strfind(x, 1, i = -2),
  pattern = "`p` must be a character vector or list"
)
expect_error(
  strfind(x, 1) <- "a",
  pattern = "`p` must be a character vector or list"
)
expect_error(
  strfind(x, 1, rt = "dict") <- "a",
  pattern = "`p` must be a character vector or list"
)
expect_error(
  strfind(x, 1, rt = "first") <- "a",
  pattern = "`p` must be a character vector or list"
)
expect_error(
  strfind(x, 1, rt = "last") <- "a",
  pattern = "`p` must be a character vector or list"
)

