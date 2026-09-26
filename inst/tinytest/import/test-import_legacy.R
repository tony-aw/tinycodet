
# import_inops ====
expect_warning(
  import_inops("stringi"),
  pattern = "`import_inops()` is deprecated and will be removed",
  fixed = TRUE
)

tempfun1 <- function(pkg) {
  import_inops(pkg)
  return(ls())
}
tempfun2 <- function(pkg) {
  import_from(pkg, import_ls(pkg, "infix", FALSE, print = FALSE))
  return(ls())
}
expect_equal(
  tempfun1("stringi"),
  tempfun2("stringi")
)


# import_LL ====

expect_warning(
  import_LL("stringi"),
  pattern = "`import_LL()` is deprecated and will be removed",
  fixed = TRUE
)

tempfun1 <- function(pkg, ls) {
  import_from(pkg, ls, re_exports = FALSE)
  return(ls())
}
tempfun2 <- function(pkg, ls) {
  import_LL(pkg, ls)
  return(ls())
}
expect_equal(
  tempfun1("stringi", "stri_c"),
  tempfun2("stringi", "stri_c")
)
expect_equal(
  tempfun1("stringi", c("stri_c", "stri_sub")),
  tempfun2("stringi", c("stri_c", "stri_sub"))
)

