
# class ===
temp.fun <- function() {
  import_inops("stringi") |> suppressMessages()
  df <- report_inops()
  return(df)
}
expect_true(is.data.frame(temp.fun()))


# package ====
temp.fun <- function() {
  import_inops("stringi") |> suppressMessages()
  df <- report_inops()
  return(df)
}
expect_true(all(temp.fun()$package == "stringi"))
expect_true(all(temp.fun()$infix_operator %in% pkg_lsf("stringi", "inops")))
expect_true(all(pkg_lsf("stringi", "inops") %in% temp.fun()$infix_operator))
expect_true(nrow(temp.fun()) == length(pkg_lsf("stringi", "inops")))

temp.fun <- function() {
  import_LL("stringi", pkg_lsf("stringi", "inops")) |> suppressMessages()
  df <- report_inops()
  return(df)
}
expect_true(all(temp.fun()$package == "stringi"))
expect_true(all(temp.fun()$infix_operator %in% pkg_lsf("stringi", "inops")))
expect_true(all(pkg_lsf("stringi", "inops") %in% temp.fun()$infix_operator))
expect_true(nrow(temp.fun()) == length(pkg_lsf("stringi", "inops")))


# non-package ====
temp.fun <- function() {
  `%foo%` <- function(e1, e2) paste0(e1, e2)
  return(report_inops())
}
expect_equal(temp.fun()$infix_operator, "%foo%")
expect_true(nrow(temp.fun())==1)
expect_true(is.na(temp.fun()$package))


# NULL ====
temp.fun <- function() {
  import_LL("stringi", pkg_lsf("stringi", "regfuns")) |> suppressMessages()
  df <- report_inops()
  return(df)
}
expect_null(temp.fun())
temp.fun <- function() {
  df <- report_inops()
  return(df)
}
expect_null(temp.fun())


# other environment ===
my_env <- new.env()
expect_null(report_inops(my_env))
my_env[["%s*%"]] <- stringi::`%s*%`
expected <- data.frame()
expect_equal(report_inops(my_env)$infix_operator, "%s*%")

