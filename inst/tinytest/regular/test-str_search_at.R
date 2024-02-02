# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

# fixed & regex ====

x <- list(
  NA,
  "abc",
  "ABC",
  c("ab", "yz", "AB", "YZ"),
  "",
  character(0)
)
pattern <- list(
  NA,
  "abc",
  c("ab", "ab"),
  "AB",
  c("AB", "AB"),
  "",
  character(0)
)

loops <- loops + 1
for(iX in 1:length(x)) {
  for(iP in 1:length(pattern)) {
    for (iCI in c(TRUE, FALSE)) {
      expect_equal(
        x[[iX]] %s{}% s_fixed(pattern[[iP]], at = "start", case_insensitive = iCI),
        stringi::stri_startswith(x[[iX]], fixed = pattern[[iP]], case_insensitive = iCI)
      ) |> errorfun()
      expect_equal(
        x[[iX]] %s!{}% s_fixed(pattern[[iP]], at = "start", case_insensitive = iCI),
        stringi::stri_startswith(x[[iX]], fixed = pattern[[iP]], case_insensitive = iCI, negate = TRUE)
      ) |> errorfun()
      expect_equal(
        x[[iX]] %s{}% s_fixed(pattern[[iP]], at = "end", case_insensitive = iCI),
        stringi::stri_endswith(x[[iX]], fixed = pattern[[iP]], case_insensitive = iCI)
      ) |> errorfun()
      expect_equal(
        x[[iX]] %s!{}% s_fixed(pattern[[iP]], at = "end", case_insensitive = iCI),
        stringi::stri_endswith(x[[iX]], fixed = pattern[[iP]], case_insensitive = iCI, negate = TRUE)
      ) |> errorfun()
      
      expect_equal(
        x[[iX]] %s{}% s_regex(pattern[[iP]], at = "start", case_insensitive = iCI),
        stringi::stri_startswith(x[[iX]], fixed = pattern[[iP]], case_insensitive = iCI)
      ) |> errorfun()
      expect_equal(
        x[[iX]] %s!{}% s_regex(pattern[[iP]], at = "start", case_insensitive = iCI),
        stringi::stri_startswith(x[[iX]], fixed = pattern[[iP]], case_insensitive = iCI, negate = TRUE)
      ) |> errorfun()
      expect_equal(
        x[[iX]] %s{}% s_regex(pattern[[iP]], at = "end", case_insensitive = iCI),
        stringi::stri_endswith(x[[iX]], fixed = pattern[[iP]], case_insensitive = iCI)
      ) |> errorfun()
      expect_equal(
        x[[iX]] %s!{}% s_regex(pattern[[iP]], at = "end", case_insensitive = iCI),
        stringi::stri_endswith(x[[iX]], fixed = pattern[[iP]], case_insensitive = iCI, negate = TRUE)
      ) |> errorfun()
      
      enumerate <- enumerate + 8
      
    }
  }
}


x <- c(paste0(letters, collapse=""), paste0(rev(letters), collapse=""), NA)
p <- s_fixed("abc", at = "start")
expect_equal(
  x %s{}% p,
  stringi::stri_startswith(x, fixed = "abc")
)

p <- s_fixed("xyz", at = "end")
expect_equal(
  x %s{}% p,
  stringi::stri_endswith(x, fixed = "xyz")
)

p <- s_fixed("cba", at = "end")
expect_equal(
  x %s{}% p,
  stringi::stri_endswith(x, fixed = "cba")
)

p <- s_fixed("zyx", at = "start")
expect_equal(
  x %s{}% p,
  stringi::stri_startswith(x, fixed = "zyx")
)



# error checks ====

expect_error(
  x %s{}% s_regex("a", at = -1),
  pattern = "improper `at` argument given"
)

expect_error(
  x %s{}% s_fixed("a", at = -1),
  pattern = "improper `at` argument given"
)

expect_error(
  x %s{}% s_coll("a", at = -1),
  pattern = "improper `at` argument given"
)

expect_error(
  x %s{}% s_chrcls("a", at = -1),
  pattern = "improper `at` argument given"
)


