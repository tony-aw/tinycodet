
# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

i <- sample(c(-50:-1, 1:50), 1000, TRUE)
n.matches <- rep.int(50, length(i))

expect_equal(
  tinycodet:::.rcpp_convert_i0(n.matches, i),
  lapply(i, \(x) tinycodet:::.rcpp_convert_i1(50, x)) |> unlist()
)
  
n.matches <- rep.int(0, length(i))
expect_equal(
  tinycodet:::.rcpp_convert_i0(n.matches, i),
  lapply(i, \(x) tinycodet:::.rcpp_convert_i1(0, x)) |> unlist()
)

expected <- cbind(start = NA_integer_, end = NA_integer_)
expect_equal(
  stri_locate_ith("string", i = 1, fixed = "a"),
  expected
)
expect_equal(
  stri_locate_ith("string", i = -1, fixed = "a"),
  expected
)

expect_equal(
  stri_locate_ith(c("string", "string"), i = c(1, -1), fixed = "a"),
  rbind(expected, expected)
)



