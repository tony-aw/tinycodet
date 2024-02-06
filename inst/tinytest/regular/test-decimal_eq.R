
# set-up ====
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}
tol <- sqrt(.Machine$double.eps)
eps <- tol * 2
enumerate <- 0
loops <- 0


# vector by vector ====
# make vars
x <- c(
  c(0.3, 0.6, 0.7),
  c(0.3, 0.6, 0.7) + eps, 
  c(0.3, 0.6, 0.7) - eps
)
y <- c(
  c(0.1*3, 0.1*6, 0.1*7),
  c(0.1*3, 0.1*6, 0.1*7) - eps,
  c(0.1*3, 0.1*6, 0.1*7) + eps
)
equal <- c(rep(TRUE, 3), rep(FALSE, 6))
smaller <- c(rep(FALSE, 6), rep(TRUE, 3))
bigger <- c(rep(FALSE, 3), rep(TRUE, 3), rep(FALSE, 3))

# regular checks
expect_equal(x %d==% y, equal)
expect_equal(x %d<=% y, equal | smaller)
expect_equal(x %d>=% y, equal | bigger)
expect_equal(x %d!=% y, !equal)
expect_equal(x %d<% y, !equal & smaller)
expect_equal(x %d>% y, !equal & bigger)

# relational checks
expect_equal(x %d!=% y, !(x %d==% y))
expect_equal(x %d<=% y, !(x %d>% y))
expect_equal(x %d>=% y, !(x %d<% y))


# scalar by scalar ====
loops <- loops + 1
for(i in seq_along(x)) {
  expect_equal(x[i] %d==% y[i], equal[i]) |> errorfun()
  expect_equal(x[i] %d<=% y[i], equal[i] | smaller[i])  |> errorfun()
  expect_equal(x[i] %d>=% y[i], equal[i] | bigger[i])  |> errorfun()
  expect_equal(x[i] %d!=% y[i], !equal[i])  |> errorfun()
  expect_equal(x[i] %d<% y[i], !equal[i] & smaller[i])  |> errorfun()
  expect_equal(x[i] %d>% y[i], !equal[i] & bigger[i])  |> errorfun()
  
  expect_equal(x[i] %d!=% y[i], !(x[i] %d==% y[i]))
  expect_equal(x[i] %d<=% y[i], !(x[i] %d>% y[i]))
  expect_equal(x[i] %d>=% y[i], !(x[i] %d<% y[i]))
  
  enumerate <- enumerate + 9
}

# vector by scalar / scalar by vector ====
# make vars
x <- 0.3
y <- c(0.3 * 1, 0.3 + eps, 0.3 - eps)
equal <- c(TRUE, FALSE, FALSE)
smaller <- c(FALSE, TRUE, FALSE)
bigger <- c(FALSE, FALSE, TRUE)

# regular checks
expect_equal(x %d==% y, equal)
expect_equal(x %d<=% y, equal | smaller)
expect_equal(x %d>=% y, equal | bigger)
expect_equal(x %d!=% y, !equal)
expect_equal(x %d<% y, !equal & smaller)
expect_equal(x %d>% y, !equal & bigger)

# relational checks
expect_equal(x %d!=% y, !(x %d==% y))
expect_equal(x %d<=% y, !(x %d>% y))
expect_equal(x %d>=% y, !(x %d<% y))

# make new vars
y <- 0.7
x <- c(0.7 * 1, 0.7 + eps, 0.7 - eps)
equal <- c(TRUE, FALSE, FALSE)
smaller <- c(FALSE, FALSE, TRUE)
bigger <- c(FALSE, TRUE, FALSE)

# regular checks
expect_equal(x %d==% y, equal)
expect_equal(x %d<=% y, equal | smaller)
expect_equal(x %d>=% y, equal | bigger)
expect_equal(x %d!=% y, !equal)
expect_equal(x %d<% y, !equal & smaller)
expect_equal(x %d>% y, !equal & bigger)

# relational checks
expect_equal(x %d!=% y, !(x %d==% y))
expect_equal(x %d<=% y, !(x %d>% y))
expect_equal(x %d>=% y, !(x %d<% y))


# dimension preservation, but other attributes dropped ====
x <- c(
  c(0.3, 0.6, 0.7),
  c(0.3, 0.6, 0.7) + eps, 
  c(0.3, 0.6, 0.7) - eps
)
y <- c(
  c(0.1*3, 0.1*6, 0.1*7),
  c(0.1*3, 0.1*6, 0.1*7) - eps,
  c(0.1*3, 0.1*6, 0.1*7) + eps
)
x <- matrix(x, ncol = 3)
colnames(x) <- sample(letters, 3)
y <- matrix(y, ncol = 3)
colnames(y) <- sample(letters, 3)
equal <- c(rep(TRUE, 3), rep(FALSE, 6)) |> matrix(ncol = 3)
smaller <- c(rep(FALSE, 6), rep(TRUE, 3)) |> matrix(ncol = 3)
bigger <- c(rep(FALSE, 3), rep(TRUE, 3), rep(FALSE, 3)) |> matrix(ncol = 3)
expect_equal(x %d==% y, equal)
expect_equal(x %d<=% y, equal | smaller)
expect_equal(x %d>=% y, equal | bigger)
expect_equal(x %d!=% y, !equal)
expect_equal(x %d<% y, !equal & smaller)
expect_equal(x %d>% y, !equal & bigger)

x <- c(
  c(0.3, 0.6, 0.7),
  c(0.3, 0.6, 0.7) + eps, 
  c(0.3, 0.6, 0.7) - eps
)
y <- c(
  c(0.1*3, 0.1*6, 0.1*7),
  c(0.1*3, 0.1*6, 0.1*7) - eps,
  c(0.1*3, 0.1*6, 0.1*7) + eps
)
x <- matrix(x, ncol = 3)
colnames(x) <- sample(letters, 3)
y <- matrix(y, ncol = 3)
equal <- c(rep(TRUE, 3), rep(FALSE, 6)) |> matrix(ncol = 3)
smaller <- c(rep(FALSE, 6), rep(TRUE, 3)) |> matrix(ncol = 3)
bigger <- c(rep(FALSE, 3), rep(TRUE, 3), rep(FALSE, 3)) |> matrix(ncol = 3)
expect_equal(x %d==% y, equal)
expect_equal(x %d<=% y, equal | smaller)
expect_equal(x %d>=% y, equal | bigger)
expect_equal(x %d!=% y, !equal)
expect_equal(x %d<% y, !equal & smaller)
expect_equal(x %d>% y, !equal & bigger)


x <- c(
  c(0.3, 0.6, 0.7),
  c(0.3, 0.6, 0.7) + eps, 
  c(0.3, 0.6, 0.7) - eps
)
y <- c(
  c(0.1*3, 0.1*6, 0.1*7),
  c(0.1*3, 0.1*6, 0.1*7) - eps,
  c(0.1*3, 0.1*6, 0.1*7) + eps
)
x <- matrix(x, ncol = 3)
y <- matrix(y, ncol = 3)
colnames(y) <- sample(letters, 3)
equal <- c(rep(TRUE, 3), rep(FALSE, 6)) |> matrix(ncol = 3)
smaller <- c(rep(FALSE, 6), rep(TRUE, 3)) |> matrix(ncol = 3)
bigger <- c(rep(FALSE, 3), rep(TRUE, 3), rep(FALSE, 3)) |> matrix(ncol = 3)
expect_equal(x %d==% y, equal)
expect_equal(x %d<=% y, equal | smaller)
expect_equal(x %d>=% y, equal | bigger)
expect_equal(x %d!=% y, !equal)
expect_equal(x %d<% y, !equal & smaller)
expect_equal(x %d>% y, !equal & bigger)

x <- c(
  c(0.3, 0.6, 0.7),
  c(0.3, 0.6, 0.7) + eps, 
  c(0.3, 0.6, 0.7) - eps
)
y <- c(
  c(0.1*3, 0.1*6, 0.1*7),
  c(0.1*3, 0.1*6, 0.1*7) - eps,
  c(0.1*3, 0.1*6, 0.1*7) + eps
)
x <- matrix(x, ncol = 3)
colnames(x) <- sample(letters, 3)
names(y) <- sample(letters, 9)
equal <- c(rep(TRUE, 3), rep(FALSE, 6))
smaller <- c(rep(FALSE, 6), rep(TRUE, 3))
bigger <- c(rep(FALSE, 3), rep(TRUE, 3), rep(FALSE, 3))
expect_equal(x %d==% y, equal)
expect_equal(x %d<=% y, equal | smaller)
expect_equal(x %d>=% y, equal | bigger)
expect_equal(x %d!=% y, !equal)
expect_equal(x %d<% y, !equal & smaller)
expect_equal(x %d>% y, !equal & bigger)

x <- c(
  c(0.3, 0.6, 0.7),
  c(0.3, 0.6, 0.7) + eps, 
  c(0.3, 0.6, 0.7) - eps
)
y <- c(
  c(0.1*3, 0.1*6, 0.1*7),
  c(0.1*3, 0.1*6, 0.1*7) - eps,
  c(0.1*3, 0.1*6, 0.1*7) + eps
)
y <- matrix(y, ncol = 3)
colnames(y) <- sample(letters, 3)
names(x) <- sample(letters, 9)
equal <- c(rep(TRUE, 3), rep(FALSE, 6))
smaller <- c(rep(FALSE, 6), rep(TRUE, 3))
bigger <- c(rep(FALSE, 3), rep(TRUE, 3), rep(FALSE, 3))
expect_equal(x %d==% y, equal)
expect_equal(x %d<=% y, equal | smaller)
expect_equal(x %d>=% y, equal | bigger)
expect_equal(x %d!=% y, !equal)
expect_equal(x %d<% y, !equal & smaller)
expect_equal(x %d>% y, !equal & bigger)


# internal error avoidance ====
x <- as.integer(1:10)
y <- as.integer(1:10)
expect_silent(
  tinycodet:::.rcpp_ntt_eq_dbl_00(x, y, sqrt(.Machine$double.eps), TRUE)
)
expect_silent(
  tinycodet:::.rcpp_ntt_eq_dbl_01(x, y[1], sqrt(.Machine$double.eps), TRUE)
)
expect_silent(
  tinycodet:::.rcpp_ntt_eq_dbl_10(x[1], y, sqrt(.Machine$double.eps), TRUE)
)

expect_silent(
  tinycodet:::.rcpp_ntt_greater_dbl_00(x, y, sqrt(.Machine$double.eps), TRUE)
)
expect_silent(
  tinycodet:::.rcpp_ntt_greater_dbl_01(x, y[1], sqrt(.Machine$double.eps), TRUE)
)
expect_silent(
  tinycodet:::.rcpp_ntt_greater_dbl_10(x[1], y, sqrt(.Machine$double.eps), TRUE)
)

expect_silent(
  tinycodet:::.rcpp_ntt_smaller_dbl_00(x, y, sqrt(.Machine$double.eps), TRUE)
)
expect_silent(
  tinycodet:::.rcpp_ntt_smaller_dbl_01(x, y[1], sqrt(.Machine$double.eps), TRUE)
)
expect_silent(
  tinycodet:::.rcpp_ntt_smaller_dbl_10(x[1], y, sqrt(.Machine$double.eps), TRUE)
)



# errors ====
x <- c(0.3, 0.6, 0.7)
expect_error(x %d==% 1:5, pattern = "vector recycling not supported")
expect_error(x %d!=% 1:5, pattern = "vector recycling not supported")
expect_error(x %d>% 1:5, pattern = "vector recycling not supported")
expect_error(x %d<% 1:5, pattern = "vector recycling not supported")
expect_error(x %d>=% 1:5, pattern = "vector recycling not supported")
expect_error(x %d<=% 1:5, pattern = "vector recycling not supported")

expect_error(x %d==% "a", pattern = "both sides must be numeric")
expect_error(x %d!=% "a", pattern = "both sides must be numeric")
expect_error(x %d>% "a", pattern = "both sides must be numeric")
expect_error(x %d<% "a", pattern = "both sides must be numeric")
expect_error(x %d>=% "a", pattern = "both sides must be numeric")
expect_error(x %d<=% "a", pattern = "both sides must be numeric")

