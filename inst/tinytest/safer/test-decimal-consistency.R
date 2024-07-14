
# set-up ====
tol <- sqrt(.Machine$double.eps)
eps <- tol * 2
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops


# Regular Integer checks ====
d <- sample(1:10)
d <- expand.grid(d, d)
d1 <- d[,1]
d2 <- d[,2]
expect_equal(
  d1 == d2,
  d1 %d==% d2
)
expect_equal(
  d1 != d2,
  d1 %d!=% d2
)
expect_equal(
  d1 <= d2,
  d1 %d<=% d2
)
expect_equal(
  d1 >= d2,
  d1 %d>=% d2
)
expect_equal(
  d1 < d2,
  d1 %d<% d2
)
expect_equal(
  d1 > d2,
  d1 %d>% d2
)
lower <- d2  - 1
upper <- d2 + 1
expect_equal(
  d1 >= lower & d1 <= upper,
  d1 %d{}% cbind(lower, upper)
)
expect_equal(
  d1 < lower | d1 > upper,
  d1 %d!{}% cbind(lower, upper)
)



# NA/NaN/Inf/-Inf/pi checks ====

d <- c(NA, NaN, Inf, -Inf, pi)
d <- expand.grid(d, d)
d1 <- d[,1]
d2 <- d[,2]
all_inf <- which(is.infinite(d1) & is.infinite(d2) & sign(d1) == sign(d2))

expect <- d1 == d2
expect[all_inf] <- NA
out <- d1 %d==% d2
expect_equal(
  expect, out
)

expect <- d1 != d2
expect[all_inf] <- NA
out <- d1 %d!=% d2
expect_equal(
  expect, out
)

expect <- d1 <= d2
expect[all_inf] <- NA
out <- d1 %d<=% d2
expect_equal(
  expect, out
)

expect <- d1 >= d2
expect[all_inf] <- NA
out <- d1 %d>=% d2
expect_equal(
  expect, out
)

expect <- d1 < d2
expect[all_inf] <- NA
out <- d1 %d<% d2
expect_equal(
  expect, out
)

expect <- d1 > d2
expect[all_inf] <- NA
out <- d1 %d>% d2
expect_equal(
  expect, out
)

lower <- d2 - 1
upper <- d2 + 1

expect <- d1 >= lower & d1 <= upper
expect[all_inf] <- NA
out <- d1 %d{}% cbind(lower, upper)
expect_equal(
  expect, out
)

expect <- d1 < lower | d1 > upper
expect[all_inf] <- NA
out <- d1 %d!{}% cbind(lower, upper)
expect_equal(
  expect, out
)



# errors ====
expect_error(1 %d==% "a")
expect_error(1 %d!=% "a")
expect_error(1 %d<=% "a")
expect_error(1 %d>=% "a")
expect_error(1 %d<% "a")
expect_error(1 %d>% "a")
expect_error(1 %d{}% "a")
expect_error(1 %d!{}% "a")

