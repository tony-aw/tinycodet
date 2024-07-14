
# set-up ====
tol <- sqrt(.Machine$double.eps)
eps <- tol * 2
d <- expand.grid(
  x = c(1, NA, NaN), lower = c(0, NA, NaN), upper = c(2, NA, NaN)
)
NA_expectations <- ifelse(is.na(d$x)|is.na(d$lower)|is.na(d$upper), NA, TRUE)


# decimal number boundaries (bnd = matrix) ====
x <- rep(c(0.3, -0.6, 0.7), 3)
lower <- x - c(rep(eps, 3), rep(- eps, 3), rep(eps, 3))
upper <- x + eps
bnd <- cbind(lower, upper)
iswithin <- c(rep(TRUE, 3), rep(FALSE, 3), rep(TRUE, 3))
expect_equal(x %d{}% bnd, iswithin)
expect_equal(x %d!{}% bnd, !iswithin)
expect_equal(
  d$x %d{}% as.matrix(d[, 2:3]),
  NA_expectations
)

expect_error(
  x %d{}% 1:3,
  pattern = "`bnd` must be a matrix with 2 columns or vector with 2 elements",
  fixed = TRUE
)
expect_error(
  x %d!{}% 1:3,
  pattern = "`bnd` must be a matrix with 2 columns or vector with 2 elements",
  fixed = TRUE
)

expect_error(
  x %d{}% bnd[, c(2,1)],
  pattern = "`bnd[, 2] < bnd[, 1]`", fixed = TRUE
)
expect_error(
  x %d!{}% bnd[, c(2,1)],
  pattern = "`bnd[, 2] < bnd[, 1]`", fixed = TRUE
)

expect_warning(
  x %d{}% bnd[1:4,],
  pattern = "longer object length is not a multiple of shorter object length"
)
expect_warning(
  x %d!{}% bnd[1:4,],
  pattern = "longer object length is not a multiple of shorter object length"
)


# decimal number boundaries (bnd = vector) ====
expect_equal(0.3 %d{}% c(0.29, 0.31), TRUE)
expect_equal(0.3 %d!{}% c(0.29, 0.31), FALSE)
expect_equal(-0.3 %d{}% c(0.29, 0.31), FALSE)
expect_equal(-0.3 %d!{}% c(0.29, 0.31), TRUE)
out <-logical(nrow(d))
for(i in 1:nrow(d)) {
  out[i] <- d$x[i] %d{}% as.numeric(d[i, 2:3])
}
expect_equal(
  out,
  NA_expectations
)


# dimension checks ====
x <- rep(c(0.3, -0.6, 0.7), 3) |> matrix(ncol = 3)
lower <- as.vector(x) - c(rep(eps, 3), rep(- eps, 3), rep(eps, 3))
upper <- as.vector(x) + eps
bnd <- cbind(lower, upper)
iswithin <- c(rep(TRUE, 3), rep(FALSE, 3), rep(TRUE, 3))
expect_equal(x %d{}% bnd, matrix(iswithin, ncol = 3))
expect_equal(x %d!{}% bnd, matrix(!iswithin, ncol = 3))
expect_equal(
  matrix(d$x, ncol = 3) %d{}% as.matrix(d[, 2:3]),
  matrix(NA_expectations, ncol = 3)
)

expect_error(
  x %d{}% bnd[, c(2,1)],
  pattern = "`bnd[, 2] < bnd[, 1]`", fixed = TRUE
)
expect_error(
  x %d!{}% bnd[, c(2,1)],
  pattern = "`bnd[, 2] < bnd[, 1]`", fixed = TRUE
)



# Infinity checks ====
expect_equal(
  Inf %d{}% c(1, Inf),
  NA
)
expect_equal(
  Inf %d!{}% c(1, Inf),
  NA
)

expect_equal(
  -Inf %d{}% c(-Inf, 1),
  NA
)
expect_equal(
  -Inf %d!{}% c(-Inf, 1),
  NA
)

