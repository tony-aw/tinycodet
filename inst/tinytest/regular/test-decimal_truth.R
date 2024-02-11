
# basic checks ====
tol <- sqrt(.Machine$double.eps)
eps <- tol * 2
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


# decimal numbers vectors ====
x <- c(0.3, 0.6, 0.7)
y <- c(0.1*3, 0.1*6, 0.1*7)
expect_equal(x %d==% y, rep(TRUE, 3))
expect_equal((x+1) %d==% y, rep(FALSE, 3))
expect_equal(x %d!=% y, rep(FALSE, 3))
expect_equal((x+1) %d!=% y, rep(TRUE, 3))
expect_equal((x+1) %d>% y, rep(TRUE, 3))
expect_equal(x %d>% y, rep(FALSE, 3))
expect_equal((x-1) %d<% y, rep(TRUE, 3))
expect_equal(x %d<% y, rep(FALSE, 3))
expect_equal(x %d<=% y, rep(TRUE, 3))
expect_equal((x+1) %d<=% y, rep(FALSE, 3))
expect_equal(x %d>=% y, rep(TRUE, 3))
expect_equal((x-1) %d>=% y, rep(FALSE, 3))
expect_warning(x %d==% 1:5, pattern = "longer object length is not a multiple of shorter object length")
expect_warning(x %d!=% 1:5, pattern = "longer object length is not a multiple of shorter object length")
expect_warning(x %d>% 1:5, pattern = "longer object length is not a multiple of shorter object length")
expect_warning(x %d<% 1:5, pattern = "longer object length is not a multiple of shorter object length")
expect_warning(x %d>=% 1:5, pattern = "longer object length is not a multiple of shorter object length")
expect_warning(x %d<=% 1:5, pattern = "longer object length is not a multiple of shorter object length")



# decimal numbers multi-dimensional ====
x <- seq(0.1, 0.8, by = 0.1) |> matrix(ncol = 2)
y <- 0.1 * (1:8) |> matrix(ncol = 2)
ybad <- 1:6 |> matrix(ncol=3)
expect_equal(x %d==% y, rep(TRUE, 8) |> matrix(ncol = 2))
expect_equal((x+1) %d==% y, rep(FALSE, 8) |> matrix(ncol = 2))
expect_equal(x %d!=% y, rep(FALSE, 8) |> matrix(ncol = 2))
expect_equal((x+1) %d!=% y, rep(TRUE, 8) |> matrix(ncol = 2))
expect_equal((x+1) %d>% y, rep(TRUE, 8) |> matrix(ncol = 2))
expect_equal(x %d>% y, rep(FALSE, 8) |> matrix(ncol = 2))
expect_equal((x-1) %d<% y, rep(TRUE, 8) |> matrix(ncol = 2))
expect_equal(x %d<% y, rep(FALSE, 8) |> matrix(ncol = 2))
expect_equal(x %d<=% y, rep(TRUE, 8) |> matrix(ncol = 2))
expect_equal((x+1) %d<=% y, rep(FALSE, 8) |> matrix(ncol = 2))
expect_equal(x %d>=% y, rep(TRUE, 8) |> matrix(ncol = 2))
expect_equal((x-1) %d>=% y, rep(FALSE, 8) |> matrix(ncol = 2))
expect_error(x %d==% ybad, pattern = "non-conformable arrays")
expect_error(x %d!=% ybad, pattern = "non-conformable arrays")
expect_error(x %d>% ybad, pattern = "non-conformable arrays")
expect_error(x %d<% ybad, pattern = "non-conformable arrays")
expect_error(x %d>=% ybad, pattern = "non-conformable arrays")
expect_error(x %d<=% ybad, pattern = "non-conformable arrays")


# decimal number boundaries (matrix) ====
x <- c(0.3, -0.6, 0.7)
bnd <- matrix(c(x-eps, x+eps), ncol=2)
expect_equal(x %d{}% bnd, rep(TRUE, 3))
expect_equal(x %d!{}% bnd, rep(FALSE, 3))
bnd <- matrix(c(x-2*eps, x - eps), ncol = 2)
expect_equal(x %d{}% bnd, rep(FALSE, 3))
expect_equal(x %d!{}% bnd, rep(TRUE, 3))

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

# decimal number boundaries (vector) ====
expect_equal(0.3 %d{}% c(0.29, 0.31), TRUE)
expect_equal(0.3 %d!{}% c(0.29, 0.31), FALSE)
expect_equal(-0.3 %d{}% c(0.29, 0.31), FALSE)
expect_equal(-0.3 %d!{}% c(0.29, 0.31), TRUE)



# multi-dim decimal number boundaries (matrix) ====
x <- seq(0.1, 0.8, by = 0.1) |> matrix(ncol = 2)
bnd <- matrix(c(x-0.1, x+0.1), ncol=2)
expect_equal(x %d{}% bnd, rep(TRUE, 8) |> matrix(ncol = 2))
expect_equal(x %d!{}% bnd, rep(FALSE, 8) |> matrix(ncol = 2))

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


# multi-dim decimal number boundaries (vector) ====
expect_equal(0.3 %d{}% c(0.29, 0.31), TRUE)
expect_equal(0.3 %d!{}% c(0.29, 0.31), FALSE)
expect_equal(-0.3 %d{}% c(0.29, 0.31), FALSE)
expect_equal(-0.3 %d!{}% c(0.29, 0.31), TRUE)

# errors ====
expect_error(1 %d==% "a")
expect_error(1 %d!=% "a")
expect_error(1 %d<=% "a")
expect_error(1 %d>=% "a")
expect_error(1 %d<% "a")
expect_error(1 %d>% "a")
expect_error(1 %d{}% "a")
expect_error(1 %d!{}% "a")
