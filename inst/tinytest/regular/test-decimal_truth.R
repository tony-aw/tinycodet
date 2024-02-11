
# basic checks ====
tol <- sqrt(.Machine$double.eps)
eps <- tol * 2
x <- c(
  c(0.3, 0.6, 0.7),
  c(0.3, 0.6, 0.7) + eps, 
  c(0.3, 0.6, 0.7) - eps,
  NA, NaN, NA,
  c(0.3, 0.6, 0.7)
)
y <- c(
  c(0.1*3, 0.1*6, 0.1*7),
  c(0.1*3, 0.1*6, 0.1*7) - eps,
  c(0.1*3, 0.1*6, 0.1*7) + eps,
  c(0.1*3, 0.1*6, 0.1*7),
  NA, NaN, NA
  
)
equal <- c(rep(TRUE, 3), rep(FALSE, 6), rep(NA, 6))
smaller <- c(rep(FALSE, 6), rep(TRUE, 3), rep(NA, 6))
bigger <- c(rep(FALSE, 3), rep(TRUE, 3), rep(FALSE, 3), rep(NA, 6))

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


# recycling checks ====
x <- 1:4
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


# decimal number boundaries (bnd = matrix) ====
x <- rep(c(0.3, -0.6, 0.7), 3)
lower <- x - c(rep(eps, 3), rep(- eps, 3), rep(eps, 3))
upper <- x + eps
bnd <- cbind(lower, upper)
iswithin <- c(rep(TRUE, 3), rep(FALSE, 3), rep(TRUE, 3))
expect_equal(x %d{}% bnd, iswithin)
expect_equal(x %d!{}% bnd, !iswithin)

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


# multi-dim decimal number boundaries (bnd = matrix) ====
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


# multi-dim decimal number boundaries (bnd = vector) ====
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




# test is_wholenumber() - vector ====
x <- seq(1.0, 10.0, by = 1.0)
expect_equal(
  is_wholenumber(1:10),
  rep(TRUE, 10)
)
expect_equal(
  is_wholenumber(seq(1L, 10L, by = 1L)),
  rep(TRUE, 10)
)
expect_equal(
  is_wholenumber(c(seq(1, 2, by = 0.1))),
  c(TRUE, rep(FALSE, 9), TRUE)
)
expect_error(
  is_wholenumber(1:10, tol = "a"),
  pattern = "`tol` must be a single, strictly positive number close to 0"
)
expect_error(
  is_wholenumber(letters[1:10]),
  pattern = "non-numeric argument to mathematical function"
)
expect_error(
  is_wholenumber(x, tol = 1:10),
  pattern = "`tol` must be a single, strictly positive number close to 0"
)
expect_error(
  is_wholenumber(x, tol = 1),
  pattern = "`tol` must be a single, strictly positive number close to 0"
)
expect_error(
  is_wholenumber(x, tol = 0),
  pattern = "`tol` must be a single, strictly positive number close to 0"
)
expect_error(
  is_wholenumber(x, tol = -0.1),
  pattern = "`tol` must be a single, strictly positive number close to 0"
)
expect_error(
  is_wholenumber(x, tol = 1),
  pattern = "`tol` must be a single, strictly positive number close to 0"
)


# test is_wholenumber() - matrix ====
x <- matrix(seq(1.0, 10.0, by = 1.0), ncol = 2)
expect_equal(
  is_wholenumber(x),
  rep(TRUE, 10) |> matrix(ncol=2)
)
expect_equal(
  is_wholenumber(as_int(x)),
  rep(TRUE, 10) |> matrix(ncol=2)
)
expect_error(
  is_wholenumber(x, tol = "a"),
  pattern = "`tol` must be a single, strictly positive number close to 0"
)
expect_error(
  is_wholenumber(letters[1:10] |> matrix(ncol=2)),
  pattern = "non-numeric argument to mathematical function"
)
expect_error(
  is_wholenumber(x, tol = 1:10),
  pattern = "`tol` must be a single, strictly positive number close to 0"
)
expect_error(
  is_wholenumber(x, tol = 1),
  pattern = "`tol` must be a single, strictly positive number close to 0"
)
expect_error(
  is_wholenumber(x, tol = 0),
  pattern = "`tol` must be a single, strictly positive number close to 0"
)
expect_error(
  is_wholenumber(x, tol = -0.1),
  pattern = "`tol` must be a single, strictly positive number close to 0"
)
expect_error(
  is_wholenumber(x, tol = 1),
  pattern = "`tol` must be a single, strictly positive number close to 0"
)


