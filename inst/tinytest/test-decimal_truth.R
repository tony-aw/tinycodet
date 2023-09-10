
# decimal numbers basics ====
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
bnd <- matrix(c(x-0.1, x+0.1), ncol=2)
expect_equal(x %d{}% bnd, rep(TRUE, 3))
expect_equal(x %d!{}% bnd, rep(FALSE, 3))
expect_error(
  x %d{}% bnd[, c(2,1)],
  pattern = "`bnd[, 2] < bnd[, 1]`", fixed = TRUE
)
expect_error(
  x %d!{}% bnd[, c(2,1)],
  pattern = "`bnd[, 2] < bnd[, 1]`", fixed = TRUE
)
expect_error(
  x %d{}% bnd[1:2,],
  pattern = "`nrow(bnd)` must be equal to 1 or equal the number of elements of `x`",
  fixed = TRUE
)
expect_error(
  x %d!{}% bnd[1:2,],
  pattern = "`nrow(bnd)` must be equal to 1 or equal the number of elements of `x`",
  fixed = TRUE
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
  x %d{}% bnd[, c(2,1)],
  pattern = "`bnd[, 2] < bnd[, 1]`", fixed = TRUE
)
expect_error(
  x %d!{}% bnd[, c(2,1)],
  pattern = "`bnd[, 2] < bnd[, 1]`", fixed = TRUE
)
expect_error(
  x %d{}% bnd[1:2,],
  pattern = "`nrow(bnd)` must be equal to 1 or equal the number of elements of `x`",
  fixed = TRUE
)
expect_error(
  x %d!{}% bnd[1:2,],
  pattern = "`nrow(bnd)` must be equal to 1 or equal the number of elements of `x`",
  fixed = TRUE
)

# multi-dim decimal number boundaries (vector) ====
expect_equal(0.3 %d{}% c(0.29, 0.31), TRUE)
expect_equal(0.3 %d!{}% c(0.29, 0.31), FALSE)
expect_equal(-0.3 %d{}% c(0.29, 0.31), FALSE)
expect_equal(-0.3 %d!{}% c(0.29, 0.31), TRUE)
