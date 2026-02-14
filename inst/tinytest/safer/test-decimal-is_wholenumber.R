
# set-up ====
tol <- sqrt(.Machine$double.eps)
eps <- tol * 2

.as_int <- tinycodet:::.as_int

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
  is_wholenumber(.as_int(x)),
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


# NA, NaN, Inf, -Inf, TRUE, FALSE ====
expect_equal(
  is_wholenumber(-Inf),
  NA
)
expect_equal(
  is_wholenumber(NA),
  NA
)
expect_equal(
  is_wholenumber(NaN),
  NA
)
expect_true(
  is_wholenumber(TRUE)
)
expect_true(
  is_wholenumber(FALSE)
)

