
# set-up ====
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}
tol <- sqrt(.Machine$double.eps)
eps <- tol * 2
enumerate <- 0
loops <- 0


# vector by vectors ====
x <- rep(c(0.3, -0.6, 0.7), 3)
lower <- x - c(rep(eps, 3), rep(- eps, 3), rep(eps, 3))
upper <- x + eps
bnd <- cbind(lower, upper)
iswithin <- c(rep(TRUE, 3), rep(FALSE, 3), rep(TRUE, 3))
expect_equal(x %d{}% bnd, iswithin)
expect_equal(x %d!{}% bnd, !iswithin)


# scalar by vectors ====
x <- 0.3
lower <- x - c(eps, - eps, eps)
upper <- rep(x + eps, 3)
bnd <- cbind(lower, upper)
iswithin <- c(TRUE, FALSE, TRUE)
expect_equal(x %d{}% bnd, iswithin)
expect_equal(x %d!{}% bnd, !iswithin)


# vector by scalars ====
x <- c(0.3, -0.6, 0.7)
lower <- 3 * 0.1 - eps
upper <- 3 * 0.1 + eps
bnd <- cbind(lower, upper)
iswithin <- c(TRUE, FALSE, FALSE)
expect_equal(x %d{}% bnd, iswithin)
expect_equal(x %d!{}% bnd, !iswithin)


# scalar by scalars ====
x <- rep(c(0.3, -0.6, 0.7), 3)
lower <- x - c(rep(eps, 3), rep(- eps, 3), rep(eps, 3))
upper <- x + eps
bnd <- cbind(lower, upper)
iswithin <- c(rep(TRUE, 3), rep(FALSE, 3), rep(TRUE, 3))
loops <- loops + 1
for(i in seq_along(x)) {
  expect_equal(x[i] %d{}% bnd[i,], iswithin[i]) |> errorfun()
  expect_equal(x[i] %d!{}% bnd[i,], !iswithin[i]) |> errorfun()
  enumerate <- enumerate + 2
}


# dimension preservation, but other attributes dropped ====
x <- rep(c(0.3, -0.6, 0.7), 3)
names(x) <- sample(letters, 9)
lower <- x - c(rep(eps, 3), rep(- eps, 3), rep(eps, 3))
upper <- x + eps
bnd <- cbind(lower, upper)
iswithin <- c(rep(TRUE, 3), rep(FALSE, 3), rep(TRUE, 3))
expect_equal(x %d{}% bnd, iswithin)
expect_equal(x %d!{}% bnd, !iswithin)

x <- rep(c(0.3, -0.6, 0.7), 3) |> matrix(ncol = 3)
colnames(x) <- sample(letters, 3)
lower <- as.vector(x) - c(rep(eps, 3), rep(- eps, 3), rep(eps, 3))
upper <- as.vector(x) + eps
bnd <- cbind(lower, upper)
iswithin <- c(rep(TRUE, 3), rep(FALSE, 3), rep(TRUE, 3)) |> matrix(ncol = 3)
expect_equal(x %d{}% bnd, iswithin)
expect_equal(x %d!{}% bnd, !iswithin)


# boundary tolerance checks ====
# (lower bound higher than upper bound, but within tolerance, so no error)
x <- rnorm(10)
expect_silent(x %d{}% c(eps/2, 0))
expect_silent(x %d{}% cbind(eps/2, 0))
expect_silent(x %d{}% cbind(eps * seq(0.1, 0.9), 0))

x <- x[1]
expect_silent(x %d{}% c(eps/2, 0))
expect_silent(x %d{}% cbind(eps/2, 0))
expect_silent(x %d{}% cbind(eps * seq(0.1, 0.9), 0))


# errors ====
x <- rnorm(10)
lower <- x - abs(rnorm(10))
upper <- x + abs(rnorm(10))
bnd <- cbind(lower, upper)
expect_error(
  x %d{}% cbind(2, 1),
  pattern = "lower bound must not be higher than upper bound", fixed = TRUE
)
expect_error(
  x %d!{}% cbind(2, 1),
  pattern = "lower bound must not be higher than upper bound", fixed = TRUE
)
expect_error(
  x %d{}% cbind(1 + eps, 1),
  pattern = "lower bound must not be higher than upper bound", fixed = TRUE
)
expect_error(
  x %d!{}% cbind(1 + eps, 1),
  pattern = "lower bound must not be higher than upper bound", fixed = TRUE
)
expect_error(
  x %d{}% bnd[1:2,],
  pattern = "vector recycling not supported",
  fixed = TRUE
)
expect_error(
  x %d!{}% bnd[1:2,],
  pattern = "vector recycling not supported",
  fixed = TRUE
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

