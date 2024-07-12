
# set-up ====
tol <- sqrt(.Machine$double.eps)
eps <- tol * 2
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

# basic checks ====

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

expect_equal(x %d==% y, equal)
expect_equal(x %d<=% y, equal | smaller)
expect_equal(x %d>=% y, equal | bigger)
expect_equal(x %d!=% y, !equal)
expect_equal(x %d<% y, !equal & smaller)
expect_equal(x %d>% y, !equal & bigger)


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


# relational checks ====
expect_equal(x %d!=% y, !(x %d==% y))
expect_equal(x %d<=% y, !(x %d>% y))
expect_equal(x %d>=% y, !(x %d<% y))



# consistency checks ====

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


# Infinity by Infinity gives NA checks ====
expect_equal(
  Inf %d==% Inf,
  NA
)
expect_equal(
  Inf %d!=% Inf,
  NA
)
expect_equal(
  Inf %d<=% Inf,
  NA
)
expect_equal(
  Inf %d>=% Inf,
  NA
)
expect_equal(
  Inf %d<% Inf,
  NA
)
expect_equal(
  Inf %d>% Inf,
  NA
)
expect_equal(
  Inf %d{}% c(Inf, Inf),
  NA
)
expect_equal(
  Inf %d!{}% c(Inf, Inf),
  NA
)



# recycling checks ====
x <- 1:4
expect_warning(x %d==% 1:5, pattern = "longer object length is not a multiple of shorter object length")
expect_warning(x %d!=% 1:5, pattern = "longer object length is not a multiple of shorter object length")
expect_warning(x %d>% 1:5, pattern = "longer object length is not a multiple of shorter object length")
expect_warning(x %d<% 1:5, pattern = "longer object length is not a multiple of shorter object length")
expect_warning(x %d>=% 1:5, pattern = "longer object length is not a multiple of shorter object length")
expect_warning(x %d<=% 1:5, pattern = "longer object length is not a multiple of shorter object length")


# dimension checks ====
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


# attribute checks ====
x.data <- list(
  sample(1:20),
  structure(
    sample(1:20),
    dim = c(5, 4),
    dimnames = list(month.abb[1:5], month.abb[1:4]), names = letters[1:20],
    test = "test1"
  ),
  structure(
    sample(1:20),
    dim = c(5, 4),
    dimnames = list(month.name[1:5], month.name[1:4]), names = LETTERS[1:20],
    test = "test2"
  )
)
y.data <- list(
  sample(1:20),
  structure(
    sample(1:20),
    dim = c(5, 4),
    dimnames = list(month.abb[1:5], month.abb[1:4]), names = letters[1:20],
    test = "test1"
  ),
  structure(
    sample(1:20),
    dim = c(5, 4),
    dimnames = list(month.name[1:5], month.name[1:4]), names = LETTERS[1:20],
    test = "test2"
  )
)
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}
loops <- loops + 1
for(i in seq_along(x.data)) {
  for(j in seq_along(y.data)) {
    x <- x.data[[i]]
    y <- x.data[[j]]
    
    expect_equal(
      x == y,
      x %d==% y
    ) |> errorfun()
    expect_equal(
      x != y,
      x %d!=% y
    ) |> errorfun()
    expect_equal(
      x <= y,
      x %d<=% y
    ) |> errorfun()
    expect_equal(
      x >= y,
      x %d>=% y
    ) |> errorfun()
    expect_equal(
      x < y,
      x %d<% y
    ) |> errorfun()
    expect_equal(
      x > y,
      x %d>% y
    ) |> errorfun()
    
    enumerate <- enumerate + 6
  }
}


# errors ====
expect_error(1 %d==% "a")
expect_error(1 %d!=% "a")
expect_error(1 %d<=% "a")
expect_error(1 %d>=% "a")
expect_error(1 %d<% "a")
expect_error(1 %d>% "a")
expect_error(1 %d{}% "a")
expect_error(1 %d!{}% "a")

