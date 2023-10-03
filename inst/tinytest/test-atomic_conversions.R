# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops


# test whole vector type casting ====
x <- seq(-2.5, 2, by =0.5)
attr(x, "test") <- "test"
attr(x, "class") <- "complex"
myattr <- attributes(x)
myattr[["class"]] <- NULL
out <- as.logical(x)
attributes(out) <- myattr
expect_equal(as_bool(x), out)

out <- as.integer(x)
attributes(out) <- myattr
expect_equal(as_int(x), out)

out <- as.double(x)
attributes(out) <- myattr
expect_equal(as_dbl(x), out)

out <- as.character(x)
attributes(out) <- myattr
expect_equal(as_chr(x), out)


# test partial vector type casting ====
x <- seq(-2.5, 2, by =0.5)
attr(x, "test") <- "test"
attr(x, "class") <- "complex"
myattr <- attributes(x)
myattr[["class"]] <- NULL
out <- expect <- x
out[1] <- as_bool(out[1])
expect[1] <- as.logical(out[1])
expect_equal(out, expect)

out <- expect <- x
out[1] <- as_int(out[1])
expect[1] <- as.integer(out[1])
expect_equal(out, expect)

out <- expect <- x
out[1] <- as_dbl(out[1])
expect[1] <- as.double(out[1])
expect_equal(out, expect)

out <- expect <- x
out[1] <- as_chr(out[1])
expect[1] <- as.character(out[1])
expect_equal(out, expect)


# test vector chained type casting for all combos ====
initials <- list(
  sample(c(TRUE, FALSE, size=10, replace=TRUE)),
  1L:10L, seq(-2.5, 2, by =0.5), as.character(1:10)
)
as.funs1 <- as.funs2 <- list(as.logical, as.integer, as.double, as.character)
tiny.funs1 <- tiny.funs2 <- list(as_bool, as_int, as_dbl, as_chr)
outs <- list()
expects<- list()
iter <- 1
loops <- loops + 1
for (i in 1:4) {
  x <- initials[[i]]
  attr(x, "test") <- "test"
  attr(x, "class") <- "complex"
  myattr <- attributes(x)
  myattr[["class"]] <- NULL
  for (j in 1:4) {
    for (k in 1:4) {
      expects[[iter]] <- as.funs1[[j]](as.funs2[[k]](x))
      attributes(expects[[iter]]) <- myattr
      outs[[iter]] <- tiny.funs1[[j]](tiny.funs2[[k]](x))
      iter <- iter + 1
      enumerate <- enumerate + 1
    }
  }
}
expect_equal(outs, expects)


# test whole matrix type casting ====
x <- seq(-2.5, 2, by =0.5) |> matrix(ncol=2)
colnames(x) <- c("one", "two")
attr(x, "test") <- "test"
attr(x, "class") <- "complex"
myattr <- attributes(x)
myattr[["class"]] <- NULL
out <- as.logical(x)
attributes(out) <- myattr
expect_equal(as_bool(x), out)

out <- as.integer(x)
attributes(out) <- myattr
expect_equal(as_int(x), out)

out <- as.double(x)
attributes(out) <- myattr
expect_equal(as_dbl(x), out)

out <- as.character(x)
attributes(out) <- myattr
expect_equal(as_chr(x), out)


# test partial matrix type casting ====
x <- seq(-2.5, 2, by =0.5) |> matrix(ncol=2)
colnames(x) <- c("one", "two")
attr(x, "test") <- "test"
attr(x, "class") <- "complex"
myattr <- attributes(x)
myattr[["class"]] <- NULL
out <- expect <- x
out[, 1] <- as_bool(out[, 1])
expect[, 1] <- as.logical(out[, 1])
expect_equal(out, expect)

out <- expect <- x
out[, 1] <- as_int(out[,1])
expect[, 1] <- as.integer(out[,1])
expect_equal(out, expect)

out <- expect <- x
out[, 1] <- as_dbl(out[, 1])
expect[, 1] <- as.double(out[, 1])
expect_equal(out, expect)

out <- expect <- x
out[, 1] <- as_chr(out[, 1])
expect[, 1] <- as.character(out[, 1])
expect_equal(out, expect)


# test matrix chained type casting for all combos ====
initials <- list(
  sample(c(TRUE, FALSE, size=10, replace=TRUE)),
  1L:10L, seq(-2.5, 2, by =0.5), as.character(1:10)
)
as.funs1 <- as.funs2 <- list(as.logical, as.integer, as.double, as.character)
tiny.funs1 <- tiny.funs2 <- list(as_bool, as_int, as_dbl, as_chr)
outs <- list()
expects<- list()
iter <- 1
loops <- loops + 1
for (i in 1:4) {
  x <- initials[[i]] |> matrix(ncol=2)
  colnames(x) <- c("one", "two")
  attr(x, "test") <- "test"
  attr(x, "class") <- "complex"
  myattr <- attributes(x)
  myattr[["class"]] <- NULL
  for (j in 1:4) {
    for (k in 1:4) {
      expects[[iter]] <- as.funs1[[j]](as.funs2[[k]](x))
      attributes(expects[[iter]]) <- myattr
      outs[[iter]] <- tiny.funs1[[j]](tiny.funs2[[k]](x))
      iter <- iter + 1
      enumerate <- enumerate + 1
    }
  }
}
expect_equal(outs, expects)


# test is_wholenumber() - vector ====
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
  pattern = "`tol` must be numeric"
)
expect_error(
  is_wholenumber(letters[1:10]),
  pattern = "non-numeric argument to mathematical function"
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
expect_equal(
  is_wholenumber(x + c(1, rep(0.1, 9))),
  c(TRUE, rep(FALSE, 9)) |> matrix(ncol=2)
)
expect_error(
  is_wholenumber(x, tol = "a"),
  pattern = "`tol` must be numeric"
)
expect_error(
  is_wholenumber(letters[1:10] |> matrix(ncol=2)),
  pattern = "non-numeric argument to mathematical function"
)

