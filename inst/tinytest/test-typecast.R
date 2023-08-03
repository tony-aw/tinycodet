
# test whole object type casting:
x <- rnorm(10) |> matrix(ncol=2)
colnames(x) <- c("one", "two")
attr(x, "test") <- "test"
myattr <- attributes(x)
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


# test partial type casting:
x <- rnorm(10) |> matrix(ncol=2)
colnames(x) <- c("one", "two")
attr(x, "test") <- "test"
myattr <- attributes(x)
out <- expect <- x
out[, 1] <- as_bool(out[,1])
expect[, 1] <- as.logical(out[,1])
expect_equal(out, expect)

out <- expect <- x
out[, 1] <- as_int(out[,1])
expect[, 1] <- as.integer(out[,1])
expect_equal(out, expect)

out <- expect <- x
out[, 1] <- as_dbl(out[,1])
expect[, 1] <- as.double(out[,1])
expect_equal(out, expect)

out <- expect <- x
out[, 1] <- as_chr(out[,1])
expect[, 1] <- as.character(out[,1])
expect_equal(out, expect)


# test chained type casting,
# for all combinations of numeric and numeric-like conversions:
initials <- list(
  sample(c(TRUE, FALSE, size=10, replace=TRUE)),
  1L:10L, rnorm(10), as.character(1:10)
)
as.funs1 <- as.funs2 <- list(as.logical, as.integer, as.double, as.character)
tiny.funs1 <- tiny.funs2 <- list(as_bool, as_int, as_dbl, as_chr)
outs <- list()
expects<- list()
enum <- 1
for (i in 1:4) {
  x <- initials[[i]] |> matrix(ncol=2)
  colnames(x) <- c("one", "two")
  attr(x, "test") <- "test"
  myattr <- attributes(x)
  for (j in 1:4) {
    for (k in 1:4) {
      expects[[enum]] <- as.funs1[[j]](as.funs2[[k]](x))
      attributes(expects[[enum]]) <- myattr
      outs[[enum]] <- tiny.funs1[[j]](tiny.funs2[[k]](x))
      enum <- enum + 1
    }
  }
}
expect_equal(outs, expects)

