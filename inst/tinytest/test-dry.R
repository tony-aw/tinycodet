x <- c(-10:9, NA, NA)
obj <- matrix(x, ncol=2)
out <- obj
out[is.na(out)] <- -1000
out[12:20] <- log(out[12:20])
out[1:11] <- out[1:11]^2
obj |> transform_if(\(x)x>0, log, \(x)x^2, \(x)-1000)

# "transform_if works", {
expect_equal(
  transform_if(obj, \(x)x>0, log, \(x)x^2, \(x)-1000),
  out
)
expect_equal(
  transform_if(obj, text = "x ; x>0 ; log(x) ; x^2 ; -1000"),
  out
)
expect_equal(
  transform_if(obj, \(x)x>0, log, \(x)x^2, \(x)-1000),
  transform_if(obj, text = "x ; x>0 ; log(x) ; x^2 ; -1000")
)


# transform_if error checks:
expect_error(
  transform_if(obj, \(x)x>0, log, \(x)x^2, ~x),
  pattern = "`cond`, `yes`, `no`, and `other` must all be functions"
)
expect_error(
  transform_if(obj, \(x)x>0, log, "hello", \(x)-1000),
  pattern = "`cond`, `yes`, `no`, and `other` must all be functions"
)
expect_error(
  transform_if(obj, \(x)x>0, 10, \(x)x^2, \(x)-1000),
  pattern = "`cond`, `yes`, `no`, and `other` must all be functions"
)
expect_error(
  transform_if(obj, 1L, 10, \(x)x^2, \(x)-1000),
  pattern = "`cond`, `yes`, `no`, and `other` must all be functions"
)
expect_error(
  transform_if(obj, \(x)x*10, log, \(x)x^2, \(x)-1000),
  pattern = "`cond` does not return a logical vector!"
)
expect_error(
  transform_if(obj, text = "x ; x*10 ; log(x) ; x^2 ; \"hello\""),
  pattern = "`cond` does not return a logical vector!"
)

# transform_if form2fun error checks:
expect_error(
  transform_if(obj, text = "x*10 ; log(x) ; x^2 ; \"hello\""),
  pattern = "Improper string"
)
expect_error(
  transform_if(obj, text =  "~ \"hello\""),
  pattern = "Improper string"
)
expect_error(
  transform_if(obj, text = "x + y; x>0 ; log(x) ; x^2 ; -1000"),
  pattern = "must declare exactly one variable"
)
expect_error(
  transform_if(obj, text = "x ; y ; y ; y ; y"),
  pattern = "expressions in string do not contain declared variable"
)
expect_error(
  transform_if(obj, text = "x ; x>0 ; log(x) ; x^2"),
  pattern = "Improper string"
)


# "subset_if ops work", {
x <- matrix(-10:9, ncol=2)
expect_equal(x %[if]% \(x)x %in% 1:10,
               1:9)
expect_equal(x %[!if]% \(x)x %in% 1:10,
               -10:0)

# subset_if error checks:
expect_error(
  x %[if]% \(x)x*10,
  pattern = "cond must return a logical vector containing only TRUE or FALSE"
)
expect_error(
  x %[!if]% \(x)x*10,
  pattern = "cond must return a logical vector containing only TRUE or FALSE"
)

# inplace function works
mtcars <- mtcars2 <- datasets::mtcars
y <- 2
mtcars$mpg[mtcars$cyl>6] <- mtcars$mpg[mtcars$cyl>6]^y
mtcars2$mpg[mtcars2$cyl>6] %:=% \(x)x^y
expect_equal(mtcars, mtcars2)

# inplace error checks:
mtcars <- mtcars2 <- datasets::mtcars
mtcars <- mtcars2 <- datasets::mtcars
y <- 2
mtcars$mpg[mtcars$cyl>6] <- mtcars$mpg[mtcars$cyl>6]^y
expect_error(
  mtcars2$mpg[mtcars2$cyl>6] %:=% ("hello"),
  pattern = "right hand side must be a function"
)


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

