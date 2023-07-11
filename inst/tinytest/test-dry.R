x <- matrix(-10:9, ncol=2)
print(x)

# "transform_if works", {
  expect_equal(x |> transform_if(\(x)x>0, \(x)return(20), \(x)return(100)),
               matrix(c(rep(100,11), rep(20, 9)), ncol=2))


# "subset_if ops work", {
  expect_equal(x %[if]% \(x)x %in% 1:10,
               1:9)
  expect_equal(x %[!if]% \(x)x %in% 1:10,
               -10:0)

# inplace function works
mtcars <- mtcars2 <- datasets::mtcars
y <- 2
mtcars$mpg[mtcars$cyl>6] <- mtcars$mpg[mtcars$cyl>6]^y
mtcars2$mpg[mtcars2$cyl>6] %:=% \(x)x^y
expect_equal(mtcars, mtcars2)
