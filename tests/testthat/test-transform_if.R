x <- matrix(-10:9, ncol=2)
print(x)

test_that("transform_if works", {
  expect_equal(x |> transform_if(\(x)x>0, \(x)return(20), \(x)return(100)),
               matrix(c(rep(100,11), rep(20, 9)), ncol=2))
})

test_that("subset_if ops work", {
  expect_equal(x %[if]% \(x)x %in% 1:10,
               1:9)
  expect_equal(x %[!if]% \(x)x %in% 1:10,
               -10:0)
})
