
x <- c(0.3, 0.6, 0.7)
y <- c(0.1*3, 0.1*6, 0.1*7)

test_that("float strict equality is TRUE", {
  expect_equal(x %f==% y, rep(TRUE, 3))
})
test_that("float strict equality is FALSE", {
  expect_equal((x+1) %f==% y, rep(FALSE, 3))
})

test_that("float strict inequality is FALSE", {
  expect_equal(x %f!=% y, rep(FALSE, 3))
})
test_that("float strict inequality is TRUE", {
  expect_equal((x+1) %f!=% y, rep(TRUE, 3))
})

test_that("float strict greater is TRUE", {
  expect_equal((x+1) %f>% y, rep(TRUE, 3))
})
test_that("float strict greater is FALSE", {
  expect_equal(x %f>% y, rep(FALSE, 3))
})

test_that("float strict smaller is TRUE", {
  expect_equal((x-1) %f<% y, rep(TRUE, 3))
})
test_that("float strict smaller is FALSE", {
  expect_equal(x %f<% y, rep(FALSE, 3))
})

test_that("float <= is TRUE (x and y equal)", {
  expect_equal(x %f<=% y, rep(TRUE, 3))
})
test_that("float <= is FALSE (x > y)", {
  expect_equal((x+1) %f<=% y, rep(FALSE, 3))
})

test_that("float >= is TRUE (x and y equal)", {
  expect_equal(x %f>=% y, rep(TRUE, 3))
})
test_that("float >= is FALSE (x < y)", {
  expect_equal((x-1) %f>=% y, rep(FALSE, 3))
})


