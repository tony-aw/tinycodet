x <- c(TRUE, FALSE, TRUE, FALSE, NA, FALSE, TRUE)
y <- c(FALSE, TRUE, TRUE, FALSE, NA, NA, NA)
outcome <- cbind("x %xor% y"=x %xor% y, "x %n&% y" = x %n&% y, "x %?=% y" = x %?=% y)
expected <- cbind(
  "x %xor% y"=c(T,T,F,F,NA,NA,NA),
  "x %n&% y" =c(F,F,F,T,NA,NA,NA),
  "x %?=% y" =c(F,F,F,F,T,F,F)
)

test_that("negating logic works", {
  expect_equal(outcome, expected)
})

test_that("out works (1)", {
  expect_equal(0:3 %out% 1:10, c(T, F,F,F))
})
test_that("out works (2)", {
  expect_equal(1:10 %out% 1:3, c(rep(F, 3), rep(T, 7)))
})

n <- c(0:5, 0:-5, 0.1, -0.1, 0, 1, Inf, -Inf, NA, NaN)
cbind(1:length(n), n)
test_that("numtype zero", {
  expect_equal(c(1e-20, 1) %=numtype% "~0", c(TRUE, FALSE))
})
test_that("numtype binary", {
  expect_equal(which(n %=numtype% "B"), c(1, 2, 7, 15, 16))
})
test_that("numtype prop", {
  expect_equal(which(n %=numtype% "prop"), c(1, 2, 7, 13, 15, 16))
})
test_that("numtype N", {
  expect_equal(which(n %=numtype% "N"), c(1:7, 15:16))
})
test_that("numtype I", {
  expect_equal(which(n %=numtype% "I"), c(1:12, 15:16))
})
test_that("numtype I", {
  expect_equal(which(n %=numtype% "I"), c(1:12, 15:16))
})
test_that("numtype odd", {
  expect_equal(which(n %=numtype% "odd"), c(2, 4, 6, 8, 10, 12, 16))
})
test_that("numtype even", {
  expect_equal(which(n %=numtype% "even"), c(1, 3, 5, 7, 9, 11, 15))
})
test_that("numtype real", {
  expect_equal(which(n %=numtype% "R"), 1:16)
})
test_that("numtype unreal", {
  expect_equal(which(n %=numtype% "unreal"), 17:20)
})


s <- c(" AbcZ123 ", " abc ", " 1.3 ", " !#$%^&*() ", "  ", "  NA  ", "  NaN  ", " Inf ")
cbind(1:length(s), s)
test_that("strtype empty", {
  expect_equal(which(s %=strtype% "empty"), 5)
})
test_that("strtype unreal", {
  expect_equal(which(s %=strtype% "unreal"), 6:8)
})
test_that("strtype numeric", {
  expect_equal(which(s %=strtype% "numeric"), c(3, 8))
})
test_that("strtype special", {
  expect_equal(which(s %=strtype% "special"), 4)
})


