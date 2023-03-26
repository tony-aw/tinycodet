
x <- rep(paste0(0:9, collapse=""), 10)
print(x)
out1 <- stri_locate_ith(x, 1:10, regex = "\\d", simplify=TRUE)
out2 <- stri_locate_ith(x, -1:-10, regex = "\\d", simplify=TRUE)
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, rep(1, 10), 10:1, 10:1, rep(1, 10))
colnames(expected) <- colnames(outcome)

test_that("stri_locate_ith (positions)", {
  expect_equal(outcome, expected)
})

x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
p <- rep("A|E|I|O|U",2)
out <- stri_locate_ith(x, c(-1, 1), regex=p, case_insensitive=TRUE, simplify=TRUE)

test_that("stri_locate_ith (ignore case vowels)", {
  expect_equal(substr(x, out[,1], out[,2]), c("i", "o"))
})


x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
p <- rep("ab", 2)
out <- stri_locate_ith(x, c(1, -1), fixed=p, simplify=TRUE)
test_that("stri_locate_ith (multichar, fixed, NA)", {
  expect_equal(substr(x, out[,1], out[,2]), c("ab", NA))
})

x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
p <- rep("AB", 2)
out <- stri_locate_ith(x, c(1, -1), regex=p, simplify=TRUE, case_insensitive=TRUE)
test_that("stri_locate_ith (multichar, ignore case, NA)", {
  expect_equal(substr(x, out[,1], out[,2]), c("ab", NA))
})

