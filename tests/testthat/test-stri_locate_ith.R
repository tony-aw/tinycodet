
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
out1 <- stri_locate_ith(x, c(-1, 1), regex=c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE, simplify=TRUE)
out2 <- stri_locate_ith(x, c(-1, 1), regex=c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE, simplify=TRUE)
test_that("stri_locate_ith (regex)", {
  expect_equal(substr(x, out1[,1], out1[,2]), c("i", "o"))
  expect_equal(substr(x, out2[,1], out2[,2]), c(NA, "o"))
})

x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
p <- rep("ab", 2)
out <- stri_locate_ith(x, c(1, -1), fixed=p, simplify=TRUE)
test_that("stri_locate_ith (fixed)", {
  expect_equal(substr(x, out[,1], out[,2]), c("ab", NA))
})

x <- c('hladn\u00FD', 'hladny')
out <- stri_locate_ith(x, 1, coll='HLADNY', strength=1, locale='sk_SK', simplify = TRUE)
test_that("stri_locate_ith (coll)", {
  expect_equal(substr(x, out[,1], out[,2]), x)
})

x <- c("hello world", "goodbye world")
out <- stri_locate_ith(x, c(1, -1), boundaries = "word", simplify = TRUE)
test_that("stri_locate_ith (boundaries)", {
  expect_equal(substr(x, out[,1], out[,2]), c("hello", "world"))
})
