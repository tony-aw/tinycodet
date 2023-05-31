
x <- rep(paste0(0:9, collapse=""), 10)
print(x)
out1 <- stri_locate_ith(x, 1:10, regex = "\\d")
out2 <- stri_locate_ith(x, -1:-10, regex = "\\d")
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- colnames(outcome)

test_that("stri_locate_ith (positions)", {
  expect_equal(outcome, expected)
})

x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
out1 <- stri_locate_ith(x, c(-1, 1), regex=c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE)
out2 <- stri_locate_ith(x, c(-1, 1), regex=c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE)
test_that("stri_locate_ith (regex)", {
  expect_equal(substr(x, out1[,1], out1[,2]), c("i", "o"))
  expect_equal(substr(x, out2[,1], out2[,2]), c(NA, "o"))
})

x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
p <- rep("ab", 2)
out <- stri_locate_ith(x, c(1, -1), fixed=p)
test_that("stri_locate_ith (fixed)", {
  expect_equal(substr(x, out[,1], out[,2]), c("ab", NA))
})

x <- c('hladn\u00FD', 'hladny')
out <- stri_locate_ith(x, 1, coll='HLADNY', strength=1, locale='sk_SK')
test_that("stri_locate_ith (coll)", {
  expect_equal(substr(x, out[,1], out[,2]), x)
})

x <- lapply(1:10, function(x)paste0(sample(c(letters, LETTERS)), collapse = ""))
p <- "a|e|i|o|u"
tonyfirst <- stri_locate_ith(x, 1, regex=p, case_insensitive=TRUE)
strifirst <- stringi::stri_locate_first(x, regex=p, case_insensitive=TRUE)
tonylast <- stri_locate_ith(x, -1, regex=p, case_insensitive=TRUE)
strilast <- stringi::stri_locate_last(x, regex=p, case_insensitive=TRUE)
test_that("stringi & tidyops matchn regex", {
  expect_equal(tonyfirst, strifirst)
  expect_equal(tonylast, strilast)
})

x <- lapply(1:10, function(x)paste0(sample(1:10), collapse = ""))
p <- "1"
tonyfirst <- stri_locate_ith(x, 1, fixed=p, case_insensitive=TRUE)
strifirst <- stringi::stri_locate_first(x, fixed=p, case_insensitive=TRUE)
tonylast <- stri_locate_ith(x, -1, fixed=p, case_insensitive=TRUE)
strilast <- stringi::stri_locate_last(x, fixed=p, case_insensitive=TRUE)
test_that("stringi & tidyops matchn fixed", {
  expect_equal(tonyfirst, strifirst)
  expect_equal(tonylast, strilast)
})
