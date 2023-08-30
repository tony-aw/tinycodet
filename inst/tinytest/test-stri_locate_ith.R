
x <- rep(paste0(0:9, collapse=""), 10)
print(x)
out1 <- stri_locate_ith(x, 1:10, regex = "\\d")
out2 <- stri_locate_ith(x, -1:-10, regex = "\\d")
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- colnames(outcome)


# "stri_locate_ith (positions)" ====
  expect_equal(outcome, expected)


# "stri_locate_ith (regex)" ====
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
out1 <- stri_locate_ith(x, c(-1, 1), regex=c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE)
out2 <- stri_locate_ith(x, c(-1, 1), regex=c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE)
expect_equal(substr(x, out1[,1], out1[,2]), c("i", "o"))
expect_equal(substr(x, out2[,1], out2[,2]), c(NA, "o"))


x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
p <- rep("ab", 2)
out <- stri_locate_ith(x, c(1, -1), fixed=p)
# "stri_locate_ith (fixed)" ====
  expect_equal(substr(x, out[,1], out[,2]), c("ab", NA))


# "stri_locate_ith (coll)" ====
x <- c('hladn\u00FD', 'hladny')
out <- stri_locate_ith(x, 1, coll='HLADNY', strength=1, locale='sk_SK')
expect_equal(substr(x, out[,1], out[,2]), x)


# "stringi & tidyops matchn regex" ====
x <- lapply(1:10, function(x)paste0(sample(c(letters, LETTERS)), collapse = ""))
p <- "a|e|i|o|u"
tonyfirst <- stri_locate_ith(x, 1, regex=p, case_insensitive=TRUE)
strifirst <- stringi::stri_locate_first(x, regex=p, case_insensitive=TRUE)
tonylast <- stri_locate_ith(x, -1, regex=p, case_insensitive=TRUE)
strilast <- stringi::stri_locate_last(x, regex=p, case_insensitive=TRUE)
expect_equal(tonyfirst, strifirst)
expect_equal(tonylast, strilast)


# "stringi & tidyops matchn fixed" ====
x <- lapply(1:10, function(x)paste0(sample(1:10), collapse = ""))
p <- "1"
tonyfirst <- stri_locate_ith(x, 1, fixed=p, case_insensitive=TRUE)
strifirst <- stringi::stri_locate_first(x, fixed=p, case_insensitive=TRUE)
tonylast <- stri_locate_ith(x, -1, fixed=p, case_insensitive=TRUE)
strilast <- stringi::stri_locate_last(x, fixed=p, case_insensitive=TRUE)
expect_equal(tonyfirst, strifirst)
expect_equal(tonylast, strilast)


# "stri_locate_ith (NAs)" ====
repNA <- rep(NA, 3)
x <- repNA
expect_equivalent(stri_locate_ith(x, -2, regex="a|e|i|o|u"), cbind(repNA, repNA))


# bad i ====
x <- c("hello", "goodbye")
i <- c(0, NA)
expect_error(
  stri_locate_ith(x, i, regex="a|e|i|o|u"),
  pattern = "`i` is not allowed to be zero or NA"
)
i <- c(-1, -1, 1)
expect_error(
  stri_locate_ith(x, i, regex="a|e|i|o|u"),
  pattern = "`i` must be the same length as `str`, or be a length of 1"
)

# bad pattern ====
x <- c("hello", "goodbye")
i <- 1
expect_error(
  stri_locate_ith(x, i, whoops="a|e|i|o|u"),
  pattern = "you have to specify either `regex`, `fixed`, `coll`, `charclass`"
)
