# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# strfind - locate ith positions ====

# regex
x <- rep(paste0(0:9, collapse=""), 10)
print(x)
p <- s_regex("\\d", case_insensitive = TRUE)
out1 <- strfind(x, p, i = 1:10)
out2 <- strfind(x, p, i = -1:-10)
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# fixed
x <- rep("aaaaaaaaaa", 10)
print(x)
p <- s_fixed("A", case_insensitive = TRUE)
out1 <- strfind(x, p, i = 1:10)
out2 <- strfind(x, p, i = -1:-10)
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# coll
x <- rep("\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD", 10)
print(x)
p <- s_coll('Y', strength = 1, locale = 'sk_SK')
out1 <- strfind(x, p, i = 1:10)
out2 <- strfind(x, p, i = -1:-10)
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# charclass, merge = TRUE (default)
x <- rep("a a a a a a a a a a", 10)
print(x)
p <- s_chrcls("[a]")
out1 <- strfind(x, p, i = 1:10)
out2 <- strfind(x, p, i = -1:-10)
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, seq(1, 19, by = 2), seq(1, 19, by = 2), seq(19, 1, by = -2), seq(19, 1, by = -2))
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# charclass, merge = FALSE
x <- rep("aaaaaaaaaa", 10)
print(x)
p <- s_chrcls("[a]")
out1 <- strfind(x, p, i = 1:10, merge = FALSE)
out2 <- strfind(x, p, i = -1:-10, merge = FALSE)
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)



# check if options are passed through correctly ====
# regex
x <- "string"
pattern <- "A|E|I|O|U"
opts <- stringi::stri_opts_regex(case_insensitive = TRUE)
p <- s_regex(pattern, case_insensitive = TRUE)
expected <- stringi::stri_locate_first_regex(x, pattern)
expect_equal(
  strfind(x, pattern, i = 1),
  expected
)
expect_equal(
  strfind(x, s_regex(pattern), i = 1),
  expected
)
expected <- stringi::stri_locate_first_regex(x, pattern, opts_regex = opts)
expect_equal(
  strfind(x, p, i = 1),
  expected
)

# fixed
x <- "string"
pattern <- "I"
opts <- stringi::stri_opts_fixed(case_insensitive = TRUE)
p <- s_fixed(pattern, case_insensitive = TRUE)
expected <- stringi::stri_locate_first_fixed(x, pattern)
expect_equal(
  strfind(x, s_fixed(pattern), i = 1),
  expected
)
expected <- stringi::stri_locate_first_fixed(x, pattern, opts_fixed = opts)
expect_equal(
  strfind(x, p, i = 1),
  expected
)

# coll
opts <- stringi::stri_coll(strength = 1, locale = 'sk_SK')
x <- 'hladn\u00FD'
pattern <- 'HLADNY'
p <- s_coll(pattern, strength = 1, locale = 'sk_SK')
expected <- stringi::stri_locate_first_coll(x, pattern)
expect_equal(
  strfind(x, s_coll(pattern), i = 1),
  expected
)
expected <- stringi::stri_locate_first_coll(x, pattern, opts_collator = opts)
expect_equal(
  strfind(x, p, i = 1),
  expected
)



