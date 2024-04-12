# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# positions - vector i ====
# regex
x <- rep(paste0(0:9, collapse=""), 10)
print(x)
out1 <- stri_locate_ith(x, 1:10, regex = "\\d")
out2 <- stri_locate_ith(x, -1:-10, regex = "\\d")
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# fixed
x <- rep("aaaaaaaaaa", 10)
print(x)
out1 <- stri_locate_ith(x, 1:10, fixed = "a")
out2 <- stri_locate_ith(x, -1:-10, fixed = "a")
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# coll
x <- rep("\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD", 10)
print(x)
out1 <- stri_locate_ith(x, 1:10, coll='Y', strength=1, locale='sk_SK')
out2 <- stri_locate_ith(x, -1:-10, coll='Y', strength=1, locale='sk_SK')
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# charclass, merge = TRUE (default)
x <- rep("a a a a a a a a a a", 10)
print(x)
out1 <- stri_locate_ith(x, 1:10, charclass = "[a]")
out2 <- stri_locate_ith(x, -1:-10, charclass = "[a]")
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, seq(1, 19, by = 2), seq(1, 19, by = 2), seq(19, 1, by = -2), seq(19, 1, by = -2))
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# charclass, merge = FALSE
x <- rep("aaaaaaaaaa", 10)
print(x)
out1 <- stri_locate_ith(x, 1:10, charclass = "[a]", merge = FALSE)
out2 <- stri_locate_ith(x, -1:-10, charclass = "[a]", merge = FALSE)
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# boundaries
x <- "1 2 3 4 5 6 7 8 9"
n <- nchar(x)
x <- rep(x, nchar(x))
out1 <- stri_locate_ith_boundaries(x, 1:n, type = "")
out2 <- stri_locate_ith_boundaries(x, -1:-n, type = "")
outcome <- cbind(0:16, out1, out2)
expected <- cbind(0:16, 1:n, 1:n, n:1, n:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)




# positions - scalar i ====
locapply <- function(x, i, ...) {
  return(do.call(rbind, lapply(seq_along(i), \(j) stri_locate_ith(x, i[j], ...))))
}
locapply_boundaries <- function(x, i, ...) {
  return(do.call(rbind, lapply(seq_along(i), \(j) stri_locate_ith_boundaries(x, i[j], ...))))
}
# regex
x <- paste0(0:9, collapse = "")
print(x)
out1 <- locapply(x, 1:10, regex = "\\d")
out2 <- locapply(x, -1:-10, regex = "\\d")
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# fixed
x <- "aaaaaaaaaa"
print(x)
out1 <- locapply(x, 1:10, fixed = "a")
out2 <- locapply(x, -1:-10, fixed = "a")
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# coll
x <- "\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD"
print(x)
out1 <- locapply(x, 1:10, coll='Y', strength=1, locale='sk_SK')
out2 <- locapply(x, -1:-10, coll='Y', strength=1, locale='sk_SK')
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# charclass, merge = TRUE (default)
x <- "a a a a a a a a a a"
print(x)
out1 <- locapply(x, 1:10, charclass = "[a]")
out2 <- locapply(x, -1:-10, charclass = "[a]")
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, seq(1, 19, by = 2), seq(1, 19, by = 2), seq(19, 1, by = -2), seq(19, 1, by = -2))
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# charclass, merge = FALSE
x <- "aaaaaaaaaa"
print(x)
out1 <- locapply(x, 1:10, charclass = "[a]", merge = FALSE)
out2 <- locapply(x, -1:-10, charclass = "[a]", merge = FALSE)
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# boundaries
x <- "1 2 3 4 5 6 7 8 9"
n <- nchar(x)
out1 <- locapply_boundaries(x, 1:n, type = "")
out2 <- locapply_boundaries(x, -1:-n, type = "")
outcome <- cbind(0:16, out1, out2)
expected <- cbind(0:16, 1:n, 1:n, n:1, n:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)


# check if options are passed through correctly ====
# regex
x <- "string"
pattern <- "A|E|I|O|U"
opts <- stringi::stri_opts_regex(case_insensitive = TRUE)
expected <- stringi::stri_locate_first_regex(x, pattern)
expect_equal(
  stri_locate_ith(x, 1, regex = pattern),
  expected
)
expect_equal(
  stri_locate_ith_regex(x, pattern, 1),
  expected
)
expected <- stringi::stri_locate_first_regex(x, pattern, opts_regex = opts)
expect_equal(
  stri_locate_ith(x, 1, regex = pattern, case_insensitive = TRUE),
  expected
)
expect_equal(
  stri_locate_ith(x, 1, regex = pattern, opts_regex = opts),
  expected
)
expect_equal(
  stri_locate_ith_regex(x, 1, pattern = pattern, opts_regex = opts),
  expected
)

# fixed
x <- "string"
pattern <- "I"
opts <- stringi::stri_opts_fixed(case_insensitive = TRUE)
expected <- stringi::stri_locate_first_fixed(x, pattern)
expect_equal(
  stri_locate_ith(x, 1, fixed = pattern),
  expected
)
expect_equal(
  stri_locate_ith_fixed(x, pattern, 1),
  expected
)
expected <- stringi::stri_locate_first_fixed(x, pattern, opts_fixed = opts)
expect_equal(
  stri_locate_ith(x, 1, fixed = pattern, case_insensitive = TRUE),
  expected
)
expect_equal(
  stri_locate_ith(x, 1, fixed = pattern, opts_fixed = opts),
  expected
)
expect_equal(
  stri_locate_ith_fixed(x, 1, pattern = pattern, opts_fixed = opts),
  expected
)

# coll
opts <- stringi::stri_coll(strength = 1, locale = 'sk_SK')
x <- 'hladn\u00FD'
pattern <- 'HLADNY'
expected <- stringi::stri_locate_first_coll(x, pattern)
expect_equal(
  stri_locate_ith(x, 1, coll = pattern),
  expected
)
expect_equal(
  stri_locate_ith_coll(x, pattern, 1),
  expected
)
expected <- stringi::stri_locate_first_coll(x, pattern, opts_collator = opts)
expect_equal(
  stri_locate_ith(x, 1, coll = pattern, strength = 1, locale = 'sk_SK'),
  expected
)
expect_equal(
  stri_locate_ith(x, 1, coll = pattern, opts_collator = opts),
  expected
)
expect_equal(
  stri_locate_ith_coll(x, 1, pattern = pattern, opts_collator = opts),
  expected
)

# boundaries
x <- "Mr. Jones and Mrs. Brown are very happy. So am I, Prof. Smith."
type <- "sentence"
opts <- stringi::stri_opts_brkiter(locale='en_US@ss=standard')
expected <- stringi::stri_locate_first_boundaries(x, type = type)
expect_equal(
  stri_locate_ith_boundaries(x, 1, type = type),
  expected
)
expected <- stringi::stri_locate_first_boundaries(x, type = type, opts_brkiter = opts)
expect_equal(
  stri_locate_ith_boundaries(x, 1, type = type, locale='en_US@ss=standard'),
  expected
)
expect_equal(
  stri_locate_ith_boundaries(
    x, 1, type = type,
    opts_brkiter = stringi::stri_opts_brkiter(locale='en_US@ss=standard')
  ),
  expected
)

expect_equal(
  stri_locate_ith_boundaries(
    x, 1,
    opts_brkiter = stringi::stri_opts_brkiter(type = type, locale='en_US@ss=standard')
    ),
  expected
)



# basic equality checks - scalar ====

x <- list(
  NA,
  "ABC",
  "abc",
  "",
  character(0)
)
pattern <- list(
  NA,
  "ab",
  "AB",
  "",
  character(0)
)

loops <- loops + 1
for(iX in 1:length(x)) {
  for(iP in 1:length(pattern)) {
    for (iCI in c(TRUE, FALSE)) {
      cat("iX = ", iX, "; iP = ", iP, "; iCI = ", iCI, "\n")
      
      # Sys.sleep(0.1)
      expect_equal(
        stri_locate_ith(x[[iX]], fixed = pattern[[iP]], i = 1, case_insensitive = iCI),
        stringi::stri_locate_first(x[[iX]], fixed = pattern[[iP]], case_insensitive = iCI)
      ) |> errorfun()
      print("Done: fixed, first")
      
      # Sys.sleep(0.1)
      expect_equal(
        stri_locate_ith(x[[iX]], fixed = pattern[[iP]], i = -1, case_insensitive = iCI),
        stringi::stri_locate_last(x[[iX]], fixed = pattern[[iP]], case_insensitive = iCI)
      ) |> errorfun()
      print("Done: fixed, last")
      
      # Sys.sleep(0.1)
      expect_equal(
        stri_locate_ith(x[[iX]], regex = pattern[[iP]], i = 1, case_insensitive = iCI),
        stringi::stri_locate_first(x[[iX]], regex = pattern[[iP]], case_insensitive = iCI)
      ) |> errorfun()
      print("Done: regex, first")
      
      # Sys.sleep(0.1)
      expect_equal(
        stri_locate_ith(x[[iX]], regex = pattern[[iP]], i = -1, case_insensitive = iCI),
        stringi::stri_locate_last(x[[iX]], regex = pattern[[iP]], case_insensitive = iCI)
      ) |> errorfun()
      print("Done: regex, last")
      
      # Sys.sleep(0.1)
      expect_equal(
        stri_locate_ith(x[[iX]], coll = pattern[[iP]], i = 1),
        stringi::stri_locate_first(x[[iX]], coll = pattern[[iP]])
      ) |> errorfun()
      print("Done: coll, first")
      
      # Sys.sleep(0.1)
      expect_equal(
        stri_locate_ith(x[[iX]], coll = pattern[[iP]], i = -1),
        stringi::stri_locate_last(x[[iX]], coll = pattern[[iP]])
      ) |> errorfun()
      print("Done: coll, last")
      
      # Sys.sleep(0.1)
      expect_equal(
        stri_locate_ith(x[[iX]], charclass = stringi::stri_c("[", pattern[[iP]], "]"), i = 1, merge = FALSE),
        stringi::stri_locate_first(x[[iX]], charclass = stringi::stri_c("[", pattern[[iP]], "]"))
      ) |> errorfun()
      print("Done: charclass, first")
      
      # Sys.sleep(0.1)
      expect_equal(
        stri_locate_ith(x[[iX]], charclass = stringi::stri_c("[", pattern[[iP]], "]"), i = -1, merge = FALSE),
        stringi::stri_locate_last(x[[iX]], charclass = stringi::stri_c("[", pattern[[iP]], "]"))
      ) |> errorfun()
      print("Done: charclass, last")
      
      enumerate <- enumerate + 8
      
    }
  }
}



# basic equality checks - vector ====

x <- list(
  c(NA_character_, NA_character_),
  c("ABC", "abc"),
  c("", "")
)
pattern <- list(
  NA,
  c("ab", "AB"),
  c("", "")
)

loops <- loops + 1
for(iX in 1:length(x)) {
  for(iP in 1:length(pattern)) {
    for (iCI in c(TRUE, FALSE)) {
      cat("iX = ", iX, "; iP = ", iP, "; iCI = ", iCI, "\n")
      
      expected <- rbind(
        stringi::stri_locate_first(x[[iX]][1], fixed = pattern[[iP]][1], case_insensitive = iCI),
        stringi::stri_locate_last(x[[iX]][2], fixed = pattern[[iP]][2], case_insensitive = iCI)
      )
      expect_equal(
        stri_locate_ith(x[[iX]], fixed = pattern[[iP]], i = c(1, -1), case_insensitive = iCI),
        expected
      ) |> errorfun()
      
      
      expected <- rbind(
        stringi::stri_locate_first(x[[iX]][1], regex = pattern[[iP]][1], case_insensitive = iCI),
        stringi::stri_locate_last(x[[iX]][2], regex = pattern[[iP]][2], case_insensitive = iCI)
      )
      expect_equal(
        stri_locate_ith(x[[iX]], regex = pattern[[iP]], i = c(1, -1), case_insensitive = iCI),
        expected
      ) |> errorfun()

      
      expected <- rbind(
        stringi::stri_locate_first(x[[iX]][1], coll = pattern[[iP]][1]),
        stringi::stri_locate_last(x[[iX]][2], coll = pattern[[iP]][2])
      )
      expect_equal(
        stri_locate_ith(x[[iX]], coll = pattern[[iP]], i = c(1, -1)),
        expected
      ) |> errorfun()
      
      
      expected <- rbind(
        stringi::stri_locate_first(x[[iX]][1], charclass = stringi::stri_c("[", pattern[[iP]][1], "]")),
        stringi::stri_locate_last(x[[iX]][2], charclass = stringi::stri_c("[", pattern[[iP]][2], "]"))
      )
      expect_equal(
        stri_locate_ith(x[[iX]], charclass = stringi::stri_c("[", pattern[[iP]], "]"), i = c(1, -1), merge = FALSE),
        expected
      ) |> errorfun()
      
      
      enumerate <- enumerate + 4
      
    }
  }
}


# stri_locate_ith (NAs) ====
repNA <- rep(NA, 3)
x <- repNA
expect_equivalent(stri_locate_ith(x, -2, regex="a|e|i|o|u"), cbind(repNA, repNA))



# bad i ====
x <- c("hello", "goodbye")
expect_error(
  stri_locate_ith(x, 0, regex="a|e|i|o|u"),
  pattern = "`i` is not allowed to be zero or NA"
)
expect_error(
  stri_locate_ith(x, NA, regex="a|e|i|o|u"),
  pattern = "`i` is not allowed to be zero or NA"
)
expect_error(
  stri_locate_ith(x, NaN, regex="a|e|i|o|u"),
  pattern = "`i` is not allowed to be zero or NA"
)
expect_error(
  stri_locate_ith(x, Inf, regex="a|e|i|o|u"),
  pattern = "`i` is not allowed to be zero or NA"
)
expect_error(
  stri_locate_ith(x, -Inf, regex="a|e|i|o|u"),
  pattern = "`i` is not allowed to be zero or NA"
)
i <- c(-1, -1, 1)
expect_error(
  stri_locate_ith(x, i, regex="a|e|i|o|u"),
  pattern = "recycling of vector `i` not allowed"
)



# bad pattern ====
x <- c("hello", "goodbye")
i <- 1
expect_error(
  stri_locate_ith(x, i, whoops="a|e|i|o|u"),
  pattern = "you have to specify either `regex`, `fixed`, `coll`, `charclass`"
)



# empty search ====
expect_warning(
  stri_locate_ith(character(0), 1, regex = "foo"),
  pattern = "empty search patterns are not supported"
)
expect_warning(
  stri_locate_ith("foo", 1, regex = character(0)),
  pattern = "empty search patterns are not supported"
)



# regex, capture groups error ====
x <- 'breakfast=eggs, lunch=pizza, dessert=icecream'
p <- '(\\w+)=(\\w+)'
expect_error(
  stri_locate_ith_regex(x, 1, p, capture_groups = TRUE)
)



# rcpp checks ====
i <- sample(c(-50:-1, 1:50), 5e5, TRUE)
n.matches <- sample(0:50, 5e5, TRUE)
expect_error(.rcpp_convert_i(n.matches, rep(0, length(n.matches))))
expect_error(.rcpp_convert_i(n.matches, rep(NA, length(n.matches))))
expect_error(.rcpp_convert_i(n.matches, rep(NaN, length(n.matches))))
expect_error(.rcpp_convert_i(n.matches, rep(Inf, length(n.matches))))
expect_error(.rcpp_convert_i(n.matches, rep(-Inf, length(n.matches))))



# recycling checks ====
# Note: ALL stri_locate_ith calls eventually go to stri_locate_ith_internal(),
# and that's where the recycling takes place.

x <- rep(paste0(0:9, collapse = ""), 10)
i <- 1:10
expect_equal(
  stri_locate_ith(x, i, regex = "\\d"),
  cbind(start = 1:10, end = 1:10)
)
expect_equal(
  stri_locate_ith(x[1], i, regex = as.character(0:9)),
  cbind(start = 1:10, end = 1:10)
)
expect_equal(
  stri_locate_ith(x, 1L, regex = rep("\\d", 10)),
  cbind(start = rep(1, 10), end = rep(1, 10))
)



# try large vector for stri_locate_ith ====
n <- 1e5
x <- sapply(1:n, \(x)paste0(sample(1:10), collapse = ""))
p <- "\\d"
i <- sample(c(-50:-1, 1:50), replace=TRUE, size = n)
expect_silent(
  stri_locate_ith_regex(x, p, i, case_insensitive = TRUE)
)

