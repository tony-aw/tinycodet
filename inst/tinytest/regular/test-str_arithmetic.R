# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops


# regex ====
x.regex <- c('stringi R', 'R STRINGI', '123')
patterns <- c('R.', '[[:alpha:]]*?', '[a-zC1]', '( R|RE)', 'sTrInG')

out_divide <- out_min <- out_ss <- list()
expected_divide <- expected_min <- expected_ss <- list()
for(p in patterns) {
  expected_divide[[p]] <-  x.regex %s/% p
  out_divide[[p]] <- stringi::stri_count_regex(x.regex, p)
  expected_min[[p]] <-  x.regex %s-% p
  out_min[[p]] <- stringi::stri_replace_all_regex(x.regex, p, "")
  expected_ss[[p]] <-  x.regex %ss% p
  out_ss[[p]] <- stringi::stri_split_regex(x.regex, p)
}
expect_equal(out_divide, expected_divide)
expect_equal(out_min, expected_min)
expect_equal(out_ss, expected_ss)


out_divide <- out_min <- out_ss <- list()
expected_divide <- expected_min <- expected_ss <- list()
for(p in patterns) {
  expected_divide[[p]] <-  x.regex %s/% s_regex(p, case_insensitive = TRUE)
  out_divide[[p]] <- stringi::stri_count_regex(x.regex, p, case_insensitive = TRUE)
  expected_min[[p]] <-  x.regex %s-% s_regex(p, case_insensitive = TRUE)
  out_min[[p]] <- stringi::stri_replace_all_regex(x.regex, p, "", case_insensitive = TRUE)
  expected_ss[[p]] <-  x.regex %ss% s_regex(p, case_insensitive = TRUE)
  out_ss[[p]] <- stringi::stri_split_regex(x.regex, p, case_insensitive = TRUE)
  enumerate <- enumerate + 3
}
expect_equal(out_divide, expected_divide)
expect_equal(out_min, expected_min)
expect_equal(out_ss, expected_ss)


x.regex <- c('abc', 'def', '123', 'ghi', '456', '789', 'jkl')
p.regex <- '^[0-9]+$'

expect_equal(
  x.regex %s/% s_regex(p.regex),
  stringi::stri_count_regex(x.regex, p.regex)
)
expect_equal(
  x.regex %s-% s_regex(p.regex),
  stringi::stri_replace_all_regex(x.regex, p.regex, "")
)
expect_equal(
  x.regex %ss% s_regex(p.regex),
  stringi::stri_split_regex(x.regex, p.regex)
)


# fixed ====
x.fixed <- c('stringi R', 'R STRINGI', '123')
p.fixed <- c('i', 'R', '0')

expect_equal(
  x.fixed %s/% s_fixed(p.fixed),
  stringi::stri_count_fixed(x.fixed, p.fixed)
)
expect_equal(
  x.fixed %s-% s_fixed(p.fixed),
  stringi::stri_replace_all_fixed(x.fixed, p.fixed, "")
)
expect_equal(
  x.fixed %ss% s_fixed(p.fixed),
  stringi::stri_split_fixed(x.fixed, p.fixed)
)

expect_equal(
  x.fixed %s/% s_fixed('R'),
  stringi::stri_count_fixed(x.fixed, 'R')
)
expect_equal(
  x.fixed %s-% s_fixed('R'),
  stringi::stri_replace_all_fixed(x.fixed, 'R', "")
)
expect_equal(
  x.fixed %ss% s_fixed('R'),
  stringi::stri_split_fixed(x.fixed, 'R')
)


# coll ====
x.list <- list("a", NA, character(0), "ipsum 1234", "")
p.list <- list("a", NA, character(0), "ipsum 1234", "")
expect1 <- expect2 <- expect3 <- list()
out1 <- out2 <- out3 <- list()
k <- 1
loops <- loops + 1
for(i in 1:length(x.list)) {
  for(j in 1:length(p.list)) {
    expect1[[k]] <- suppressWarnings(stringi::stri_count_coll(x.list[[i]], p.list[[j]]))
    out1[[k]] <- suppressWarnings(x.list[[i]] %s/% s_coll(p.list[[j]]))
    expect2[[k]] <- suppressWarnings(stringi::stri_replace_all_coll(x.list[[i]], p.list[[j]], ""))
    out2[[k]] <- suppressWarnings(x.list[[i]] %s-% s_coll(p.list[[j]]))
    expect3[[k]] <- suppressWarnings(stringi::stri_split_coll(x.list[[i]], p.list[[j]]))
    out3[[k]] <- suppressWarnings(x.list[[i]] %ss% s_coll(p.list[[j]]))
    enumerate <- enumerate + 2

  }
}
expect_equal(expect1, out1)
expect_equal(expect2, out2)
expect_equal(expect3, out3)


x.coll <- c(
  "", "ala", "ola", "ab", "cab", "ccccab", "aaaabaaaa", "ala", "", "bbb",
  "Lorem\n123", " ", "kota", "4\t\u0105", "aaaab", "bababababaab"
)
p.coll <- c(
  rep("ala", 3), rep("ab", 4), rep("bbb", 3), rep("\t\u0105", 4), "ab", "aab"
)
expect_equal(
  stringi::stri_count_coll(x.coll, p.coll),
  x.coll %s/% s_coll(p.coll)
)
expect_equal(
  stringi::stri_replace_all_coll(x.coll, p.coll, ""),
  x.coll %s-% s_coll(p.coll)
)
expect_equal(
  stringi::stri_split_coll(x.coll, p.coll),
  x.coll %ss% s_coll(p.coll)
)

expect_equal(
  stringi::stri_count_coll(character(0), "ipsum 1234"),
  character(0) %s/% s_coll("ipsum 1234")
)
expect_equal(
  stringi::stri_replace_all_coll(character(0), "ipsum 1234", ""),
  character(0) %s-% s_coll("ipsum 1234", "")
)
expect_equal(
  stringi::stri_split_coll(character(0), "ipsum 1234"),
  character(0) %ss% s_coll("ipsum 1234")
)


# charclass ====
x.charclass <- c('stRRRingi','R STRINGI', '123')
p.charclass <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')

expect_equal(
  x.charclass %s/% s_chrcls(p.charclass),
  stringi::stri_count_charclass(x.charclass, p.charclass)
)
expect_equal(
  x.charclass %s-% s_chrcls(p.charclass),
  stringi::stri_replace_all_charclass(x.charclass, p.charclass, "")
)
expect_equal(
  x.charclass %ss% s_chrcls(p.charclass),
  stringi::stri_split_charclass(x.charclass, p.charclass)
)


# arithmetic (both with and without list) ====
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
y <- c("a", "b")
p1 <- rep("a|e|i|o|u", 2)
p2 <- list(regex=rep("A|E|I|O|U", 2), case_insensitive=TRUE)
n <- c(3, 2)

expect_equal(x %s+% y, paste0(x, y))
expect_equal(x %s-% p1, c("bcdfghjklm", "npqrstvwxyz"))
expect_equal(x %s-% p2, c("bcdfghjklm", "npqrstvwxyz"))
expect_equal(x %ss% p1, list(c("", "bcd", "fgh", "jklm"), c("n", "pqrst", "vwxyz")))
expect_equal(x %ss% p2, list(c("", "bcd", "fgh", "jklm"), c("n", "pqrst", "vwxyz")))
expect_equal(x %s*% n, strrep(x, n))
expect_equal(x %s/% p1, c(3,2))
expect_equal(x %s/% p2, c(3,2))


# boundaries ====
test <- c(
  paste0("The\u00a0above-mentioned    features are very useful. ",
         "Spam, spam, eggs, bacon, and spam. 123 456 789"),
  "good morning, good evening, and good night"
)
expect_equal(
  test %s//% list(type = "character"),
  stringi::stri_count_boundaries(test, type = "character")
)
expect_equal(
  test %s//% list(type = "word"),
  stringi::stri_count_boundaries(test, type = "word")
)
expect_equal(
  test %s//% list(type = "sentence"),
  stringi::stri_count_boundaries(test, type = "sentence")
)
expect_equal(
  test %s//% list(type = "line_break"),
  stringi::stri_count_boundaries(test, type = "line_break")
)
expect_error(test %s//% list(type = "chr"))


# error checks ===
expect_error(
  x %s-% 1,
  pattern = "right hand side must be a character vector or list"
)

expect_error(
  x %s/% 1,
  pattern = "right hand side must be a character vector or list"
)

expect_error(
  x %ss% 1,
  pattern = "right hand side must be a character vector or list"
)


