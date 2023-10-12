# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops



# regex ====
x.regex <- c('stringi R', 'R STRINGI', '123')

expect_equal(
  x.regex %s{}% 'R.',stringi::stri_detect_regex(x.regex, 'R.')
)
expect_equal(
  x.regex %s!{}% 'R.',!stringi::stri_detect_regex(x.regex, 'R.')
)

expect_equal(
  x.regex %s{}% '[[:alpha:]]*?',stringi::stri_detect_regex(x.regex, '[[:alpha:]]*?')
)
expect_equal(
  x.regex %s!{}% '[[:alpha:]]*?',!stringi::stri_detect_regex(x.regex, '[[:alpha:]]*?')
)

expect_equal(
  x.regex %s{}% '[a-zC1]',stringi::stri_detect_regex(x.regex, '[a-zC1]')
)
expect_equal(
  x.regex %s!{}% '[a-zC1]',!stringi::stri_detect_regex(x.regex, '[a-zC1]')
)

expect_equal(
  x.regex %s{}% '( R|RE)',stringi::stri_detect_regex(x.regex, '( R|RE)')
)
expect_equal(
  x.regex %s!{}% '( R|RE)',!stringi::stri_detect_regex(x.regex, '( R|RE)')
)

expect_equal(
  x.regex %s{}% s_regex('sTrInG', case_insensitive = TRUE),
  stringi::stri_detect_regex(x.regex, 'sTrInG', case_insensitive = TRUE)
)
expect_equal(
  x.regex %s!{}% s_regex('sTrInG', case_insensitive = TRUE),
  !stringi::stri_detect_regex(x.regex, 'sTrInG', case_insensitive = TRUE)
)

x.regex <- c('abc', 'def', '123', 'ghi', '456', '789', 'jkl')
p.regex <- '^[0-9]+$'

expect_equal(
  x.regex %s{}% s_regex(p.regex, max_count=1),
  stringi::stri_detect_regex(x.regex, p.regex, max_count=1)
)
expect_equal(
  x.regex %s!{}% s_regex(p.regex, max_count=1),
  !stringi::stri_detect_regex(x.regex, p.regex, max_count=1)
)

expect_equal(
  x.regex %s{}% s_regex(p.regex, max_count=2),
  stringi::stri_detect_regex(x.regex, p.regex, max_count=2)
)
expect_equal(
  x.regex %s!{}% s_regex(p.regex, max_count=2),
  !stringi::stri_detect_regex(x.regex, p.regex, max_count=2)
)

expect_equal(
  x.regex %s{}% s_regex(p.regex, max_count=3),
  stringi::stri_detect_regex(x.regex, p.regex, max_count=3)
)
expect_equal(
  x.regex %s!{}% s_regex(p.regex, max_count=3),
  !stringi::stri_detect_regex(x.regex, p.regex, max_count=3)
)


# fixed ====
x.fixed <- c('stringi R', 'R STRINGI', '123')
p.fixed <- c('i', 'R', '0')

expect_equal(
  x.fixed %s{}% s_fixed(p.fixed),
  stringi::stri_detect_fixed(x.fixed, p.fixed)
)
expect_equal(
  x.fixed %s!{}% s_fixed(p.fixed),
  !stringi::stri_detect_fixed(x.fixed, p.fixed)
)
expect_equal(
  x.fixed %s{}% s_fixed('R'),
 stringi::stri_detect_fixed(x.fixed, 'R')
)
expect_equal(
  x.fixed %s!{}% s_fixed('R'),
 !stringi::stri_detect_fixed(x.fixed, 'R')
)


# coll ====
x.list <- list("a", NA, character(0), "ipsum 1234", "")
p.list <- list("a", NA, character(0), "ipsum 1234", "")
expect1 <- expect2 <- list()
out1 <- out2 <- list()
k <- 1
loops <- loops + 1
for(i in 1:length(x.list)) {
  for(j in 1:length(p.list)) {
    expect1[[k]] <- suppressWarnings(stringi::stri_detect_coll(x.list[[i]], p.list[[j]]))
    out1[[k]] <- suppressWarnings(x.list[[i]] %s{}% s_coll(p.list[[j]]))

    expect2[[k]] <- suppressWarnings(!stringi::stri_detect_coll(x.list[[i]], p.list[[j]]))
    out2[[k]] <- suppressWarnings(x.list[[i]] %s!{}% s_coll(p.list[[j]]))

    enumerate <- enumerate + 1
  }
}
expect_equal(expect1, out1)
expect_equal(expect2, out2)


x.coll <- c(
  "", "ala", "ola", "ab", "cab", "ccccab", "aaaabaaaa", "ala", "", "bbb",
  "Lorem\n123", " ", "kota", "4\t\u0105", "aaaab", "bababababaab"
)
p.coll <- c(
  rep("ala", 3), rep("ab", 4), rep("bbb", 3), rep("\t\u0105", 4), "ab", "aab"
)
expect_equal(
 stringi::stri_detect_coll(x.coll, p.coll),
  x.coll %s{}% s_coll(p.coll)
)
expect_equal(
  !stringi::stri_detect_coll(x.coll, p.coll),
  x.coll %s!{}% s_coll(p.coll)
)

expect_equal(
 stringi::stri_detect_coll(character(0), "ipsum 1234"),
  character(0) %s{}% s_coll("ipsum 1234")
)
expect_equal(
  !stringi::stri_detect_coll(character(0), "ipsum 1234"),
  character(0) %s!{}% s_coll("ipsum 1234")
)


# charclass ====
x.charclass <- c('stRRRingi','R STRINGI', '123')
p.charclass <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')

expect_equal(
  x.charclass %s{}% s_chrcls(p.charclass),
 stringi::stri_detect_charclass(x.charclass, p.charclass)
)
expect_equal(
  x.charclass %s!{}% s_chrcls(p.charclass),
 !stringi::stri_detect_charclass(x.charclass, p.charclass)
)


# error checks ====
expect_error(
  x %s{}% -1,
  pattern = "right hand side must be a character vector or list"
)

expect_error(
  x %s!{}% 1,
  pattern = "right hand side must be a character vector or list"
)


