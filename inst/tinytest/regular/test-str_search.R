# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops



# regex ====
x.regex <- c('stringi R', 'R STRINGI', '123')

expect_equal(
  x.regex %s{}% 'R.',
  stringi::stri_detect_regex(x.regex, 'R.')
)
expect_equal(
  x.regex %s!{}% 'R.',
  !stringi::stri_detect_regex(x.regex, 'R.')
)

expect_equal(
  x.regex %s{}% '[[:alpha:]]*?',
  stringi::stri_detect_regex(x.regex, '[[:alpha:]]*?')
)
expect_equal(
  x.regex %s!{}% '[[:alpha:]]*?',!
    stringi::stri_detect_regex(x.regex, '[[:alpha:]]*?')
)

expect_equal(
  x.regex %s{}% '[a-zC1]',
  stringi::stri_detect_regex(x.regex, '[a-zC1]')
)
expect_equal(
  x.regex %s!{}% '[a-zC1]',
  !stringi::stri_detect_regex(x.regex, '[a-zC1]')
)

expect_equal(
  x.regex %s{}% '( R|RE)',
  stringi::stri_detect_regex(x.regex, '( R|RE)')
)
expect_equal(
  x.regex %s!{}% '( R|RE)',
  !stringi::stri_detect_regex(x.regex, '( R|RE)')
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
  stringi::stri_detect_regex(x.regex, p.regex, max_count=1, negate = TRUE)
)

expect_equal(
  x.regex %s{}% s_regex(p.regex, max_count=2),
  stringi::stri_detect_regex(x.regex, p.regex, max_count=2)
)
expect_equal(
  x.regex %s!{}% s_regex(p.regex, max_count=2),
  stringi::stri_detect_regex(x.regex, p.regex, max_count=2, negate = TRUE)
)

expect_equal(
  x.regex %s{}% s_regex(p.regex, max_count=3),
  stringi::stri_detect_regex(x.regex, p.regex, max_count=3)
)
expect_equal(
  x.regex %s!{}% s_regex(p.regex, max_count=3),
  stringi::stri_detect_regex(x.regex, p.regex, max_count=3, negate = TRUE)
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


# strfind - extract all ====
tempfun <- stringi::stri_extract_all

# regex
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
out1 <- strfind(x, s_regex(c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE))
out2 <- strfind(x, s_regex(c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE))
expect_equal(out1, tempfun(x, regex = c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, regex = c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE))

# fixed
x <- c(
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(LETTERS[1:13], collapse=""),
  paste0(LETTERS[14:26], collapse="")
)
print(x)
out1 <- strfind(x, s_regex(c("ab"), case_insensitive=TRUE))
out2 <- strfind(x, s_regex(c("ab"), case_insensitive=FALSE))
expect_equal(out1, tempfun(x, fixed = c("ab"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, fixed = c("ab"), case_insensitive=FALSE))

# coll
x <- c('hladn\u00FD', 'hladny')
out <- strfind(x, s_coll('HLADNY', strength=1, locale='sk_SK'))
expect_equal(out, tempfun(x, coll = 'HLADNY', strength=1, locale='sk_SK'))

# charclass, merge = TRUE
x <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
out <- strfind(x, s_chrcls(p))
expect_equal(out, tempfun(x, charclass = p))

# charclass, merge = FALSE
x <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
out <- strfind(x, s_chrcls(p), merge = FALSE)
expect_equal(out, tempfun(x, charclass = p, merge = FALSE))





# strfind - locate all ====
tempfun <- stringi::stri_locate_all

# regex
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
out1 <- strfind(x, s_regex(c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE), "all")
out2 <- strfind(x, s_regex(c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE), "all")
expect_equal(out1, tempfun(x, regex = c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, regex = c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE))

# fixed
x <- c(
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(LETTERS[1:13], collapse=""),
  paste0(LETTERS[14:26], collapse="")
)
print(x)
out1 <- strfind(x, s_regex(c("ab"), case_insensitive=TRUE), "all")
out2 <- strfind(x, s_regex(c("ab"), case_insensitive=FALSE), "all")
expect_equal(out1, tempfun(x, fixed = c("ab"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, fixed = c("ab"), case_insensitive=FALSE))

# coll
x <- c('hladn\u00FD', 'hladny')
out <- strfind(x, s_coll('HLADNY', strength=1, locale='sk_SK'), "all")
expect_equal(out, tempfun(x, coll = 'HLADNY', strength=1, locale='sk_SK'))

# charclass, merge = TRUE
x <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
out <- strfind(x, s_chrcls(p), "all")
expect_equal(out, tempfun(x, charclass = p))

# charclass, merge = FALSE
x <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
out <- strfind(x, s_chrcls(p), "all", merge = FALSE)
expect_equal(out, tempfun(x, charclass = p, merge = FALSE))



# strfind - positions ====

# regex
x <- rep(paste0(0:9, collapse=""), 10)
print(x)
p <- s_regex("\\d", case_insensitive = TRUE)
out1 <- strfind(x, p, 1:10)
out2 <- strfind(x, p, -1:-10)
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# fixed
x <- rep("aaaaaaaaaa", 10)
print(x)
p <- s_fixed("A", case_insensitive = TRUE)
out1 <- strfind(x, p, 1:10)
out2 <- strfind(x, p, -1:-10)
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# coll
x <- rep("\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD\u00FD", 10)
print(x)
p <- s_coll('Y', strength = 1, locale = 'sk_SK')
out1 <- strfind(x, p, 1:10)
out2 <- strfind(x, p, -1:-10)
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# charclass, merge = TRUE (default)
x <- rep("a a a a a a a a a a", 10)
print(x)
p <- s_chrcls("[a]")
out1 <- strfind(x, p, 1:10)
out2 <- strfind(x, p, -1:-10)
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, seq(1, 19, by = 2), seq(1, 19, by = 2), seq(19, 1, by = -2), seq(19, 1, by = -2))
colnames(expected) <- c("", "start", "end", "start", "end")
expect_equal(expected, outcome)

# charclass, merge = FALSE
x <- rep("aaaaaaaaaa", 10)
print(x)
p <- s_chrcls("[a]")
out1 <- strfind(x, p, 1:10, merge = FALSE)
out2 <- strfind(x, p, -1:-10, merge = FALSE)
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



# strfind - replace all ====
tempfun <- stringi::stri_replace_all

# regex
x <- out1 <- out2 <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
rp <- "foo"
strfind(out1, s_regex(c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE)) <- rp
strfind(out2, s_regex(c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE)) <- rp
expect_equal(out1, tempfun(x, rp, regex = c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, rp, regex = c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE))

# fixed
x <- out1 <- out2 <- c(
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(LETTERS[1:13], collapse=""),
  paste0(LETTERS[14:26], collapse="")
)
print(x)
strfind(out1, s_regex(c("ab"), case_insensitive=TRUE)) <- rp
strfind(out2, s_regex(c("ab"), case_insensitive=FALSE)) <- rp
expect_equal(out1, tempfun(x, rp, fixed = c("ab"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, rp, fixed = c("ab"), case_insensitive=FALSE))

# coll
x <- out <- c('hladn\u00FD', 'hladny')
strfind(out, s_coll('HLADNY', strength=1, locale='sk_SK')) <- rp
expect_equal(out, tempfun(x, rp, coll = 'HLADNY', strength=1, locale='sk_SK'))

# charclass
x <- out <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
strfind(out, s_chrcls(p)) <- rp
expect_equal(out, tempfun(x, rp, charclass = p))

