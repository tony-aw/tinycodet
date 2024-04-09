# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}





# strfind - locate all ====
tempfun <- stringi::stri_locate_all

# regex
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
out1 <- strfind(x, s_regex(c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE), i = "all")
out2 <- strfind(x, s_regex(c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE), i = "all")
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
out1 <- strfind(x, s_regex(c("ab"), case_insensitive=TRUE), i = "all")
out2 <- strfind(x, s_regex(c("ab"), case_insensitive=FALSE), i = "all")
expect_equal(out1, tempfun(x, fixed = c("ab"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, fixed = c("ab"), case_insensitive=FALSE))

# coll
x <- c('hladn\u00FD', 'hladny')
out <- strfind(x, s_coll('HLADNY', strength=1, locale='sk_SK'), i = "all")
expect_equal(out, tempfun(x, coll = 'HLADNY', strength=1, locale='sk_SK'))

# charclass, merge = TRUE
x <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
out <- strfind(x, s_chrcls(p), i = "all")
expect_equal(out, tempfun(x, charclass = p))

# charclass, merge = FALSE
x <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
out <- strfind(x, s_chrcls(p), i = "all", merge = FALSE)
expect_equal(out, tempfun(x, charclass = p, merge = FALSE))






# strfind - locate first ====
tempfun <- stringi::stri_locate_first

# regex
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
out1 <- strfind(x, s_regex(c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE), i = "first")
out2 <- strfind(x, s_regex(c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE), i = "first")
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
out1 <- strfind(x, s_regex(c("ab"), case_insensitive=TRUE), i = "first")
out2 <- strfind(x, s_regex(c("ab"), case_insensitive=FALSE), i = "first")
expect_equal(out1, tempfun(x, fixed = c("ab"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, fixed = c("ab"), case_insensitive=FALSE))

# coll
x <- c('hladn\u00FD', 'hladny')
out <- strfind(x, s_coll('HLADNY', strength=1, locale='sk_SK'), i = "first")
expect_equal(out, tempfun(x, coll = 'HLADNY', strength=1, locale='sk_SK'))

# charclass
x <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
out <- strfind(x, s_chrcls(p), i = "first")
expect_equal(out, tempfun(x, charclass = p))




# strfind - locate last ====
tempfun <- stringi::stri_locate_last

# regex
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
out1 <- strfind(x, s_regex(c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE), i = "last")
out2 <- strfind(x, s_regex(c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE), i = "last")
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
out1 <- strfind(x, s_regex(c("ab"), case_insensitive=TRUE), i = "last")
out2 <- strfind(x, s_regex(c("ab"), case_insensitive=FALSE), i = "last")
expect_equal(out1, tempfun(x, fixed = c("ab"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, fixed = c("ab"), case_insensitive=FALSE))

# coll
x <- c('hladn\u00FD', 'hladny')
out <- strfind(x, s_coll('HLADNY', strength=1, locale='sk_SK'), i = "last")
expect_equal(out, tempfun(x, coll = 'HLADNY', strength=1, locale='sk_SK'))

# charclass
x <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
out <- strfind(x, s_chrcls(p), i = "last")
expect_equal(out, tempfun(x, charclass = p))




# compare stringi locate examples ====

x <- "stringi"
p <- s_fixed("i")
expect_equal(
  strfind(x, p, i = "all"),
  stringi::stri_locate_all("stringi", fixed = "i")
)

x <- "hladn\u00FD"
p <- s_coll("HLADNY", strength = 1, locale = "sk_SK")
expect_equal(
  strfind(x, p, i = "first"),
  stringi::stri_locate_first_coll('hladn\u00FD', 'HLADNY', strength=1, locale='sk_SK')
)

x <- "hladn\u00FD"
p <- s_coll("HLADNY", strength = 1, locale = "sk_SK")
expect_equal(
  strfind(x, p, i = "last"),
  stringi::stri_locate_last_coll('hladn\u00FD', 'HLADNY', strength=1, locale='sk_SK')
)

x <- c('breakfast=eggs;lunch=pizza', 'breakfast=spam', 'no food here')
p <- s_regex(
  '(?<when>\\w+)=(?<what>\\w+)',
  capture_groups = TRUE
)
expect_equal(
  strfind(x, p, i = "all"),
  stringi::stri_locate_all_regex(
    c('breakfast=eggs;lunch=pizza', 'breakfast=spam', 'no food here'),
    '(?<when>\\w+)=(?<what>\\w+)',
    capture_groups=TRUE
  )
)

x <- "abababa"
p <- s_fixed("ABA", case_insensitive = TRUE, overlap = TRUE)
expect_equal(
  strfind(x, p, i = "all"),
  stringi::stri_locate_all_fixed(x, "ABA", case_insensitive=TRUE, overlap=TRUE)
)

x <- "ababa"
p <- s_fixed("aba")
expect_equal(
  strfind(x, p, i = "first"),
  stringi::stri_locate_first_fixed(x, "aba")
)
expect_equal(
  strfind(x, p, i = "last"),
  stringi::stri_locate_last_fixed(x, "aba")
)

x <- "abababa"
p <- "aba"
expect_equal(
  strfind(x, p, i = "first"),
  stringi::stri_locate_first_regex(x, "aba")
)
expect_equal(
  strfind(x, p, i = "last"),
  stringi::stri_locate_last_regex(x, "aba")
)


