# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


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


