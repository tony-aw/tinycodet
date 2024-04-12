# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}




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





