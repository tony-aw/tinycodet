# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}



# strfind - vectorized replacement (empty rt) ====
tempfun <- stringi::stri_replace_all

# basic
x <- list(
  NA,
  "abc",
  "ABC",
  c("ab", "yz", "AB", "YZ"),
  "",
  character(0)
)
pattern <- list(
  NA,
  "ab",
  c("ab", "ab"),
  "AB",
  c("AB", "AB"),
  character(0)
)

rp <- list(
  NA,
  "foo",
  c("foo1", "foo2"),
  "",
  character(0)
)

loops <- loops + 1
for(iX in 1:length(x)) {
  for(iP in 1:length(pattern)) {
    for(iRp in 1:length(rp)) {
      for (iCI in c(TRUE, FALSE)) {
        out <- x[[iX]]
        strfind(out, s_fixed(as.character(pattern[[iP]]), case_insensitive = iCI)) <- rp[[iRp]]
        expect_equal(
          out,
          stringi::stri_replace_all(x[[iX]], rp[[iRp]], fixed = pattern[[iP]], case_insensitive = iCI)
        ) |> errorfun()
        
        out <- x[[iX]]
        strfind(out, s_regex(as.character(pattern[[iP]]), case_insensitive = iCI)) <- rp[[iRp]]
        expect_equal(
          out,
          stringi::stri_replace_all(x[[iX]], rp[[iRp]], regex = pattern[[iP]], case_insensitive = iCI)
        ) |> errorfun()
        
        enumerate <- enumerate + 2
        
      }
    }
  }
}

# regex
x <- out1 <- out2 <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""), "xyz")
rp <- "foo"
strfind(out1, s_regex(c("A|E|I|O|U", "a|e|i|o|u", "A|E|I|O|U"), case_insensitive=TRUE)) <- rp
strfind(out2, s_regex(c("A|E|I|O|U", "a|e|i|o|u", "A|E|I|O|U"), case_insensitive=FALSE)) <- rp
expect_equal(out1, tempfun(x, rp, regex = c("A|E|I|O|U","a|e|i|o|u", "A|E|I|O|U"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, rp, regex = c("A|E|I|O|U","a|e|i|o|u", "A|E|I|O|U"), case_insensitive=FALSE))

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



# strfind - vectorized replacement (specified rt) ====
tempfun <- stringi::stri_replace_all

# basic
x <- list(
  NA,
  "abc",
  "ABC",
  c("ab", "yz", "AB", "YZ"),
  "",
  character(0)
)
pattern <- list(
  NA,
  "ab",
  c("ab", "ab"),
  "AB",
  c("AB", "AB"),
  character(0)
)

rp <- list(
  NA,
  "foo",
  c("foo1", "foo2"),
  "",
  character(0)
)

loops <- loops + 1
for(iX in 1:length(x)) {
  for(iP in 1:length(pattern)) {
    for(iRp in 1:length(rp)) {
      for (iCI in c(TRUE, FALSE)) {
        out <- x[[iX]]
        strfind(out, s_fixed(as.character(pattern[[iP]]), case_insensitive = iCI), rt = "vec") <- rp[[iRp]]
        expect_equal(
          out,
          stringi::stri_replace_all(x[[iX]], rp[[iRp]], fixed = pattern[[iP]], case_insensitive = iCI)
        ) |> errorfun()
        
        out <- x[[iX]]
        strfind(out, s_regex(as.character(pattern[[iP]]), case_insensitive = iCI), rt = "vec") <- rp[[iRp]]
        expect_equal(
          out,
          stringi::stri_replace_all(x[[iX]], rp[[iRp]], regex = pattern[[iP]], case_insensitive = iCI)
        ) |> errorfun()
        
        enumerate <- enumerate + 2
        
      }
    }
  }
}

# regex
x <- out1 <- out2 <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""), "xyz")
rp <- "foo"
strfind(out1, s_regex(c("A|E|I|O|U", "a|e|i|o|u", "A|E|I|O|U"), case_insensitive=TRUE), rt = "vec") <- rp
strfind(out2, s_regex(c("A|E|I|O|U", "a|e|i|o|u", "A|E|I|O|U"), case_insensitive=FALSE), rt = "vec") <- rp
expect_equal(out1, tempfun(x, rp, regex = c("A|E|I|O|U","a|e|i|o|u", "A|E|I|O|U"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, rp, regex = c("A|E|I|O|U","a|e|i|o|u", "A|E|I|O|U"), case_insensitive=FALSE))

# fixed
x <- out1 <- out2 <- c(
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(LETTERS[1:13], collapse=""),
  paste0(LETTERS[14:26], collapse="")
)
print(x)
strfind(out1, s_regex(c("ab"), case_insensitive=TRUE), rt = "vec") <- rp
strfind(out2, s_regex(c("ab"), case_insensitive=FALSE), rt = "vec") <- rp
expect_equal(out1, tempfun(x, rp, fixed = c("ab"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, rp, fixed = c("ab"), case_insensitive=FALSE))

# coll
x <- out <- c('hladn\u00FD', 'hladny')
strfind(out, s_coll('HLADNY', strength=1, locale='sk_SK'), rt = "vec") <- rp
expect_equal(out, tempfun(x, rp, coll = 'HLADNY', strength=1, locale='sk_SK'))

# charclass
x <- out <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
strfind(out, s_chrcls(p), rt = "vec") <- rp
expect_equal(out, tempfun(x, rp, charclass = p))




# strfind - dictionary replacement ====
tempfun <- function(...) {stringi::stri_replace_all(..., vectorize_all = FALSE)}

# basic
x <- rep('The quick brown fox jumped over the lazy dog.', 3)
p <- c('quick', 'brown', 'fox')
rp <- c('SLOW',  'BLACK', 'BEAR')
strfind(x, p, rt = "dict") <- rp
expect_equal(
  x,
  rep("The SLOW BLACK BEAR jumped over the lazy dog.", 3)
)

# regex
x <- out1 <- out2 <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""), "xyz")
rp <- "foo"
strfind(out1, s_regex(c("A|E|I|O|U", "a|e|i|o|u", "A|E|I|O|U"), case_insensitive=TRUE), rt = "dict") <- rp
strfind(out2, s_regex(c("A|E|I|O|U", "a|e|i|o|u", "A|E|I|O|U"), case_insensitive=FALSE), rt = "dict") <- rp
expect_equal(out1, tempfun(x, rp, regex = c("A|E|I|O|U","a|e|i|o|u", "A|E|I|O|U"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, rp, regex = c("A|E|I|O|U","a|e|i|o|u", "A|E|I|O|U"), case_insensitive=FALSE))

# fixed
x <- out1 <- out2 <- c(
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(LETTERS[1:13], collapse=""),
  paste0(LETTERS[14:26], collapse="")
)
print(x)
strfind(out1, s_regex(c("ab"), case_insensitive=TRUE), rt = "dict") <- rp
strfind(out2, s_regex(c("ab"), case_insensitive=FALSE), rt = "dict") <- rp
expect_equal(out1, tempfun(x, rp, fixed = c("ab"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, rp, fixed = c("ab"), case_insensitive=FALSE))

# coll
x <- out <- c('hladn\u00FD', 'hladny')
strfind(out, s_coll('HLADNY', strength=1, locale='sk_SK'), rt = "dict") <- rp
expect_equal(out, tempfun(x, rp, coll = 'HLADNY', strength=1, locale='sk_SK'))

# charclass
x <- out <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
strfind(out, s_chrcls(p), rt = "dict") <- rp
expect_equal(out, tempfun(x, rp, charclass = p))



# strfind - first replacement ====
tempfun <- stringi::stri_replace_first

# basic
x <- list(
  NA,
  "abc",
  "ABC",
  c("ab", "yz", "AB", "YZ"),
  "",
  character(0)
)
pattern <- list(
  NA,
  "ab",
  c("ab", "ab"),
  "AB",
  c("AB", "AB"),
  character(0)
)

rp <- list(
  NA,
  "foo",
  c("foo1", "foo2"),
  "",
  character(0)
)

loops <- loops + 1
for(iX in 1:length(x)) {
  for(iP in 1:length(pattern)) {
    for(iRp in 1:length(rp)) {
      for (iCI in c(TRUE, FALSE)) {
        out <- x[[iX]]
        strfind(out, s_fixed(as.character(pattern[[iP]]), case_insensitive = iCI), rt = "first") <- rp[[iRp]]
        expect_equal(
          out,
          stringi::stri_replace_all(x[[iX]], rp[[iRp]], fixed = pattern[[iP]], case_insensitive = iCI)
        ) |> errorfun()
        
        out <- x[[iX]]
        strfind(out, s_regex(as.character(pattern[[iP]]), case_insensitive = iCI), rt = "first") <- rp[[iRp]]
        expect_equal(
          out,
          stringi::stri_replace_all(x[[iX]], rp[[iRp]], regex = pattern[[iP]], case_insensitive = iCI)
        ) |> errorfun()
        
        enumerate <- enumerate + 2
        
      }
    }
  }
}

# regex
x <- out1 <- out2 <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""), "xyz")
rp <- "foo"
strfind(out1, s_regex(c("A|E|I|O|U", "a|e|i|o|u", "A|E|I|O|U"), case_insensitive=TRUE), rt = "first") <- rp
strfind(out2, s_regex(c("A|E|I|O|U", "a|e|i|o|u", "A|E|I|O|U"), case_insensitive=FALSE), rt = "first") <- rp
expect_equal(out1, tempfun(x, rp, regex = c("A|E|I|O|U","a|e|i|o|u", "A|E|I|O|U"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, rp, regex = c("A|E|I|O|U","a|e|i|o|u", "A|E|I|O|U"), case_insensitive=FALSE))

# fixed
x <- out1 <- out2 <- c(
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(LETTERS[1:13], collapse=""),
  paste0(LETTERS[14:26], collapse="")
)
print(x)
strfind(out1, s_regex(c("ab"), case_insensitive=TRUE), rt = "first") <- rp
strfind(out2, s_regex(c("ab"), case_insensitive=FALSE), rt = "first") <- rp
expect_equal(out1, tempfun(x, rp, fixed = c("ab"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, rp, fixed = c("ab"), case_insensitive=FALSE))

# coll
x <- out <- c('hladn\u00FD', 'hladny')
strfind(out, s_coll('HLADNY', strength=1, locale='sk_SK'), rt = "first") <- rp
expect_equal(out, tempfun(x, rp, coll = 'HLADNY', strength=1, locale='sk_SK'))

# charclass
x <- out <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
strfind(out, s_chrcls(p), rt = "first") <- rp
expect_equal(out, tempfun(x, rp, charclass = p))


# strfind - last replacement ====
tempfun <- stringi::stri_replace_last

# basic
x <- list(
  NA,
  "abc",
  "ABC",
  c("ab", "yz", "AB", "YZ"),
  "",
  character(0)
)
pattern <- list(
  NA,
  "ab",
  c("ab", "ab"),
  "AB",
  c("AB", "AB"),
  character(0)
)

rp <- list(
  NA,
  "foo",
  c("foo1", "foo2"),
  "",
  character(0)
)

loops <- loops + 1
for(iX in 1:length(x)) {
  for(iP in 1:length(pattern)) {
    for(iRp in 1:length(rp)) {
      for (iCI in c(TRUE, FALSE)) {
        out <- x[[iX]]
        strfind(out, s_fixed(as.character(pattern[[iP]]), case_insensitive = iCI), rt = "last") <- rp[[iRp]]
        expect_equal(
          out,
          stringi::stri_replace_all(x[[iX]], rp[[iRp]], fixed = pattern[[iP]], case_insensitive = iCI)
        ) |> errorfun()
        
        out <- x[[iX]]
        strfind(out, s_regex(as.character(pattern[[iP]]), case_insensitive = iCI), rt = "last") <- rp[[iRp]]
        expect_equal(
          out,
          stringi::stri_replace_all(x[[iX]], rp[[iRp]], regex = pattern[[iP]], case_insensitive = iCI)
        ) |> errorfun()
        
        enumerate <- enumerate + 2
        
      }
    }
  }
}

# regex
x <- out1 <- out2 <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""), "xyz")
rp <- "foo"
strfind(out1, s_regex(c("A|E|I|O|U", "a|e|i|o|u", "A|E|I|O|U"), case_insensitive=TRUE), rt = "last") <- rp
strfind(out2, s_regex(c("A|E|I|O|U", "a|e|i|o|u", "A|E|I|O|U"), case_insensitive=FALSE), rt = "last") <- rp
expect_equal(out1, tempfun(x, rp, regex = c("A|E|I|O|U","a|e|i|o|u", "A|E|I|O|U"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, rp, regex = c("A|E|I|O|U","a|e|i|o|u", "A|E|I|O|U"), case_insensitive=FALSE))

# fixed
x <- out1 <- out2 <- c(
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(LETTERS[1:13], collapse=""),
  paste0(LETTERS[14:26], collapse="")
)
print(x)
strfind(out1, s_regex(c("ab"), case_insensitive=TRUE), rt = "last") <- rp
strfind(out2, s_regex(c("ab"), case_insensitive=FALSE), rt = "last") <- rp
expect_equal(out1, tempfun(x, rp, fixed = c("ab"), case_insensitive=TRUE))
expect_equal(out2, tempfun(x, rp, fixed = c("ab"), case_insensitive=FALSE))

# coll
x <- out <- c('hladn\u00FD', 'hladny')
strfind(out, s_coll('HLADNY', strength=1, locale='sk_SK'), rt = "last") <- rp
expect_equal(out, tempfun(x, rp, coll = 'HLADNY', strength=1, locale='sk_SK'))

# charclass
x <- out <- c('stRRRingi','R STRINGI', '123')
p <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
strfind(out, s_chrcls(p), rt = "last") <- rp
expect_equal(out, tempfun(x, rp, charclass = p))

