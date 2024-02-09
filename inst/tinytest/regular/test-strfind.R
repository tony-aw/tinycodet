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
  "",
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
  "",
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
  "",
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
  "",
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


# warnings ====
x <- "hello"
p <- "a|e|i|o|u"
expect_warning(strfind(x, p, rt = "dict"))
expect_warning(strfind(x, p, i = "all") <- "a")


# errors ====
x <- "hello"
p <- "a|e|i|o|u"
expect_error(strfind(x, p, i = "bah"))
expect_error(strfind(x, p, rt = "bah") <- "a")
expect_error(strfind(x, p, rt = "bah") <- list(~ a + b))

expect_error(
  strfind(x, 1),
  pattern = "`p` must be a character vector or list"
)
expect_error(
  strfind(x, 1, i = "all"),
  pattern = "`p` must be a character vector or list"
)
expect_error(
  strfind(x, 1, i = -2),
  pattern = "`p` must be a character vector or list"
)
expect_error(
  strfind(x, 1) <- "a",
  pattern = "`p` must be a character vector or list"
)
expect_error(
  strfind(x, 1, rt = "dict") <- "a",
  pattern = "`p` must be a character vector or list"
)
expect_error(
  strfind(x, 1, rt = "first") <- "a",
  pattern = "`p` must be a character vector or list"
)
expect_error(
  strfind(x, 1, rt = "last") <- "a",
  pattern = "`p` must be a character vector or list"
)

