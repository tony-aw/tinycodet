
# example of %s{}% and %s!{}% ====

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
x %s{}% "a"
x %s!{}% "a"
which(x %s{}% "a")
which(x %s!{}% "a")
x[x %s{}% "a"]
x[x %s!{}% "a"]
x[x %s{}% "a"] <- 1
x[x %s!{}% "a"] <- 1
print(x)

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
x %s{}% "1"
x %s!{}% "1"
which(x %s{}% "1")
which(x %s!{}% "1")
x[x %s{}% "1"]
x[x %s!{}% "1"]
x[x %s{}% "1"] <- "a"
x[x %s!{}% "1"] <- "a"
print(x)

#############################################################################


# Example of %s{}% and %s!{}% with "at" argument ====

x <- c(paste0(letters, collapse = ""),
       paste0(rev(letters), collapse = ""), NA)
p <- s_fixed("abc", at = "start")
x %s{}% p
stringi::stri_startswith(x, fixed = "abc") # same as above

p <- s_fixed("xyz", at = "end")
x %s{}% p
stringi::stri_endswith(x, fixed = "xyz") # same as above

p <- s_fixed("cba", at = "end")
x %s{}% p
stringi::stri_endswith(x, fixed = "cba") # same as above

p <- s_fixed("zyx", at = "start")
x %s{}% p
stringi::stri_startswith(x, fixed = "zyx") # same as above


#############################################################################

# Example of locating with strfind ====

x <- "stringi"
p <- s_fixed("i")
strfind(x, p, i = "all")
# equivalent to:
stringi::stri_locate_all("stringi", fixed = "i")


x <- "hladn\u00FD"
p <- s_coll("HLADNY", strength = 1, locale = "sk_SK")
strfind(x, p, i = "first")
# equivalent to:
stringi::stri_locate_first_coll('hladn\u00FD', 'HLADNY', strength=1, locale='sk_SK')


x <- "hladn\u00FD"
p <- s_coll("HLADNY", strength = 1, locale = "sk_SK")
strfind(x, p, i = "last")
# equivalent to:
stringi::stri_locate_last_coll('hladn\u00FD', 'HLADNY', strength=1, locale='sk_SK')


x <- c('breakfast=eggs;lunch=pizza', 'breakfast=spam', 'no food here')
p <- s_regex(
  '(?<when>\\w+)=(?<what>\\w+)',
  capture_groups = TRUE
)
strfind(x, p, i = "all")
# equivalent to:
stringi::stri_locate_all_regex(
  c('breakfast=eggs;lunch=pizza', 'breakfast=spam', 'no food here'),
  '(?<when>\\w+)=(?<what>\\w+)',
  capture_groups=TRUE
)  # named capture groups


x <- "abababa"
p <- s_fixed("ABA", case_insensitive = TRUE, overlap = TRUE)
strfind(x, p, i = "all")
# equivalent to:
stringi::stri_locate_all_fixed(x, "ABA", case_insensitive=TRUE, overlap=TRUE)


x <- "ababa"
p <- s_fixed("aba")
strfind(x, p, i = "first"); strfind(x, p, i = "last")
# equivalent to:
stringi::stri_locate_first_fixed(x, "aba"); stringi::stri_locate_last_fixed(x, "aba")


x <- "abababa"
p <- "aba"
strfind(x, p, i = "first"); strfind(x, p, i = "last")
# equivalent to:
stringi::stri_locate_first_regex(x, "aba"); stringi::stri_locate_last_regex(x, "aba") 


#############################################################################


# Example of transforming ith occurrence ====

# new character vector:
x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)

# report ith (second and second-last) vowel locations:
p <- s_regex( # vowels
  rep("A|E|I|O|U", 2),
  case_insensitive = TRUE
)
loc <- strfind(x, p, i = c(2, -2))
print(loc)

# extract ith vowels:
extr <- stringi::stri_sub(x, from = loc)
print(extr)

# replace ith vowels with numbers:
repl <- chartr("aeiou", "12345", extr) # transformation
stringi::stri_sub(x, loc) <- repl
print(x)


#############################################################################


# Example of strfind for regular vectorized replacement ====

x <- rep('The quick brown fox jumped over the lazy dog.', 3)
print(x)
p <- c('quick', 'brown', 'fox')
rp <- c('SLOW',  'BLACK', 'BEAR')
x %s{}% p
strfind(x, p)
strfind(x, p) <- rp
print(x)

#############################################################################


# Example of strfind for dictionary replacement ====

x <- rep('The quick brown fox jumped over the lazy dog.', 3)
print(x)
p <- c('quick', 'brown', 'fox')
rp <- c('SLOW',  'BLACK', 'BEAR')
# thus dictionary is:
# quick => SLOW; brown => BLACK; fox => BEAR
strfind(x, p, rt = "dict") <- rp
print(x)


#############################################################################


# Example of strfind for first and last replacement ====

x <- rep('The quick brown fox jumped over the lazy dog.', 3)
print(x)
p <- s_fixed("the", case_insensitive = TRUE)
rp <- "One"
strfind(x, p, rt = "first") <- rp
print(x)

x <- rep('The quick brown fox jumped over the lazy dog.', 3)
print(x)
p <- s_fixed("the", case_insensitive = TRUE)
rp <- "Some Other"
strfind(x, p, rt = "last") <- rp
print(x)




