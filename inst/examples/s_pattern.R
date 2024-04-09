

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
p <- rep("a|e|i|o|u", 2) # same as p <- list(regex = rep("a|e|i|o|u", 2))
x %s/% p # count how often vowels appear in each string of vector x.

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
x %s/% list(regex = rep("A|E|I|O|U", 2), case_insensitive = TRUE)
x %s/% s_regex(rep("A|E|I|O|U", 2), case_insensitive = TRUE)


x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
p <- list(fixed = c("A", "A"), case_insensitive = TRUE)
x %s{}% p
x %s!{}% p
p <- s_fixed(c("A", "A"), case_insensitive = TRUE)
x %s{}% p
x %s!{}% p

x <- c(paste0(letters[1:13], collapse = ""),
      paste0(letters[14:26], collapse = ""), NA)
p <- s_fixed("abc", at = "start")
x %s{}% p
stringi::stri_startswith(x, fixed = "abc") # same as above

p <- s_fixed("xyz", at = "end")
x %s{}% p
stringi::stri_endswith(x, fixed = "xyz") # same as above

