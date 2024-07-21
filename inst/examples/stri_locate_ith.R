
#############################################################################

# practical example: transform regex pattern ====

# input character vector:
x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)

# locate ith (second and second-last) vowel locations:
p <- rep("A|E|I|O|U", 2) # vowels
loc <- stri_locate_ith(x, c(2, -2), regex = p, case_insensitive = TRUE)
print(loc)

# extract ith vowels:
extr <- stringi::stri_sub(x, loc)
print(extr)

# transform & replace ith vowels with numbers:
repl <- chartr("aeiou", "12345", extr)
stringi::stri_sub(x, loc) <- repl

# result (notice ith vowels are now numbers):
print(x)

#############################################################################


# practical example: group-capture regex pattern ====

# input character:
# first group: c(breakfast=eggs, breakfast=bacon)
# second group: c(lunch=pizza, lunch=spaghetti)
x <- c('breakfast=eggs;lunch=pizza',
       'breakfast=bacon;lunch=spaghetti',
       'no food here') # no group here
print(x)
       
# locate ith=2nd group:
p <- '(\\w+)=(\\w+)'
loc <- stri_locate_ith(x, i = 2, regex = p)
print(loc)

# extract ith=2nd group:
extr <- stringi::stri_sub(x, loc)
print(extr)

# capture ith=2nd group:
stringi::stri_match(extr, regex = p)

#############################################################################


# practical example: replace words using boundaries ====

# input character vector:
x <- c("good morning and good night",
"hello ladies and gentlemen")
print(x)

# report ith word locations:
loc <- stri_locate_ith_boundaries(x, c(-3, 3), type = "word")
print(loc)

# extract ith words:
extr <- stringi::stri_sub(x, from = loc)
print(extr)

# transform and replace words (notice ith words have inverted case):
tf <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
stringi::stri_sub(x, loc) <- tf

# result:
print(x)


#############################################################################

# find pattern ====

extr <- stringi::stri_sub(x, from = loc)
repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
stringi::stri_sub_replace(x, loc, replacement=repl)


#############################################################################

# simple pattern ====

x <- rep(paste0(1:10, collapse = ""), 10)
print(x)
out <- stri_locate_ith(x, 1:10, regex = as.character(1:10))
cbind(1:10, out)


x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
p <- rep("a|e|i|o|u", 2)
out <- stri_locate_ith(x, c(-1, 1), regex = p)
print(out)
substr(x, out[, 1], out[, 2])


#############################################################################

# ignore case pattern ====


x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
p <- rep("A|E|I|O|U", 2)
out <- stri_locate_ith(x, c(1, -1), regex = p, case_insensitive = TRUE)
substr(x, out[, 1], out[, 2])


#############################################################################

# multi-character pattern ====

x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
# multi-character pattern:
p <- rep("AB", 2)
out <- stri_locate_ith(x, c(1, -1), regex = p, case_insensitive = TRUE)
print(out)
substr(x, out[, 1], out[, 2])



#############################################################################

# Replacement transformation using stringi ====

x <- c("hello world", "goodbye world")
loc <- stri_locate_ith(x, c(1, -1), regex = "a|e|i|o|u")
extr <- stringi::stri_sub(x, from = loc)
repl <- chartr(extr, old = "a-zA-Z", new = "A-Za-z")
stringi::stri_sub_replace(x, loc, replacement = repl)


#############################################################################

# Boundaries ====

test <- c(
  paste0("The\u00a0above-mentioned    features are very useful. ",
         "Spam, spam, eggs, bacon, and spam. 123 456 789"),
  "good morning, good evening, and good night"
)

loc <- stri_locate_ith_boundaries(test, i = c(1, -1), type = "word")
stringi::stri_sub(test, from = loc)


