
x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
y <- c("a", "b")
p <- rep("a|e|i|o|u", 2) # same as p <- list(regex = rep("a|e|i|o|u", 2))
n <- c(3, 2)

x %s+% y # = paste0(x,y)
x %s-% p # remove all vowels from x
x %s*% n
x %s/% p # count how often vowels appear in each string of vector x
x %ss% p # split x around vowels, removing the vowels in the process
x %ss% s_regex(p, simplify = NA) # same as above, but in matrix form

test <- c(
paste0("The\u00a0above-mentioned    features are very useful. ",
"Spam, spam, eggs, bacon, and spam. 123 456 789"),
"good morning, good evening, and good night"
)
test %s//% list(type = "character")


x <- c(paste0(letters[1:13], collapse = ""),
       paste0(letters[14:26], collapse = ""))
print(x)
y <- "a"
# pattern that ignores case:
p <- list(regex = rep("A|E|I|O|U", 2), case_insensitive = TRUE)
n <- c(2, 3)

x %s+% y # = paste0(x,y)
x %s-% p # remove all vowels from x
x %s*% n
x %s/% p # count how often vowels appears in each string of vector x.

x <- c(paste(letters, collapse = ", "), paste(LETTERS, collapse = ", "))
print(x)
x %ss% ", "
t(x %ss% s_fixed(", ", simplify = NA))

