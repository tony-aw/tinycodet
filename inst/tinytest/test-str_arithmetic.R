
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
y <- c("a", "b")
p1 <- rep("a|e|i|o|u", 2)
p2 <- s_pattern(regex=rep("A|E|I|O|U", 2), case_insensitive=TRUE)
n <- c(3, 2)

# "arithmetic works (both with and without s_pattern)", {
expect_equal(x %s+% y, paste0(x, y))
expect_equal(x %s-% p1, c("bcdfghjklm", "npqrstvwxyz"))
expect_equal(x %s-% p2, c("bcdfghjklm", "npqrstvwxyz"))
expect_equal(x %s*% n, strrep(x, n))
expect_equal(x %s/% p1, c(3,2))
expect_equal(x %s/% p2, c(3,2))

