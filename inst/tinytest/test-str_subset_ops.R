
x <- c(
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse="")
)
print(x)
ss <- rbind(c(2,3), c(1,0), c(0,1), c(0,0))

# "string subset ops work", {
  expect_equal(x %sget% ss, c("abklm", "n", "m", ""))
  expect_equal(x %strim% ss, c("cdefghij", "opqrstuvwxyz", "abcdefghijkl", x[4]))

