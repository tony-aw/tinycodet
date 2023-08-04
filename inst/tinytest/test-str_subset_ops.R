
x <- c(
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse="")
)
print(x)
ss <- rbind(c(2,3), c(1,0), c(0,1), c(0,0))

# string subset ops:
expect_equal(x %sget% ss, c("abklm", "n", "m", ""))
expect_equal(x %strim% ss, c("cdefghij", "opqrstuvwxyz", "abcdefghijkl", x[4]))
expect_equal("hello" %ss% c(1,5), c("h", "o"))
expect_equal("hello" %ss% 5:1, c("o", "l", "l", "e", "h"))

# error checking:
x <- c(
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse="")
)
print(x)
ss <- rbind(c(2,3), c(1,0), c(0,1), c(0,0))
expect_error(
  x %sget% -ss,
  pattern = "right hand side cannot contain negative numbers"
)
expect_error(
  x %strim% -ss,
  pattern = "right hand side cannot contain negative numbers"
)
ss[1,1] <- NA
expect_error(
  x %sget% ss,
  pattern = "right hand side cannot contain NA"
)
expect_error(
  x %strim% ss,
  pattern = "right hand side cannot contain NA"
)
expect_error(
  x %ss% 1:5,
  pattern = "left hand side must be a single string"
)
