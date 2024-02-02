# set-up ====
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

x <- c(
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(letters[1:13], collapse="")
)
x.list <- as.list(x)
print(x)
ss <- rbind(c(2,3), c(1,0), c(0,1), c(0,0), c(50, 0), c(0, 50), c(50, 50))

# sget ====
expect_equal(x %sget% ss, c("abklm", "n", "m", "", x[5], x[6], x[7]))
expected.list <- list("abklm", "n", "m", "", x[5], x[6], x[7])
out.list <- list()
loops <- loops + 1
for(i in 1:7) {
  out.list[[i]] <- x.list[[i]] %sget% ss[i,]
  enumerate <- enumerate + 1
}
expect_equal(expected.list, out.list)

# strim ====
expect_equal(x %strim% ss, c("cdefghij", "opqrstuvwxyz", "abcdefghijkl", x[4], rep("", 3)))
expected.list <- list("cdefghij", "opqrstuvwxyz", "abcdefghijkl", x[4], "", "", "")
loops <- loops + 1
for(i in 1:7) {
  out.list[[i]] <- x.list[[i]] %strim% ss[i,]
  enumerate <- enumerate + 1
}
expect_equal(expected.list, out.list)


# error checking ====
x <- c(
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse=""),
  paste0(letters[1:13], collapse=""),
  paste0(letters[14:26], collapse="")
)
print(x)
ss <- rbind(c(2,3), c(1,0), c(0,1), c(0,0))
expect_error(
  x %sget% as_chr(ss),
  pattern = "right hand side must be an integer vector or matrix"
)
expect_error(
  x %strim% as_chr(ss),
  pattern = "right hand side must be an integer vector or matrix"
)

expect_error(
  x %sget% -ss,
  pattern = "right hand side cannot contain negative numbers"
)
expect_error(
  x %strim% -ss,
  pattern = "right hand side cannot contain negative numbers"
)

expect_error(
  x %sget% ss[1:2,],
  pattern = "right hand side has wrong length or dimensions",
  fixed = TRUE
)
expect_error(
  x %strim% ss[1:2,],
  pattern = "right hand side has wrong length or dimensions",
  fixed = TRUE
)

expect_error(
  x %sget% cbind(ss, ss),
  pattern = "right hand side has wrong length or dimensions",
  fixed = TRUE
)
expect_error(
  x %strim% cbind(ss, ss),
  pattern = "right hand side has wrong length or dimensions",
  fixed = TRUE
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

