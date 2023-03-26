
x <- rep(paste0(letters[1:10], collapse = ""), 3)
start=c(1, 2, 3)
end=c(1, 3, 4)
loc=cbind(start, end)

test_that("substr_extract works", {
  expect_equal(substr_extract(x, start=start, end=end),
               c("a", "bc", "cd"))
  expect_equal(substr_extract(x, "at", start=start, end=end),
               c("a", "bc", "cd"))
  expect_equal(substr_extract(x, "before", start=start, end=end),
               c("", "a", "ab"))
  expect_equal(substr_extract(x, "after", start=start, end=end),
               c(substr(x[1], 2, nchar(x[1])), substr(x[1], 4,nchar(x[1])), substr(x[1], 5, nchar(x[1]))))
  expect_equal(substr_extract(x, loc=loc),
               c("a", "bc", "cd"))
  expect_equal(substr_extract(x, "at", loc=loc),
               c("a", "bc", "cd"))
  expect_equal(substr_extract(x, "before", loc=loc),
               c("", "a", "ab"))
  expect_equal(substr_extract(x, "after", loc=loc),
               c(substr(x[1], 2, nchar(x[1])), substr(x[1], 4,nchar(x[1])), substr(x[1], 5, nchar(x[1]))))
})

test_that("substr_repl works", {
  expect_equal(substr_repl(x, "!!", start=start, end=end),
               c("!!bcdefghij", "a!!defghij", "ab!!efghij"))
  expect_equal(substr_repl(x, "!!", loc=loc),
               c("!!bcdefghij", "a!!defghij", "ab!!efghij"))
  expect_equal(substr_repl(x, c("!!", "??", ";;"), start=start, end=end),
               c("!!bcdefghij", "a??defghij", "ab;;efghij"))
  expect_equal(substr_repl(x, c("!!", "??", ";;"), loc=loc),
               c("!!bcdefghij", "a??defghij", "ab;;efghij"))
})

test_that("substr_chartr works", {
  expect_equal(substr_chartr(x, start=start, end=end),
               c("Abcdefghij", "aBCdefghij", "abCDefghij"))
  expect_equal(substr_chartr(x, loc=loc),
               c("Abcdefghij", "aBCdefghij", "abCDefghij"))
  expect_equal(substr_chartr(toupper(x), start=start, end=end),
               c("aBCDEFGHIJ", "AbcDEFGHIJ", "ABcdEFGHIJ"))
  expect_equal(substr_chartr(toupper(x), loc=loc),
               c("aBCDEFGHIJ", "AbcDEFGHIJ", "ABcdEFGHIJ"))
})

test_that("substr_addin works", {
  expect_equal(substr_addin(x, " ", "after", loc=loc),
               c("a bcdefghij", "abc defghij", "abcd efghij"))
  expect_equal(substr_addin(x, " ", "before", loc=loc),
               c(" abcdefghij", "a bcdefghij", "ab cdefghij"))
  expect_equal(substr_addin(x, " ", "after", at=loc[,2]),
               c("a bcdefghij", "abc defghij", "abcd efghij"))
  expect_equal(substr_addin(x, " ", "before", at=loc[,1]),
               c(" abcdefghij", "a bcdefghij", "ab cdefghij"))

  expect_equal(substr_addin(x, c(" ", "~", ";"), "after", loc=loc),
               c("a bcdefghij", "abc~defghij", "abcd;efghij"))
  expect_equal(substr_addin(x, c(" ", "~", ";"), "before", loc=loc),
               c(" abcdefghij", "a~bcdefghij", "ab;cdefghij"))
  expect_equal(substr_addin(x, c(" ", "~", ";"), "after", at=loc[,2]),
               c("a bcdefghij", "abc~defghij", "abcd;efghij"))
  expect_equal(substr_addin(x, c(" ", "~", ";"), "before", at=loc[,1]),
               c(" abcdefghij", "a~bcdefghij", "ab;cdefghij"))
})

x <- rep("1234567890", 3)
start <- 1:3
end <- 7:9

test_that("substr_arrange works", {
  expect_equal(substr_arrange(x, "incr", start=start, end=end),
               x)
  expect_equal(substr_arrange(x, "decr", start=start, end=end),
               c("7654321890", "1876543290", "1298765430"))
  expect_equal(substr_arrange(x, "rev", start=start, end=end),
               c("7654321890", "1876543290", "1298765430"))
})



x <- rep(paste0(letters[1:10], collapse = ""), 3)
start=c(1, 2, 3)
end=c(1, 3, 4)
loc=cbind(start, end)

test_that("substr_extract with stringfish works", {
  expect_equal(substr_extract(x, start=start, end=end, fish=TRUE),
               c("a", "bc", "cd"))
  expect_equal(substr_extract(x, "at", start=start, end=end, fish=TRUE),
               c("a", "bc", "cd"))
  expect_equal(substr_extract(x, "before", start=start, end=end, fish=TRUE),
               c("", "a", "ab"))
  expect_equal(substr_extract(x, "after", start=start, end=end, fish=TRUE),
               c(substr(x[1], 2, nchar(x[1])), substr(x[1], 4,nchar(x[1])), substr(x[1], 5, nchar(x[1]))))
  expect_equal(substr_extract(x, loc=loc, fish=TRUE),
               c("a", "bc", "cd"))
  expect_equal(substr_extract(x, "at", loc=loc, fish=TRUE),
               c("a", "bc", "cd"))
  expect_equal(substr_extract(x, "before", loc=loc, fish=TRUE),
               c("", "a", "ab"))
  expect_equal(substr_extract(x, "after", loc=loc, fish=TRUE),
               c(substr(x[1], 2, nchar(x[1])), substr(x[1], 4,nchar(x[1])), substr(x[1], 5, nchar(x[1]))))
})

test_that("substr_repl with stringfish works", {
  expect_equal(substr_repl(x, "!!", start=start, end=end, fish=TRUE),
               c("!!bcdefghij", "a!!defghij", "ab!!efghij"))
  expect_equal(substr_repl(x, "!!", loc=loc, fish=TRUE),
               c("!!bcdefghij", "a!!defghij", "ab!!efghij"))
  expect_equal(substr_repl(x, c("!!", "??", ";;"), start=start, end=end, fish=TRUE),
               c("!!bcdefghij", "a??defghij", "ab;;efghij"))
  expect_equal(substr_repl(x, c("!!", "??", ";;"), loc=loc, fish=TRUE),
               c("!!bcdefghij", "a??defghij", "ab;;efghij"))
})

test_that("substr_chartr with stringfish works", {
  expect_equal(substr_chartr(x, start=start, end=end, fish=TRUE),
               c("Abcdefghij", "aBCdefghij", "abCDefghij"))
  expect_equal(substr_chartr(x, loc=loc, fish=TRUE),
               c("Abcdefghij", "aBCdefghij", "abCDefghij"))
  expect_equal(substr_chartr(toupper(x), start=start, end=end, fish=TRUE),
               c("aBCDEFGHIJ", "AbcDEFGHIJ", "ABcdEFGHIJ"))
  expect_equal(substr_chartr(toupper(x), loc=loc, fish=TRUE),
               c("aBCDEFGHIJ", "AbcDEFGHIJ", "ABcdEFGHIJ"))
})

test_that("substr_addin with stringfish works", {
  expect_equal(substr_addin(x, " ", "after", loc=loc, fish=TRUE),
               c("a bcdefghij", "abc defghij", "abcd efghij"))
  expect_equal(substr_addin(x, " ", "before", loc=loc, fish=TRUE),
               c(" abcdefghij", "a bcdefghij", "ab cdefghij"))
  expect_equal(substr_addin(x, " ", "after", at=loc[,2], fish=TRUE),
               c("a bcdefghij", "abc defghij", "abcd efghij"))
  expect_equal(substr_addin(x, " ", "before", at=loc[,1], fish=TRUE),
               c(" abcdefghij", "a bcdefghij", "ab cdefghij"))

  expect_equal(substr_addin(x, c(" ", "~", ";"), "after", loc=loc, fish=TRUE),
               c("a bcdefghij", "abc~defghij", "abcd;efghij"))
  expect_equal(substr_addin(x, c(" ", "~", ";"), "before", loc=loc, fish=TRUE),
               c(" abcdefghij", "a~bcdefghij", "ab;cdefghij"))
  expect_equal(substr_addin(x, c(" ", "~", ";"), "after", at=loc[,2], fish=TRUE),
               c("a bcdefghij", "abc~defghij", "abcd;efghij"))
  expect_equal(substr_addin(x, c(" ", "~", ";"), "before", at=loc[,1], fish=TRUE),
               c(" abcdefghij", "a~bcdefghij", "ab;cdefghij"))
})

x <- rep("1234567890", 3)
start <- 1:3
end <- 7:9

test_that("substr_arrange with stringfish works", {
  expect_equal(substr_arrange(x, "incr", start=start, end=end, fish=TRUE),
               x)
  expect_equal(substr_arrange(x, "decr", start=start, end=end, fish=TRUE),
               c("7654321890", "1876543290", "1298765430"))
  expect_equal(substr_arrange(x, "rev", start=start, end=end, fish=TRUE),
               c("7654321890", "1876543290", "12987654300"))
})
