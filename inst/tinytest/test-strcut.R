
# test strcut_loc (complete_na = TRUE) ====
x <- rep(paste0(1:9, collapse=""),9)
x.split <- 1:9 |> as.character()
print(x)
loc <- stri_locate_ith(x, 1:9, fixed = as.character(1:9))
out <- cbind(
  prepart = c("", lapply(1:8, \(x)paste0(x.split[(1:x)], collapse = ""))|> unlist()),
  mainpart = 1:9 |> as.character(),
  postpart = c(lapply(1:9, \(x)paste0(x.split[-(1:x)], collapse = "")) |> unlist())
)
expect_equal(strcut_loc(x, loc), out)

x <- rep(paste0(1:10, collapse=""), 10)
print(x)
loc <- stri_locate_ith(x, 1:10, fixed = as.character(1:10))
out <- cbind(
  prepart = rep("1234", 10),
  mainpart = rep("5", 10),
  postpart = rep("678910", 10)

)
expect_equal(strcut_loc(x, c(5,5)), out)


x <- rep(paste0(1:9, collapse=""),9)
loc <- cbind(rep(NA, 9), rep(NA, 9))
out <- cbind(
  prepart = rep("", 9),
  mainpart = x,
  postpart = rep("", 9)
)
expect_equal(strcut_loc(x, loc), out)

x <- rep(NA, 9)
loc <- stri_locate_ith(x, 1:9, fixed = as.character(1:9))
out <- cbind(
  prepart = rep(NA, 9),
  mainpart = rep(NA, 9),
  postpart = rep(NA, 9)
)
expect_equal(strcut_loc(x, loc), out)

x <- c("hello", NA)
loc <- matrix(c(2, 2, NA, NA), ncol=2, byrow=TRUE)
out <- cbind(
  prepart = c("h", NA),
  mainpart = c("e", NA),
  postpart = c("llo", NA)
)
expect_equal(strcut_loc(x, loc), out)

x <- c("hello", NA)
loc <- matrix(c(NA, NA, 5, 5), ncol=2, byrow=TRUE)
out <- cbind(
  prepart = c("", NA),
  mainpart = c("hello", NA),
  postpart = c("", NA)
)
expect_equal(strcut_loc(x, loc), out)


# test strcut_loc (complete_na = FALSE) ====
x <- rep(paste0(1:9, collapse=""),9)
x.split <- 1:9 |> as.character()
print(x)
loc <- stri_locate_ith(x, 1:9, fixed = as.character(1:9))
out <- cbind(
  prepart = c("", lapply(1:8, \(x)paste0(x.split[(1:x)], collapse = ""))|> unlist()),
  mainpart = 1:9 |> as.character(),
  postpart = c(lapply(1:9, \(x)paste0(x.split[-(1:x)], collapse = "")) |> unlist())
)
expect_equal(strcut_loc(x, loc, fill_loc = FALSE), out)

x <- rep(paste0(1:10, collapse=""), 10)
print(x)
loc <- stri_locate_ith(x, 1:10, fixed = as.character(1:10))
out <- cbind(
  prepart = rep("1234", 10),
  mainpart = rep("5", 10),
  postpart = rep("678910", 10)

)
expect_equal(strcut_loc(x, c(5,5), fill_loc = FALSE), out)


x <- rep(paste0(1:9, collapse=""),9)
loc <- cbind(rep(NA, 9), rep(NA, 9))
out <- matrix(as.character(rep(NA, 3*9)), ncol=3)
colnames(out) <- c("prepart", "mainpart", "postpart")
expect_equal(strcut_loc(x, loc, fill_loc = FALSE), out)

x <- rep(NA, 9)
loc <- stri_locate_ith(x, 1:9, fixed = as.character(1:9))
out <- cbind(
  prepart = rep(NA, 9),
  mainpart = rep(NA, 9),
  postpart = rep(NA, 9)
)
expect_equal(strcut_loc(x, loc, fill_loc = FALSE), out)

x <- c("hello", NA)
loc <- matrix(c(2, 2, NA, NA), ncol=2, byrow=TRUE)
out <- cbind(
  prepart = c("h", NA),
  mainpart = c("e", NA),
  postpart = c("llo", NA)
)
expect_equal(strcut_loc(x, loc, fill_loc = FALSE), out)

x <- c("hello", NA, "hello")
loc <- matrix(c(NA, NA, 5, 5, 1, 5), ncol=2, byrow=TRUE)
out <- cbind(
  prepart = c(NA, NA, ""),
  mainpart = c(NA, NA, "hello"),
  postpart = c(NA, NA, "")
)
expect_equal(strcut_loc(x, loc, fill_loc = FALSE), out)


# strcut_loc - error checks ====
x <- c("hello", "goodbye")
loc <- cbind(c(2, 2), c(1, 1))
expect_error(
  strcut_loc(x, loc, fill_loc = NA),
  pattern = "`fill_loc` must be either `TRUE` or `FALSE`"
)

x <- c("hello", "goodbye")
loc <- cbind(c(2, 2), c(1, 1))
expect_error(
  strcut_loc(x, loc, fill_loc = "foo"),
  pattern = "`fill_loc` must be either `TRUE` or `FALSE`"
)

x <- c("hello", "goodbye")
loc <- matrix(rep(-1, 4), ncol=2)
expect_error(
  strcut_loc(x, loc),
  pattern = "`loc` can only have strictly positive numbers"
)

x <- c("hello", "goodbye")
loc <- cbind(c(2, 2), c(1, 1))
expect_error(
  strcut_loc(x, loc),
  pattern = "`loc[, 2] < loc[, 1]`", fixed =  TRUE
)


# test strcut_brk ====
test <- c(
  paste0("The\u00a0above-mentioned    features are very useful. ",
  "Spam, spam, eggs, bacon, and spam. 123 456 789"),
  "good morning, good evening, and good night"
)
expect_equal(
  strcut_brk(test, "line"),
  stringi::stri_split_boundaries(test, type="line", simplify = NA)
)
expect_equal(
  strcut_brk(test, "word"),
  stringi::stri_split_boundaries(test, type="word", simplify = NA)
)
expect_equal(
  strcut_brk(test, "word", skip_word_none=TRUE),
  stringi::stri_split_boundaries(test, type="word", simplify = NA, skip_word_none=TRUE)
)
expect_equal(
  strcut_brk(test, "word", skip_word_none=TRUE, skip_word_letter=TRUE),
  stringi::stri_split_boundaries(test, type="word", simplify = NA, skip_word_none=TRUE, skip_word_letter=TRUE)
)
expect_equal(
  strcut_brk(test, "word", skip_word_none=FALSE, skip_word_letter=TRUE),
  stringi::stri_split_boundaries(test, type="word", simplify = NA, skip_word_none=FALSE, skip_word_letter=TRUE)
)
expect_equal(
  strcut_brk(test, "sentence"),
  stringi::stri_split_boundaries(test, type="sentence", simplify = NA)
)
expect_equal(
  strcut_brk(test, "sentence", skip_sentence_sep=TRUE),
  stringi::stri_split_boundaries(test, type="sentence", simplify = NA, skip_sentence_sep=TRUE)
)
expect_equal(
  strcut_brk(test),
  stringi::stri_split_boundaries(test, type="character", simplify = NA)
)

# strcut_brk - error checks ====
expect_error(
  strcut_brk(test, "whoopsie"),
  pattern = paste0("Syntax error in RBBI rule. (U_BRK_RULE_SYNTAX)"),
  fixed = TRUE
)
expect_error(
  strcut_brk(test, n=1)
)
expect_error(
  strcut_brk(test, tokens_only=TRUE)
)
expect_error(
  strcut_brk(test, "chr", simplify = NA)
)
expect_error(
  strcut_brk(test, c("chr", "word")),
  pattern = "`brk` must be a single string"
)

