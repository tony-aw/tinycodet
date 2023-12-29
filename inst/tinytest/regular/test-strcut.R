
# test strcut_loc ====
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
  prepart = rep(NA, 9),
  mainpart = x,
  postpart = rep(NA, 9)
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
  prepart = c(NA, NA),
  mainpart = c("hello", NA),
  postpart = c(NA, NA)
)
expect_equal(strcut_loc(x, loc), out)


# strcut_loc - error checks ====
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


# test strcut_brk - matrix ====
test <- c(
  paste0("The\u00a0above-mentioned    features are very useful. ",
  "Spam, spam, eggs, bacon, and spam. 123 456 789"),
  "good morning, good evening, and good night"
)
expect_equal(
  strcut_brk(test),
  stringi::stri_split_boundaries(test, type="character", simplify = NA)
)
expect_equal(
  strcut_brk(test, type = "character"),
  stringi::stri_split_boundaries(test, type="character", simplify = NA)
)
expect_equal(
  strcut_brk(test, "line"),
  stringi::stri_split_boundaries(test, type="line_break", simplify = NA)
)
expect_equal(
  strcut_brk(test, "line"),
  stringi::stri_split_boundaries(test, type="line_break", simplify = NA)
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


# test strcut_brk - list ====
test <- c(
  paste0("The\u00a0above-mentioned    features are very useful. ",
         "Spam, spam, eggs, bacon, and spam. 123 456 789"),
  "good morning, good evening, and good night"
)
expect_equal(
  strcut_brk(test, tolist = TRUE),
  stringi::stri_split_boundaries(test, type="character", simplify = FALSE)
)
expect_equal(
  strcut_brk(test, type = "character", tolist = TRUE),
  stringi::stri_split_boundaries(test, type="character", simplify = FALSE)
)
expect_equal(
  strcut_brk(test, "line", tolist = TRUE),
  stringi::stri_split_boundaries(test, type="line_break", simplify = FALSE)
)
expect_equal(
  strcut_brk(test, "line", tolist = TRUE),
  stringi::stri_split_boundaries(test, type="line_break", simplify = FALSE)
)
expect_equal(
  strcut_brk(test, "word", tolist = TRUE),
  stringi::stri_split_boundaries(test, type="word", simplify = FALSE)
)
expect_equal(
  strcut_brk(test, "word", skip_word_none=TRUE, tolist = TRUE),
  stringi::stri_split_boundaries(test, type="word", simplify = FALSE, skip_word_none=TRUE)
)
expect_equal(
  strcut_brk(test, "word", skip_word_none=TRUE, skip_word_letter=TRUE, tolist = TRUE),
  stringi::stri_split_boundaries(test, type="word", simplify = FALSE, skip_word_none=TRUE, skip_word_letter=TRUE)
)
expect_equal(
  strcut_brk(test, "word", skip_word_none=FALSE, skip_word_letter=TRUE, tolist = TRUE),
  stringi::stri_split_boundaries(test, type="word", simplify = FALSE, skip_word_none=FALSE, skip_word_letter=TRUE)
)
expect_equal(
  strcut_brk(test, "sentence", tolist = TRUE),
  stringi::stri_split_boundaries(test, type="sentence", simplify = FALSE)
)
expect_equal(
  strcut_brk(test, "sentence", skip_sentence_sep=TRUE, tolist = TRUE),
  stringi::stri_split_boundaries(test, type="sentence", simplify = FALSE, skip_sentence_sep=TRUE)
)


# strcut_brk - error checks ====
expect_error(
  strcut_brk(test, "whoopsie"),
  pattern = paste0("Syntax error in RBBI rule. (U_BRK_RULE_SYNTAX)"),
  fixed = TRUE
)
expect_error(
  strcut_brk(test, tokens_only = TRUE),
  pattern = "formal argument \"tokens_only\" matched by multiple actual arguments"
)
expect_error(
  strcut_brk(test, "chr", simplify = NA),
  pattern = "\"simplify\" matched by multiple actual arguments"
)
expect_error(
  strcut_brk(test, c("character", "word")),
  pattern = "`type` must be a single string"
)
expect_error(
  strcut_brk(test, tolist = NA),
  pattern = "`tolist` must be either `TRUE` or `FALSE`",
  fixed = TRUE
)

