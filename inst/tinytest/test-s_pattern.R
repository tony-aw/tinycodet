# rgex ====
out <- s_regex(
  "hello", case_insensitive = FALSE,
  comments = FALSE,
  dotall = FALSE,
  multiline = FALSE,
  time_limit = 0L,
  stack_limit = 0L
)

expect <- c(
  list(regex = "hello"),
  stringi::stri_opts_regex(case_insensitive = FALSE,
                           comments = FALSE,
                           dotall = FALSE,
                           multiline = FALSE,
                           time_limit = 0L,
                           stack_limit = 0L)
)

expect_equal(out, expect)

# fixed ====
out <- s_fixed(
  "hello", case_insensitive = FALSE,
  overlap = FALSE
)
expect <- c(
  list(fixed = "hello"),
  stringi::stri_opts_fixed(case_insensitive = FALSE, overlap = FALSE)
)
expect_equal(out, expect)

# coll ====
out <- s_coll(
  "hello",
  locale = NULL,
  strength = 3L,
  alternate_shifted = FALSE,
  french = FALSE,
  uppercase_first = NA,
  case_level = FALSE,
  numeric = FALSE,
  normalization = FALSE
)

expect <- c(
  list(coll="hello"),
  stringi::stri_opts_collator(
    locale = NULL,
    strength = 3L,
    alternate_shifted = FALSE,
    french = FALSE,
    uppercase_first = NA,
    case_level = FALSE,
    numeric = FALSE,
    normalization = FALSE,
  )
)
expect_equal(out, expect)

# chrcls ====
out <- s_chrcls("hello", some_option=NA)
expect <- list(charclass="hello", some_option=NA)
expect_equal(out, expect)
