# regex ====
expected <- c(
  list(regex = "hello"),
  stringi::stri_opts_regex(case_insensitive = FALSE,
                           comments = FALSE,
                           dotall = FALSE,
                           multiline = FALSE,
                           time_limit = 0L,
                           stack_limit = 0L),
  list(foo = 1:10)
)

expect_equal(s_regex(
  "hello", case_insensitive = FALSE,
  comments = FALSE,
  dotall = FALSE,
  multiline = FALSE,
  time_limit = 0L,
  stack_limit = 0L,
  foo = 1:10
), expected)


tempfun <- function() {
  s_regex(
    "hello", case_insensitive = FALSE,
    comments = FALSE,
    dotall = FALSE,
    multiline = FALSE,
    time_limit = 0L,
    stack_limit = 0L,
    foo = 1:10
  )
}
expect_equal(tempfun(), expected)


# fixed ====
expected <- c(
  list(fixed = "hello"),
  stringi::stri_opts_fixed(case_insensitive = FALSE, overlap = FALSE),
  list(foo = 1:10)
)
expect_equal(s_fixed(
  "hello", case_insensitive = FALSE,
  overlap = FALSE,
  foo = 1:10
), expected)


tempfun <- function() {
  s_fixed(
    "hello", case_insensitive = FALSE,
    overlap = FALSE,
    foo = 1:10
  )
}
expect_equal(tempfun(), expected)



# coll ====
expected <- c(
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
  ),
  list(foo = 1:10)
)
expect_equal(s_coll(
  "hello",
  locale = NULL,
  strength = 3L,
  alternate_shifted = FALSE,
  french = FALSE,
  uppercase_first = NA,
  case_level = FALSE,
  numeric = FALSE,
  normalization = FALSE,
  foo = 1:10
), expected)

tempfun <- function() {
  s_coll(
    "hello",
    locale = NULL,
    strength = 3L,
    alternate_shifted = FALSE,
    french = FALSE,
    uppercase_first = NA,
    case_level = FALSE,
    numeric = FALSE,
    normalization = FALSE,
    foo = 1:10
  )
}
expect_equal(tempfun(), expected)


# chrcls ====
expected <- list(charclass="hello", some_option=NA,
                 foo = 1:10)
expect_equal(s_chrcls("hello", some_option=NA,
                      foo = 1:10), expected)

tempfun <- function() {
  s_chrcls("hello", some_option=NA,
           foo = 1:10)
}
expect_equal(tempfun(), expected)



