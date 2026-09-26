
foo <- stringi::stri_c
env <- environment(foo)

expect_equal(
  tinycodet:::.rcpp_get_function_name(foo, env, names(env)),
  "stri_c"
)