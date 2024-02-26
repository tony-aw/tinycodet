
enumerate <- 0

# aes_pro - errors ====
x <- "colx"
y <- "coly"
expect_error(
  aes_pro(x, y),
  pattern = "formula inputs must be given"
)

expect_error(
  aes_pro(x ~ y, y ~ x),
  pattern = "improper formula given"
)


# aes_pro - checks ====
if(requireNamespace("ggplot2")) {
  
  x <- ~ colX # regular column name
  y <- ~ `1st` # special column name
  fill <- ~ colFill
  expect_equal(
    ggplot2::aes(x = colX, y = `1st`, fill = colFill),
    aes_pro(x, y, fill = fill)
  )
  
  x <- ~ colX^2 # regular column name with function/expression
  y <- ~ `1st`^2 # special column name with function/expression
  fill <- ~ colFill^2
  expect_equal(
    ggplot2::aes(colX^2, `1st`^2, fill = colFill^2),
    aes_pro(x, y, fill = fill)
  )
  
}

enumerate <- enumerate + 4


# with_pro ====
x <- data.frame(a = 1:10, b = letters[1:10])
myform <- ~ a^2
expect_equal(
  with(x, a^2),
  with_pro(x, myform)
)
expect_error(
  with_pro(x, ~ c^2),
  pattern = "unknown variable(s) given",
  fixed = TRUE
)

expect_error(
  with_pro(1:10, myform),
  pattern = "`data` must be a recursive object",
  fixed = TRUE
)

expect_error(
  with_pro(x, "a^2"),
  pattern = "`form` must be a formula",
  fixed = TRUE
)

expect_error(
  with_pro(x, a ~ b),
  pattern = "improper formula given"
)

enumerate <- enumerate + 5

