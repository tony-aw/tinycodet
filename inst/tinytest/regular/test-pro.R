
# aes_pro - errors ====
my_x <- "colx"
my_y <- "coly"
expect_error(
  aes_pro(my_x, my_y),
  pattern = "formula inputs must be given"
)

expect_error(
  aes_pro(x ~ y, y ~ x),
  pattern = "improper formula given"
)


# aes_pro - checks ====
if(requireNamespace("ggplot2")) {
  
  my_x <- ~ colX # regular column name
  my_y <- ~ `1st` # special column name
  fill <- ~ colFill
  expect_equal(
    ggplot2::aes(x = colX, y = `1st`, fill = colFill),
    aes_pro(my_x, my_y, fill = fill)
  )
  
  my_x <- ~ colX^2 # regular column name with function/expression
  my_y <- ~ `1st`^2 # special column name with function/expression
  fill <- ~ colFill^2
  expect_equal(
    ggplot2::aes(colX^2, `1st`^2, fill = colFill^2),
    aes_pro(my_x, my_y, fill = fill)
  )
  
  tempfun <- function(x, y, fill) {
    aes_pro(x, y, fill = fill)
  }
  
  
  my_x <- ~ colX # regular column name
  my_y <- ~ `1st` # special column name
  fill <- ~ colFill
  expect_equal(
    ggplot2::aes(x = colX, y = `1st`, fill = colFill),
    tempfun(my_x, my_y, fill)
  )
  
  my_x <- ~ colX^2 # regular column name with function/expression
  my_y <- ~ `1st`^2 # special column name with function/expression
  fill <- ~ colFill^2
  expect_equal(
    ggplot2::aes(colX^2, `1st`^2, fill = colFill^2),
    tempfun(my_x, my_y, fill)
  )
}


# with_pro ====
x <- data.frame(a = 1:10, b = letters[1:10])
myform <- form(~ a^2)
expect_equal(
  with(x, a^2),
  with_pro(x, myform)
)

y <- 2
myform <- form(~ a^y, env = environment())
expect_equal(
  with(x, a^y),
  with_pro(x, myform)
)

tempfun <- function(x, form) {
  with_pro(x, myform)
}

x <- data.frame(a = 1:10, b = letters[1:10])
myform <- form(~ a^2)
expect_equal(
  with(x, a^2),
  tempfun(x, myform)
)

y <- 2
myform <- form(~ a^y, env = environment())
expect_equal(
  with(x, a^y),
  tempfun(x, myform)
)


# with_pro - errors ====
x <- data.frame(a = 1:10, b = letters[1:10])
myform <- form(~ a^2)
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

