
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
  
  my_x <- ~ colX # regular characters column name
  my_y <- ~ `1st` # special characters column name
  fill <- ~ `TRUE` # protected characters column name
  expect_equal(
    ggplot2::aes(x = colX, y = `1st`, fill = `TRUE`),
    aes_pro(my_x, my_y, fill = fill)
  )
  
  my_x <- ~ colX^2 # regular characters column name with function/expression
  my_y <- ~ `1st`^2 # special characters column name with function/expression
  fill <- ~ `TRUE`^2 # protected characters column name with function/expression
  expect_equal(
    ggplot2::aes(colX^2, `1st`^2, fill = `TRUE`^2),
    aes_pro(my_x, my_y, fill = fill)
  )
  
  # very long names:
  my_x <- ~ Very_very_very_unecessarily_and_ridiculously_long_variable_name_for_variable1_for_the_purpose_of_testing_programmatically_friendly_functions_without_NSE
  my_y <- ~ Very_very_very_unecessarily_and_ridiculously_long_variable_name_for_variable2_for_the_purpose_of_testing_programmatically_friendly_functions_without_NSE
  expect_equal(
    ggplot2::aes(
      Very_very_very_unecessarily_and_ridiculously_long_variable_name_for_variable1_for_the_purpose_of_testing_programmatically_friendly_functions_without_NSE,
      Very_very_very_unecessarily_and_ridiculously_long_variable_name_for_variable2_for_the_purpose_of_testing_programmatically_friendly_functions_without_NSE
    ),
    aes_pro(my_x, my_y)
  )
  
  
  
  # with tempfun
  tempfun <- function(x, y, fill) {
    aes_pro(x, y, fill = fill)
  }
  
  
  my_x <- ~ colX # regular characters column name
  my_y <- ~ `1st` # special characters column name
  fill <- ~ `TRUE` # protected characters column name
  expect_equal(
    ggplot2::aes(x = colX, y = `1st`, fill = `TRUE`),
    tempfun(my_x, my_y, fill)
  )
  
  my_x <- ~ colX^2 # regular characters column name with function/expression
  my_y <- ~ `1st`^2 # special characters column name with function/expression
  fill <- ~ `TRUE`^2 # protectec characters column name with function/expression
  expect_equal(
    ggplot2::aes(colX^2, `1st`^2, fill = `TRUE`^2),
    tempfun(my_x, my_y, fill)
  )
  
  # very long names:
  my_x <- ~ Very_very_very_unecessarily_and_ridiculously_long_variable_name_for_variable1_for_the_purpose_of_testing_programmatically_friendly_functions_without_NSE
  my_y <- ~ Very_very_very_unecessarily_and_ridiculously_long_variable_name_for_variable2_for_the_purpose_of_testing_programmatically_friendly_functions_without_NSE
  fill <- ~ Very_very_very_unecessarily_and_ridiculously_long_variable_name_for_variable3_for_the_purpose_of_testing_programmatically_friendly_functions_without_NSE
  expect_equal(
    ggplot2::aes(
      Very_very_very_unecessarily_and_ridiculously_long_variable_name_for_variable1_for_the_purpose_of_testing_programmatically_friendly_functions_without_NSE,
      Very_very_very_unecessarily_and_ridiculously_long_variable_name_for_variable2_for_the_purpose_of_testing_programmatically_friendly_functions_without_NSE,
      fill = Very_very_very_unecessarily_and_ridiculously_long_variable_name_for_variable3_for_the_purpose_of_testing_programmatically_friendly_functions_without_NSE
    ),
    tempfun(my_x, my_y, fill)
  )
}

form1 <- ~ Very_very_very_unecessarily_and_ridiculously_long_variable_name_for_variable1_for_the_purpose_of_testing_programmatically_friendly_functions_without_NSE
form2 <- ~ Very_very_very_unecessarily_and_ridiculously_long_variable_name_for_variable2_for_the_purpose_of_testing_programmatically_friendly_functions_without_NSE
expect_silent(
  aes_pro(form1, form2)
)



# with_pro ====
x <- data.frame(a = 1:10, `1st` = 11:20, `TRUE` = 21:30, check.names = FALSE)
myform <- ~  a^2 + `1st`^2 + `TRUE`^2
expect_equal(
  with(x,  a^2 + `1st`^2 + `TRUE`^2),
  with_pro(x, myform)
)

y <- 2
myform <- ~ a^y
expect_equal(
  with(x, a^y),
  with_pro(x, myform)
)

tempfun <- function(x, form) {
  with_pro(x, form)
}

x <- data.frame(a = 1:10, `1st` = 11:20, `TRUE` = 21:30, check.names = FALSE)
myform <- ~  a^2 + `1st`^2 + `TRUE`^2
expect_equal(
  with(x,   a^2 + `1st`^2 + `TRUE`^2),
  tempfun(x, myform)
)

y <- 2
myform <- ~ a^y
expect_equal(
  with(x, a^y),
  tempfun(x, myform)
)


# with_pro - errors ====
x <- data.frame(a = 1:10, b = 11:20)
myform <- ~  a^2 + b^2
expect_error(
  with_pro(x, ~ z^2),
  pattern = "object 'z' not found",
  fixed = TRUE
)

expect_error(
  with_pro(1:10, myform),
  pattern = "`data` must be a list or data.frame",
  fixed = TRUE
)

expect_error(
  with_pro(x, " a^2 + b^2"),
  pattern = "`form` must be a formula",
  fixed = TRUE
)

expect_error(
  with_pro(x, a ~ b),
  pattern = "improper formula given"
)

x <- data.frame(a = 1:10, b = 11:20)
myform <- ~  a^2 + b^2
with_pro(x, myform)
expect_false(is.null(environment(myform)))

