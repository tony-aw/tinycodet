

# empty environment check ====

myfun <- function() {
  foo <- sample(letters, 1e5, TRUE)
  return(form(a ~ b))
}
x <- myfun()
expect_equal(
  environment(x) |> as.list(),
  list()
)
expect_null(
  environment(x)
)


# non-empty environment checks ====
myfun <- function() {
  foo <- letters
  return(form(a ~ b, environment()))
}
x <- myfun()
expect_equal(
  environment(x) |> as.list(),
  list(foo = letters)
)


# string equals formula input ====
expect_equal(
  form("x"),
  form(~x)
)
expect_equal(
  form("x"),
  form(~x)
)
mystring <- "x"
expect_equal(
  form(mystring),
  form(~x)
)


# functional usage check - literal formula input, env = NULL ====
myform <- form(~ stri.)
expect_equal(
  import_as(~ stri., "stringi"),
  import_as(myform, "stringi")
)
mydata <- data.frame(
  y = rpois(1e4, 10),
  x1 = rnorm(1e4),
  x2 = rnorm(1e4),
  id = sample(as.factor(letters), 1e4, TRUE)
)
myform <- form(sqrt(y) ~ x1 + x2^2)
expect_equal(
  model.frame(myform, data = mydata),
  model.frame(sqrt(y) ~ x1 + x2^2, data = mydata)
)
expect_equal(
  model.matrix(myform, data = mydata),
  model.matrix(sqrt(y) ~ x1 + x2^2, data = mydata)
)
expect_equal(
  coef(summary(lm(myform, data = mydata))),
  coef(summary(lm(sqrt(y) ~ x1 + x2^2, data = mydata)))
)

myform <- form(y ~ x1 + x2^2)
expect_equal(
  coef(summary(glm(myform, data = mydata, family = poisson()))),
  coef(summary(glm(y ~ x1 + x2^2, data = mydata, family = poisson())))
)
myform <- form(count ~ spray)
expect_equal(
  boxplot(myform, data = InsectSprays, col = "lightgray", plot = FALSE),
  boxplot(count ~ spray, data = InsectSprays, col = "lightgray", plot = FALSE)
)
if(requireNamespace("mgcv")) {
  myform <- form(y ~ s(x1) + s(x2) + s(id, bs = "re"))
  expect_equal(
    coef(summary(mgcv::gam(myform, data = mydata, family = poisson()))),
    coef(summary(mgcv::gam(y ~ s(x1) + s(x2), data = mydata, family = poisson())))
  )
}
if(requireNamespace("nlme")) {
  main <- form(height ~ SSasymp(age, Asym, R0, lrc))
  fixed <- form(Asym + R0 + lrc ~ 1)
  random <- form(Asym ~ 1)
  
  fm1 <- nlme::nlme(main,
              data = Loblolly,
              fixed = fixed,
              random = random,
              start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
  
  fm2 <- nlme::nlme(height ~ SSasymp(age, Asym, R0, lrc),
              data = Loblolly,
              fixed = Asym + R0 + lrc ~ 1,
              random = Asym ~ 1,
              start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
  expect_equal(
    coef(summary(fm1)),
    coef(summary(fm2))
  )
}
if(requireNamespace("ggplot2")) {
  myform <- form(~ a)
  expect_equal(
    ggplot2::facet_wrap(myform),
    ggplot2::facet_wrap(~ a)
  )
  
}


# functional usage check - string variable input, env = NULL ====
myform <- form("stri.")
expect_equal(
  import_as(~ stri., "stringi"),
  import_as(myform, "stringi")
)
mydata <- data.frame(
  y = rpois(1e4, 10),
  x1 = rnorm(1e4),
  x2 = rnorm(1e4),
  id = sample(as.factor(letters), 1e4, TRUE)
)
mystring <- "sqrt(y) ~ x1 + x2^2"
myform <- form(mystring)
expect_equal(
  model.frame(myform, data = mydata),
  model.frame(sqrt(y) ~ x1 + x2^2, data = mydata)
)
expect_equal(
  model.matrix(myform, data = mydata),
  model.matrix(sqrt(y) ~ x1 + x2^2, data = mydata)
)
expect_equal(
  coef(summary(lm(myform, data = mydata))),
  coef(summary(lm(sqrt(y) ~ x1 + x2^2, data = mydata)))
)

mystring <- "y ~ x1 + x2^2"
myform <- form(mystring)
expect_equal(
  coef(summary(glm(myform, data = mydata, family = poisson()))),
  coef(summary(glm(y ~ x1 + x2^2, data = mydata, family = poisson())))
)
mystring <- "count ~ spray"
myform <- form(mystring)
expect_equal(
  boxplot(myform, data = InsectSprays, col = "lightgray", plot = FALSE),
  boxplot(count ~ spray, data = InsectSprays, col = "lightgray", plot = FALSE)
)
if(requireNamespace("mgcv")) {
  mystring <- 'y ~ s(x1) + s(x2) + s(id, bs = "re")'
  myform <- form(mystring)
  expect_equal(
    coef(summary(mgcv::gam(myform, data = mydata, family = poisson()))),
    coef(summary(mgcv::gam(y ~ s(x1) + s(x2), data = mydata, family = poisson())))
  )
}
if(requireNamespace("nlme")) {
  mystrings <- c(
    "height ~ SSasymp(age, Asym, R0, lrc)",
    "Asym + R0 + lrc ~ 1",
    "Asym ~ 1"
  )
  main <- form(mystrings[1])
  fixed <- form(mystrings[2])
  random <- form(mystrings[3])
  
  fm1 <- nlme::nlme(main,
                    data = Loblolly,
                    fixed = fixed,
                    random = random,
                    start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
  
  fm2 <- nlme::nlme(height ~ SSasymp(age, Asym, R0, lrc),
                    data = Loblolly,
                    fixed = Asym + R0 + lrc ~ 1,
                    random = Asym ~ 1,
                    start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
  expect_equal(
    coef(summary(fm1)),
    coef(summary(fm2))
  )
}
if(requireNamespace("ggplot2")) {
  mystring <- "a"
  myform <- form(mystring)
  expect_equal(
    ggplot2::facet_wrap(myform),
    ggplot2::facet_wrap(~ a)
  )
  
}


# functional usage check - literal formula input, env = baseenv ====
myform <- form(~ stri., env = baseenv())
expect_equal(
  import_as(~ stri., "stringi"),
  import_as(myform, "stringi")
)
mydata <- data.frame(
  y = rpois(1e4, 10),
  x1 = rnorm(1e4),
  x2 = rnorm(1e4),
  id = sample(as.factor(letters), 1e4, TRUE)
)
myform <- form(sqrt(y) ~ x1 + x2^2, env = baseenv())
expect_equal(
  model.frame(myform, data = mydata),
  model.frame(sqrt(y) ~ x1 + x2^2, data = mydata)
)
expect_equal(
  model.matrix(myform, data = mydata),
  model.matrix(sqrt(y) ~ x1 + x2^2, data = mydata)
)
expect_equal(
  coef(summary(lm(myform, data = mydata))),
  coef(summary(lm(sqrt(y) ~ x1 + x2^2, data = mydata)))
)

myform <- form(y ~ x1 + x2^2, env = baseenv())
expect_equal(
  coef(summary(glm(myform, data = mydata, family = poisson()))),
  coef(summary(glm(y ~ x1 + x2^2, data = mydata, family = poisson())))
)
myform <- form(count ~ spray, env = baseenv())
expect_equal(
  boxplot(myform, data = InsectSprays, col = "lightgray", plot = FALSE),
  boxplot(count ~ spray, data = InsectSprays, col = "lightgray", plot = FALSE)
)
if(requireNamespace("mgcv")) {
  myform <- form(y ~ s(x1) + s(x2) + s(id, bs = "re"), env = baseenv())
  expect_equal(
    coef(summary(mgcv::gam(myform, data = mydata, family = poisson()))),
    coef(summary(mgcv::gam(y ~ s(x1) + s(x2), data = mydata, family = poisson())))
  )
}
if(requireNamespace("nlme")) {
  main <- form(height ~ SSasymp(age, Asym, R0, lrc), env = baseenv())
  fixed <- form(Asym + R0 + lrc ~ 1)
  random <- form(Asym ~ 1)
  
  fm1 <- nlme::nlme(main,
                    data = Loblolly,
                    fixed = fixed,
                    random = random,
                    start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
  
  fm2 <- nlme::nlme(height ~ SSasymp(age, Asym, R0, lrc),
                    data = Loblolly,
                    fixed = Asym + R0 + lrc ~ 1,
                    random = Asym ~ 1,
                    start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
  expect_equal(
    coef(summary(fm1)),
    coef(summary(fm2))
  )
}
if(requireNamespace("ggplot2")) {
  myform <- form(~ a, env = baseenv())
  expect_equal(
    ggplot2::facet_wrap(myform),
    ggplot2::facet_wrap(~ a)
  )
}

# functional usage check - string variable input, env = baseenv ====
myform <- form("stri.", env = baseenv())
expect_equal(
  import_as(~ stri., "stringi"),
  import_as(myform, "stringi")
)
mydata <- data.frame(
  y = rpois(1e4, 10),
  x1 = rnorm(1e4),
  x2 = rnorm(1e4),
  id = sample(as.factor(letters), 1e4, TRUE)
)
mystring <- "sqrt(y) ~ x1 + x2^2"
myform <- form(mystring, env = baseenv())
expect_equal(
  model.frame(myform, data = mydata),
  model.frame(sqrt(y) ~ x1 + x2^2, data = mydata)
)
expect_equal(
  model.matrix(myform, data = mydata),
  model.matrix(sqrt(y) ~ x1 + x2^2, data = mydata)
)
expect_equal(
  coef(summary(lm(myform, data = mydata))),
  coef(summary(lm(sqrt(y) ~ x1 + x2^2, data = mydata)))
)

mystring <- "y ~ x1 + x2^2"
myform <- form(mystring, env = baseenv())
expect_equal(
  coef(summary(glm(myform, data = mydata, family = poisson()))),
  coef(summary(glm(y ~ x1 + x2^2, data = mydata, family = poisson())))
)
mystring <- "count ~ spray"
myform <- form(mystring, env = baseenv())
expect_equal(
  boxplot(myform, data = InsectSprays, col = "lightgray", plot = FALSE),
  boxplot(count ~ spray, data = InsectSprays, col = "lightgray", plot = FALSE)
)
if(requireNamespace("mgcv")) {
  mystring <- 'y ~ s(x1) + s(x2) + s(id, bs = "re")'
  myform <- form(mystring, env = baseenv())
  expect_equal(
    coef(summary(mgcv::gam(myform, data = mydata, family = poisson()))),
    coef(summary(mgcv::gam(y ~ s(x1) + s(x2), data = mydata, family = poisson())))
  )
}
if(requireNamespace("nlme")) {
  mystrings <- c(
    "height ~ SSasymp(age, Asym, R0, lrc)",
    "Asym + R0 + lrc ~ 1",
    "Asym ~ 1"
  )
  main <- form(mystrings[1], env = baseenv())
  fixed <- form(mystrings[2], env = baseenv())
  random <- form(mystrings[3], env = baseenv())
  
  fm1 <- nlme::nlme(main,
                    data = Loblolly,
                    fixed = fixed,
                    random = random,
                    start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
  
  fm2 <- nlme::nlme(height ~ SSasymp(age, Asym, R0, lrc),
                    data = Loblolly,
                    fixed = Asym + R0 + lrc ~ 1,
                    random = Asym ~ 1,
                    start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
  expect_equal(
    coef(summary(fm1)),
    coef(summary(fm2))
  )
}
if(requireNamespace("ggplot2")) {
  mystring <- "a"
  myform <- form(mystring, env = baseenv())
  expect_equal(
    ggplot2::facet_wrap(myform),
    ggplot2::facet_wrap(~ a)
  )
}

# string to formula check ====
myform <- ~ a
environment(myform) <- NULL
expect_equal(
  form("a"),
  myform
)

myform <- ~ a
environment(myform) <- baseenv()
expect_equal(
  form("a", env = baseenv()),
  myform
)


# check if the user cannot trick form()'s literal formula checks ====
`~a` <- ~a
expect_error(
  form(`~a`)
)

`~open` <- ~`open`
expect_error(
  form(`~open`)
)


# errors ====

myform <- x ~ y
expect_error(
  form(myform),
  pattern = "if `f` is a formula, it must be a literal formula, not a variable that contains a formula",
  fixed = TRUE
)
expect_error(
  form(1),
  pattern = "`f` must be a single string or a literal formula"
)
expect_error(
  form(letters),
  pattern = "multiple strings not allowed"
)


