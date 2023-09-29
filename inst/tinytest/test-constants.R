
# test %<-c% ====
temp.fun <- function() {
  x %<-c% 3
  return(x)
}
expect_equal(temp.fun(), 3)

temp.fun <- function() {
  x <- 4
  x %<-c% 3
  return(x)
}
expect_equal(temp.fun(), 3)

temp.fun <- function() {
  x %<-c% 3
  x <- 4
}
expect_error(temp.fun())

temp.fun <- function() {
  x %<-c% 3
  x %<-c% 4
}
expect_error(temp.fun())

temp.fun <- function() {
  x %<-c% data.frame(x=1, y=2)
  x[1,1] <- 1
}
expect_error(temp.fun())

temp.fun <- function() {
  x %<-c% 3
  return(x)
}
expect_equal(temp.fun(), 3)

temp.fun <- function() {
  x <- 4
  x %<-c% 3
  return(x)
}
expect_equal(temp.fun(), 3)

temp.fun <- function() {
  x %<-c% 3
  x <- 4
}
expect_error(temp.fun())

temp.fun <- function() {
  x %<-c% 3
  x %<-c% 4
}
expect_error(temp.fun())

temp.fun <- function() {
  x %<-c% data.frame(x=1, y=2)
  x[1,1] <- 1
}
expect_error(temp.fun())

# test lock_TF ====
temp.fun <- function() {
  T <- 3
  lock_TF()
  return(T)
}
expect_true(temp.fun())

temp.fun <- function() {
  F <- 4
  lock_TF()
  return(F)
}
expect_false(temp.fun())

temp.fun <- function() {
  lock_TF()
  T <- 3
}
expect_error(temp.fun())

temp.fun <- function() {
  lock_TF()
  F <- 4
}
expect_error(temp.fun())

temp.fun <- function() {
  T <- 3
  lock_TF()
  return(T)
}
expect_true(temp.fun())

temp.fun <- function() {
  F <- 4
  lock_TF()
  return(F)
}
expect_false(temp.fun())

temp.fun <- function() {
  lock_TF()
  T <- 3
}
expect_error(temp.fun())

temp.fun <- function() {
  lock_TF()
  F <- 4
}
expect_error(temp.fun())

my_env <- new.env()
lock_TF(my_env)
expect_true(my_env[["T"]])
expect_false(my_env[["F"]])
expect_error(my_env[["T"]] <- FALSE)
expect_error(my_env[["F"]] <- TRUE)
