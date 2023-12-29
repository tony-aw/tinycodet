# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops


# transform_if works ====
x <- c(-10:9, NA, NA)
obj <- matrix(x, ncol=2)
expected <- obj
expected[is.na(expected)] <- -1000
expected[12:20] <- log(expected[12:20])
expected[1:11] <- expected[1:11]^2
cond <- 0
const <- 1000
expect_equal(
  transform_if(obj, \(x)x>cond, log, \(x)x^2, \(x)-const),
  expected
)


# transform_if works on empty condition subsets ====
expect_equal(
  transform_if(1:10, \(x) x > 1000, 2, 3, -1), # all YES & NA empty
  rep(3, 10)
)
expect_equal(
  transform_if(1:10, \(x) x %in% 1:10, 2, 3, -1), # all NO & NA empty 
  rep(2, 10)
)
expect_equal(
  transform_if(rep(NA, 10), \(x) x == 1, 2, 3, -1), # all YES & NO empty 
  rep(-1, 10)
)
expect_equal(
  transform_if(1:10, as.logical(rep(NA, 10)), 2, 3, -1), # condition completely NA
  rep(-1, 10)
)


# transform_if works like ifelse (but without warnings) ====
expected <- ifelse(
  is.na(obj>0), -1000,
  ifelse(
    obj>0,  log(obj), obj^2
  )
) |> suppressWarnings()
expect_equal(
  transform_if(obj, \(x)x>0, log, \(x)x^2, \(x)-1000),
  expected
)


# transform_if works - all possible combinations of functions/vectors/scalars ====
cond <- list(\(x)x>0, obj>0)
yes <- list(\(x)2, 2)
no <- list(\(x)3, 3)
other <- list(\(x)-1000, -1000)
out <- list()
m <- 1
loops <- loops + 1
for(i in 1:2) {
  for (j in 1:2) {
    for (k in 1:2) {
      for(l in 1:2) {
        out[[m]] <- transform_if(
          obj, cond = cond[[i]], yes = yes[[j]], no = no[[k]], other = other[[l]]
        )
        m <- m + 1
        enumerate <- enumerate + 1

      }
    }
  }
}
expected <-  rep(
  list(transform_if(obj, \(x)x>0, 2, 3, \(x)-1000)),
  times = length(out)
)
expect_equal(out, expected)


# transform_if error checks ====
expect_error(
  transform_if(obj, \(x)x>0, ~ 10, \(x)x^2, \(x)-1000),
  pattern = "improper `yes` given"
)
expect_error(
  transform_if(obj, \(x)x>0, c(1,1), \(x)x^2, \(x)-1000),
  pattern = "improper `yes` given"
)
expect_error(
  transform_if(obj, \(x)x>0, log, ~"hello", \(x)-1000),
  pattern = "improper `no` given"
)
expect_error(
  transform_if(obj, \(x)x>0, log, c(1,1), \(x)-1000),
  pattern = "improper `no` given"
)
expect_error(
  transform_if(obj, \(x)x>0, log, \(x)x^2, ~ x),
  pattern = "improper `other` given"
)
expect_error(
  transform_if(obj, \(x)x>0, log, \(x)x^2, c(1,1)),
  pattern = "improper `other` given"
)

expect_error(
  transform_if(obj, \(x)x*10, log, \(x)x^2, \(x)-1000),
  pattern = paste0(
    "`cond` must be of class logical,",
    "\n",
    "or a function that returns an object of class logical"
  )
)
expect_error(
  transform_if(obj, obj[-1]>0, log, \(x)x^2, \(x)-1000),
  pattern = paste0(
    "`cond` must be the same length as `x`,",
    "\n",
    "or a function that returns an object with the same length as `x`"
  )
)


# subset_if ops work ====
x <- matrix(-10:9, ncol=2)
expect_equal(x %[if]% \(x)x %in% 1:10,
               1:9)
expect_equal(x %[!if]% \(x)x %in% 1:10,
               -10:0)


# subset_if error checks ====
expect_error(
  x %[if]% \(x)x*10,
  pattern = "`cond` must return a logical vector"
)
expect_error(
  x %[!if]% \(x)x*10,
  pattern = "`cond` must return a logical vector"
)


# unreal replacement works checks ====
x <- c(1:10, NA, NaN, Inf, -Inf)
x %unreal =% 0
expect_equal(
  c(1:10, rep(0, 4)),
  x
)

# inplace function works ====
mtcars <- mtcars2 <- datasets::mtcars
y <- 2
mtcars$mpg[mtcars$cyl>6] <- mtcars$mpg[mtcars$cyl>6]^y
mtcars2$mpg[mtcars2$cyl>6] %:=% \(x)x^y
expect_equal(mtcars, mtcars2)


# inplace error checks ====
mtcars <- mtcars2 <- datasets::mtcars
mtcars <- mtcars2 <- datasets::mtcars
y <- 2
mtcars$mpg[mtcars$cyl>6] <- mtcars$mpg[mtcars$cyl>6]^y
expect_error(
  mtcars2$mpg[mtcars2$cyl>6] %:=% ("hello"),
  pattern = "right hand side must be a function"
)

