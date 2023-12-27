
# basic - equal lengths ====
x <- c(TRUE, FALSE, TRUE, FALSE)
y <- c(FALSE, TRUE, TRUE, FALSE)
outcome <- data.frame(
x=x, y=y,
"x %xor% y"=x %xor% y, "x %n&% y" = x %n&% y, "x %?=% y" = x %?=% y,
check.names = FALSE
)
expected <- data.frame(
x,y,
"x %xor% y"=c(T,T,F,F),
"x %n&% y" =c(F,F,F,T),
"x %?=% y" =c(F,F,F,F),
check.names = FALSE
)
expect_equal(outcome, expected)

# basic - unequal lengths ====
x <- c(TRUE, FALSE)
y <- c(FALSE, TRUE, TRUE, FALSE)
outcome <- data.frame(
  x=x, y=y,
  "x %xor% y"=x %xor% y, "x %n&% y" = x %n&% y, "x %?=% y" = x %?=% y,
  check.names = FALSE
)
expected <- data.frame(
  x,y,
  "x %xor% y"=c(T,T,F,F),
  "x %n&% y" =c(F,F,F,T),
  "x %?=% y" =c(F,F,F,F),
  check.names = FALSE
)
expect_equal(outcome, expected)

# negating logic works (1) ====
df <- expand.grid(x = c(NA, NaN, Inf, -Inf), y = c(NA, NaN, Inf, -Inf))
x <- df$x
y <- df$y
outcome <- data.frame(
x=x, y=y,
"x %xor% y"=x %xor% y, "x %n&% y" = x %n&% y, "x %?=% y" = x %?=% y,
check.names = FALSE
)
expected <- data.frame(
x,y,
"x %xor% y" = ifelse(is.infinite(df$x)&is.infinite(df$y), FALSE, NA),
"x %n&% y" = ifelse(is.infinite(df$x)&is.infinite(df$y), FALSE, NA),
"x %?=% y" = rep(TRUE, 16),
check.names = FALSE
)
expect_equal(outcome, expected)


# negating logic works (3) ====
df <- expand.grid(x = rep(c(NA, NaN, Inf, -Inf), 2), y = rep(c(TRUE, FALSE), 4))
x <- df$x
y <- df$y
expect_equal(x %?=% y, rep(FALSE, 64))



# out works ====
expect_equal(0:3 %out% 1:10, c(T, F,F,F))
expect_equal(1:10 %out% 1:3, c(rep(F, 3), rep(T, 7)))


# numtypes ====
n <- c(0:5, 0:-5, 0.1, -0.1, 0, 1, Inf, -Inf, NA, NaN)
cbind(1:length(n), n)
expect_equal(c(1e-20, 1) %=numtype% "~0", c(TRUE, FALSE))

expect_equal(which(n %=numtype% "B"), c(1, 2, 7, 15, 16))

expect_equal(which(n %=numtype% "prop"), c(1, 2, 7, 13, 15, 16))

expect_equal(which(n %=numtype% "I"), c(1:12, 15:16))

expect_equal(which(n %=numtype% "I"), c(1:12, 15:16))

expect_equal(which(n %=numtype% "odd"), c(2, 4, 6, 8, 10, 12, 16))

expect_equal(which(n %=numtype% "even"), c(1, 3, 5, 7, 9, 11, 15))

expect_equal(which(n %=numtype% "R"), 1:16)

expect_equal(which(n %=numtype% "unreal"), 17:20)


# strtypes ====
s <- c(" AbcZ123 ", " abc ", " 1.3 ", " !#$%^&*() ", "", "NA", "NaN", " Inf ")
cbind(1:length(s), s)

expect_equal(which(s %=strtype% "empty"), 5)

expect_equal(which(s %=strtype% "unreal"), 6:8)

expect_equal(which(s %=strtype% "numeric"), c(3, 8))

expect_equal(which(s %=strtype% "special"), 4)


# error checks ====
expect_error(
  n %=numtype% c("unreal", "~0", "B", "prop", "I", "odd", "even", "R"),
  pattern = "`numtype` must be a single string"
)
expect_error(
  s %=strtype% c("empty", "unreal"),
  pattern = "`strtype` must be a single string"
)
expect_error(
  s %=numtype% "unreal",
  pattern = "`n` must be numeric"
)
expect_error(
  n %=strtype% "unreal",
  pattern = "`s` must be character"
)
expect_error(
  n %=numtype% "foo",
  pattern = "numtype not recognised"
)
expect_error(
  s %=strtype% "foo",
  pattern = "strtype not recognised"
)

