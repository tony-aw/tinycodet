
x <- c(0.3, 0.6, 0.7)
y <- c(0.1*3, 0.1*6, 0.1*7)

# "decimal number strict equality is TRUE", {
expect_equal(x %d==% y, rep(TRUE, 3))

# "decimal number strict equality is FALSE", {
expect_equal((x+1) %d==% y, rep(FALSE, 3))


# "decimal number strict inequality is FALSE", {
expect_equal(x %d!=% y, rep(FALSE, 3))

# "decimal number strict inequality is TRUE", {
expect_equal((x+1) %d!=% y, rep(TRUE, 3))


# "decimal number strict greater is TRUE", {
expect_equal((x+1) %d>% y, rep(TRUE, 3))

# "decimal number strict greater is FALSE", {
expect_equal(x %d>% y, rep(FALSE, 3))


# "decimal number strict smaller is TRUE", {
expect_equal((x-1) %d<% y, rep(TRUE, 3))

# "decimal number strict smaller is FALSE", {
expect_equal(x %d<% y, rep(FALSE, 3))


# "decimal number <= is TRUE (x and y equal)", {
expect_equal(x %d<=% y, rep(TRUE, 3))

# "decimal number <= is FALSE (x > y)", {
expect_equal((x+1) %d<=% y, rep(FALSE, 3))


# "decimal number >= is TRUE (x and y equal)", {
expect_equal(x %d>=% y, rep(TRUE, 3))

# "decimal number >= is FALSE (x < y)", {
expect_equal((x-1) %d>=% y, rep(FALSE, 3))


x <- c(0.3, 0.6, 0.7)
bnd <- matrix(c(0.29, 0.59, 0.69, 0.31, 0.61, 0.71), ncol=2)
# "decimal number boundaries (matrix)", {
expect_equal(x %d{}% bnd, rep(TRUE, 3))
expect_equal(x %d!{}% bnd, rep(FALSE, 3))
expect_equal(x %d{}% -bnd, rep(FALSE, 3))
expect_equal(x %d!{}% -bnd, rep(TRUE, 3))


# "decimal number boundaries (vector)", {
expect_equal(0.3 %d{}% c(0.29, 0.31), TRUE)
expect_equal(0.3 %d!{}% c(0.29, 0.31), FALSE)
expect_equal(-0.3 %d{}% c(0.29, 0.31), FALSE)
expect_equal(-0.3 %d!{}% c(0.29, 0.31), TRUE)

