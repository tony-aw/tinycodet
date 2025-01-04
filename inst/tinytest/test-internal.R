
expect_false(tinycodet:::.C_any_neg(c(0, 2^31 + 10)))
expect_true(tinycodet:::.C_any_neg(rep(-2^31 - 10, 10)))
expect_false(tinycodet:::.C_any_nonpos(c(1, 2^31 + 10)))
expect_true(tinycodet:::.C_any_nonpos(c(0, 10)))
expect_true(tinycodet:::.C_any_nonpos(c(0, -2^31 - 10)))
