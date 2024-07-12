
expect_false(tinycodet:::.C_any_neg(2^31 + 10))
expect_true(tinycodet:::.C_any_neg(-2^31 -10))
expect_false(tinycodet:::.C_any_nonpos(2^31 + 10))
expect_true(tinycodet:::.C_any_nonpos(0))
