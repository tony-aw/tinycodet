
# Placeholder with simple test
expect_equal(1 + 1, 2)
expect_message(
  tinycodet:::.onAttach(),
  pattern = "Run `?tinycodet::tinycodet` to open the introduction help page of tinycodet.",
  fixed = TRUE
)
