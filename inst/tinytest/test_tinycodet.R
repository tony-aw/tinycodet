
# Placeholder with simple test
expect_equal(1 + 1, 2)
expect_message(
  tinycodet:::.onAttach(),
  pattern = "Run \033[4m?tinycodet::tinycodet\033[24m to open the introduction help page of tinycodet.",
  fixed = TRUE
)
