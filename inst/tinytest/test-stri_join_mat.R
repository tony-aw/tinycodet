

expected <- c("abcde", "fghij", "klmno", "pqrst", "uvwxy")

expect_equal(matrix(letters[1:25], ncol=5, byrow = TRUE) |> stri_join_mat(margin=1),
             expected)
expect_equal(matrix(letters[1:25], ncol=5, byrow = FALSE) |> stri_join_mat(margin=2),
             expected)

expect_error(
  matrix(letters[1:25], ncol=5, byrow = TRUE) |> stri_join_mat(margin=3),
  pattern = "`margin` must be either 1 or 2"
)

