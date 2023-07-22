

expected <- c("abcde", "fghij", "klmno", "pqrst", "uvwxy")
# "multiplication works", {
  expect_equal(matrix(letters[1:25], ncol=5, byrow = T) |> stri_join_mat(margin=1),
               expected)
  expect_equal(matrix(letters[1:25], ncol=5, byrow = F) |> stri_join_mat(margin=2),
               expected)

