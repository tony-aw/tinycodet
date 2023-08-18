
d <- get(
  utils::data(list="chicago", package = "gamair", envir = environment())
)
expect_equal(
  import_data("chicago", "gamair") |> head(),
  head(d)
)

expect_error(
  import_data(c("chicago", "cairo"), "gamair"),
  pattern = "only a single dataset and a single package can be given"
)

expect_error(
  import_data("airmiles", c("gamair", "datasets")),
  pattern = "only a single dataset and a single package can be given"
)
