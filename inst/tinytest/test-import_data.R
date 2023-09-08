
d <- get(
  utils::data(list="chicago", package = "gamair", envir = environment())
)
expect_equal(
  import_data("gamair", "chicago") |> head(),
  head(d)
)

expect_error(
  import_data("gamair", c("chicago", "cairo")),
  pattern = "only a single dataset and a single package can be given"
)

expect_error(
  import_data(c("gamair", "datasets"), "airmiles"),
  pattern = "only a single dataset and a single package can be given"
)

expect_error(
  import_data("gamair", "airmiles", lib.loc = mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)
