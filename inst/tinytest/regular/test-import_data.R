
d <- get(
  utils::data(list="cars", package = "datasets", envir = environment())
)
expect_equal(
  import_data("datasets", "cars") |> head(),
  head(d)
)

expect_error(
  import_data("datasets", c("cars", "cairo")),
  pattern = "only a single dataset and a single package can be given"
)

expect_error(
  import_data(c("datasets", "datasets"), "airmiles"),
  pattern = "only a single dataset and a single package can be given"
)

expect_error(
  import_data("datasets", "airmiles", lib.loc = mean),
  pattern = "`lib.loc` must be a character vector with at least one library path"
)

