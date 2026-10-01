
# no duplicates ====
expect_false(
 any(duplicated(import_ls("stringi")))
)

# print equals value ====
co <- capture.output(import_ls("stringi")) |> paste0(collapse = "")
con <- textConnection(co)
result <- dget(con)
close(con)
expect_equal(
  sort(result),
  import_ls("stringi")
)

# error handling ====
pattern <- "`types` must be a non-duplicate character vector with at least one element, containing only {reg, infix, rp, nonfun}"
expect_error(
  import_ls("stringi", "inop"),
  pattern = pattern,
  fixed = TRUE
)
