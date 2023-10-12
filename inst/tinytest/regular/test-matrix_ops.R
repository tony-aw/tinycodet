
set.seed(1)
mat <- matrix(sample(1:25), nrow=5)
rank <- sample(1:length(mat)) |> matrix(ncol=ncol(mat))

# "numeric row~ works"
expect_equal(rowSums(mat %row~% rank), rowSums(mat))

# "numric col~ works"
expect_equal(colSums(mat %col~% rank), colSums(mat))


mat <- matrix(as.character(sample(1:25)), nrow=5)
rank <- sample(1:length(mat)) |> matrix(ncol=ncol(mat))

# "character row~ works"
expect_equal(
as.numeric(mat %row~% rank) |> matrix(ncol=ncol(mat)) |> rowSums(),
mat |> as.numeric() |> matrix(ncol=ncol(mat)) |> rowSums()
)
# "character col~ works"
expect_equal(
as.numeric(mat %col~% rank) |> matrix(ncol=ncol(mat)) |> colSums(),
mat |> as.numeric() |> matrix(ncol=ncol(mat)) |> colSums()
)


