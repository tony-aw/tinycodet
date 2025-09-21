
# set-up ====
enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


# main tests ====
as_type <- function(x, as_fun, ...) {
  out <- as_fun(x, ...)
  dim(out) <- dim(x)
  dimnames(out) <- dimnames(x)
  names(out) <- names(x)
  return(out)
}
funlist <- list(
  as.raw,
  as.logical,
  as.integer,
  as.double,
  as.complex,
  as.character,
  as.list
)

for(j in c(1, 2, 4, 5, 10)) {
  for(i in seq_along(funlist)) {
    x <- sample(1:20)
    x <- as_type(x, funlist[[i]])
    dim(x) <- c(j, 20/j)
    mat <- sample(1:length(x)) |> matrix(ncol=ncol(x))
    
    expect_equal(
      as.numeric(x %row~% mat) |> matrix(ncol=ncol(x)) |> rowSums(),
      x |> as.numeric() |> matrix(ncol=ncol(x)) |> rowSums()
    ) |> errorfun()
    expect_equal(
      as.numeric(x %col~% mat) |> matrix(ncol=ncol(x)) |> colSums(),
      x |> as.numeric() |> matrix(ncol=ncol(x)) |> colSums()
    ) |> errorfun()
    
    enumerate <- enumerate + 2L
    
  }
}




# errors ====

x <- matrix(1:20, 5, 4)
mat <- matrix(20:1, 4, 5)
expect_error(
  x %row~% mat,
  pattern = "non-conformable matrices"
)
expect_error(
  x %col~% mat,
  pattern = "non-conformable matrices"
)


x <- matrix(1:20, 5, 4)
mat <- 1:20
expect_error(
  x %row~% mat,
  pattern = "both arguments must be matrices"
)
expect_error(
  x %col~% mat,
  pattern = "both arguments must be matrices"
)

mat <- as_type(x, as.character)
expect_error(
  x %row~% mat,
  pattern = "right-hand side must be a numeric matrix of order rankings"
)
expect_error(
  x %col~% mat,
  pattern = "right-hand side must be a numeric matrix of order rankings"
)

enumerate <- enumerate + 6L


