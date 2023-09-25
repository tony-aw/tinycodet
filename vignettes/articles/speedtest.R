library(tinycodet)
library(rbenchmark)

n <- 1e5
x <- rep("hello", n)
i <- sample(1:3, n, replace = TRUE)
loc <- stri_locate_ith(x, i=i, regex="a|e|i|o|u")
benchmark.strcut <- benchmark(
  "strcut_loc" = { strcut_loc(x, loc) },
  "strcut_brk" = { strcut_brk(x, brk="chr") },
  "stringi::stri_split_boundaries" = {
    stringi::stri_split_boundaries(x, type="character")
  },
  replications = 500,
  order = NULL,
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self")
)

n <- 1e5
x <- rep(paste0(1:100, collapse=""), n)
p <- "\\d"
i <- sample(c(-50:-1, 1:50), replace=TRUE, size = n)
locate_stringi <- function(...) {
  stringi::stri_locate_all(...)
  stringi::stri_count(...)
}
benchmark.stri_locate_ith <- benchmark(
  "stri_locate_ith" = { stri_locate_ith(x, i, regex = p) },
  "stringi::stri_locate_all+count" = { locate_stringi(str=x, regex = p) },
  replications = 500,
  order = NULL,
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self")
)

save(benchmark.strcut, benchmark.stri_locate_ith, file = "speedtest.RData")
