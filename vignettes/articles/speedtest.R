library(tinycodet)
library(ggplot2)

n <- 1e5
x <- rep("hello", n)
i <- sample(1:3, n, replace = TRUE)
loc <- stri_locate_ith(x, i=i, regex="a|e|i|o|u")
bm.strcut <- bench::mark(
  "strcut_loc" = { strcut_loc(x, loc) },
  "strcut_brk" = { strcut_brk(x, type = "character") },
  "stringi::stri_split_boundaries" = {
    stringi::stri_split_boundaries(x, type="character")
  },
  min_iterations = 500,
  check = FALSE,
  filter_gc = FALSE
)
bm.strcut
autoplot(bm.strcut)
save(bm.strcut, file = "bm.strcut.RData")


n <- 1e5
x <- rep(paste0(1:50, collapse = ""), n)
p <- "\\d"
i <- sample(c(-50:-1, 1:50), replace=TRUE, size = n)
locate_stringi <- function(...) {
  stringi::stri_locate_all(...)
  stringi::stri_count(...)
}
bm.stri_locate_ith <- bench::mark(
  "stri_locate_ith" = { stri_locate_ith_regex(x, p, i) },
  "stringi::(stri_locate_all + stri_count)" = { locate_stringi(str=x, regex = p) },
  min_iterations = 500,
  check = FALSE,
  filter_gc = TRUE
)
bm.stri_locate_ith
autoplot(bm.stri_locate_ith)
save(bm.stri_locate_ith, file = "bm.stri_locate_ith.RData")


mat <- matrix(10000^2, ncol = 10000)
bm.matorder <- bench::mark(
  tinycodet = {mat %row~% mat; mat %col~% mat},
  Rfast = {Rfast::rowSort(mat); Rfast::colSort(mat)},
  min_iterations = 1000
)
bm.matorder
autoplot(bm.matorder)
save(bm.matorder, file = "bm.matorder.RData")
