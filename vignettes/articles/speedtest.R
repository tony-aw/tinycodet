
library(tinycodet)
library(ggplot2)
loadNamespace("bench")

n <- 1e5
x <- rep("hello", n)
i <- sample(1:3, n, replace = TRUE)
loc <- stri_locate_ith(x, i=i, regex="a|e|i|o|u")
bm.strcut <- bench::mark(
  "strcut_loc" = { strcut_loc(x, loc) },
  "strcut_brk" = { strcut_brk(x, type = "", tolist = TRUE) },
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
  min_iterations = 200,
  check = FALSE,
  filter_gc = FALSE
)
summary(bm.stri_locate_ith)
autoplot(bm.stri_locate_ith)
save(bm.stri_locate_ith, file = "bm.stri_locate_ith.RData")


mat <- matrix(sample(1:1e6), ncol = 1e3)
bm.roworder <- bench::mark(
  tinycodet = mat %row~% mat,
  "base R" = do.call(rbind, apply(mat, 1, sort, simplify = FALSE)),
  min_iterations = 250
)
bm.roworder
ggplot2::autoplot(bm.roworder)
save(bm.roworder, file = "bm.roworder.RData")


bm.colorder <- bench::mark(
  tinycodet = mat %col~% mat,
  "base R" = do.call(cbind, apply(mat, 2, sort, simplify = FALSE)),
  min_iterations = 250
)
bm.colorder
ggplot2::autoplot(bm.colorder)
save(bm.colorder, file = "bm.colorder.RData")
