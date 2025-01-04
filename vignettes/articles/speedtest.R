
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


n <- 5e4
x <- rep(paste0(1:50, collapse = ""), n)
p <- "1"
i <- sample(c(-50:-1, 1:50), replace=TRUE, size = n)
bm.stri_locate_ith_vs_all <- bench::mark(
  "stri_locate_ith" = stri_locate_ith_fixed(x, p, i),
  "stringi::stri_locate_all" = stringi::stri_locate_all(x, fixed = p),
  min_iterations = 500,
  check = FALSE
)
summary(bm.stri_locate_ith_vs_all)
autoplot(bm.stri_locate_ith_vs_all)
save(bm.stri_locate_ith_vs_all, file = "bm.stri_locate_ith_vs_all.RData")


n <- 5e4
x <- rep(paste0(1:50, collapse = ""), n)
p <- "1"
i <- sample(c(-50:-1, 1:50), replace=TRUE, size = n)
bm.tinycodet_vs_strex <- bench::mark(
  "tinycodet::stri_locate_ith" = stri_locate_ith_fixed(x, p, i),
  "strex::str_locate_nth" = strex::str_locate_nth(x, stringr::fixed(p), i),
  min_iterations = 500,
  check = FALSE,
  filter_gc = FALSE
)
summary(bm.tinycodet_vs_strex)
autoplot(bm.tinycodet_vs_strex)
save(bm.tinycodet_vs_strex, file = "bm.tinycodet_vs_strex.RData")



n <- 5e4
x <- rep(paste0(1:50, collapse = ""), n)
p <- "1"
i <- sample(c(-50:-1, 1:50), replace=TRUE, size = n)
bm.stri_locate_ith_vs_strex <- bench::mark(
  "stri_locate_ith" = stri_locate_ith_fixed(x, p, i),
  "strex::str_locate_nth" = strex::str_locate_nth(x, stringr::fixed(p), i),
  min_iterations = 500,
  check = FALSE,
  filter_gc = FALSE
)
summary(bm.stri_locate_ith_vs_strex)
autoplot(bm.stri_locate_ith_vs_strex)
save(bm.stri_locate_ith_vs_strex, file = "bm.stri_locate_ith_vs_strex.RData")



tempfun <- function(x, margin) {
  if(margin == 1) {
    for(i in 1:nrow(x)) x[i,] <- sort(x[i,])
    return(x)
  }
  if(margin == 2) {
    for(i in 1:ncol(x)) x[,i] <- sort(x[,i])
    return(x)
  }
}

n <- 2e3
mat <- matrix(sample(seq_len(n^2)), ncol = n)
bm.roworder <- bench::mark(
  tinycodet = mat %row~% mat,
  "base R" = tempfun(mat, 1),
  min_iterations = 250
)
bm.roworder
ggplot2::autoplot(bm.roworder)
save(bm.roworder, file = "bm.roworder.RData")


bm.colorder <- bench::mark(
  tinycodet = mat %col~% mat,
  "base R" = tempfun(mat, 2),
  min_iterations = 250
)
bm.colorder
ggplot2::autoplot(bm.colorder)
save(bm.colorder, file = "bm.colorder.RData")
