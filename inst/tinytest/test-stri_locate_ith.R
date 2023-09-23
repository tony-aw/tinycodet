
# positions ====
x <- rep(paste0(0:9, collapse=""), 10)
print(x)
out1 <- stri_locate_ith(x, 1:10, regex = "\\d")
out2 <- stri_locate_ith(x, -1:-10, regex = "\\d")
outcome <- cbind(0:9, out1, out2)
expected <- cbind(0:9, 1:10, 1:10, 10:1, 10:1)
colnames(expected) <- colnames(outcome)
expect_equal(expected, outcome)


# regex ====
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
out1 <- stri_locate_ith(x, c(-1, 1), regex=c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE)
out2 <- stri_locate_ith(x, c(-1, 1), regex=c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE)
expect_equal(substr(x, out1[,1], out1[,2]), c("i", "o"))
expect_equal(substr(x, out2[,1], out2[,2]), c(NA, "o"))

x <- lapply(1:10, function(x)paste0(sample(c(letters, LETTERS)), collapse = ""))
p <- "a|e|i|o|u"
tonyfirst <- stri_locate_ith(x, 1, regex=p, case_insensitive=TRUE)
strifirst <- stringi::stri_locate_first(x, regex=p, case_insensitive=TRUE)
tonylast <- stri_locate_ith(x, -1, regex=p, case_insensitive=TRUE)
strilast <- stringi::stri_locate_last(x, regex=p, case_insensitive=TRUE)
expect_equal(tonyfirst, strifirst)
expect_equal(tonylast, strilast)

x.regex <- c('stringi R', 'R STRINGI', '123')
pattern <- list("R.", '[[:alpha:]]*?', '[a-zC1]', '( R|RE)', 'sTrInG')
expect1 <- expect2 <- expect3 <- list()
out1 <- out2 <- out3 <- list()
for(i in 1:length(pattern)) {
  out1[[i]] <- stri_locate_ith(x.regex, i = 1, regex = pattern[[i]])
  expect1[[i]] <- stringi::stri_locate_first_regex(x.regex, pattern[[i]])
  out2[[i]] <- stri_locate_ith(x.regex, i = -1, regex = pattern[[i]])
  expect2[[i]] <- stringi::stri_locate_last_regex(x.regex, pattern[[i]])
  out3[[i]] <- stri_locate_ith(x.regex, i = c(1, -1, 1), regex = pattern[[i]])
  expect3[[i]] <- rbind(stringi::stri_locate_first_regex(x.regex[1], pattern[[i]]),
                   stringi::stri_locate_last_regex(x.regex[2], pattern[[i]]),
                   stringi::stri_locate_first_regex(x.regex[3], pattern[[i]]))
}
expect_equal(expect1, out1)
expect_equal(expect2, out2)
expect_equal(expect3, out3)


# fixed ====
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
print(x)
p <- rep("ab", 2)
out <- stri_locate_ith(x, c(1, -1), fixed=p)
expect_equal(substr(x, out[,1], out[,2]), c("ab", NA))

x <- lapply(1:10, function(x)paste0(sample(1:10), collapse = ""))
p <- "1"
tonyfirst <- stri_locate_ith(x, 1, fixed=p, case_insensitive=TRUE)
strifirst <- stringi::stri_locate_first(x, fixed=p, case_insensitive=TRUE)
tonylast <- stri_locate_ith(x, -1, fixed=p, case_insensitive=TRUE)
strilast <- stringi::stri_locate_last(x, fixed=p, case_insensitive=TRUE)
expect_equal(tonyfirst, strifirst)
expect_equal(tonylast, strilast)

x.fixed <- c('stringi R', 'R STRINGI', '123')
p.fixed <- c('i', 'R', '0')
expect1 <- expect2 <- expect3 <- list()
out1 <- out2 <- out3 <- list()
for(i in 1:length(pattern)) {
  out1[[i]] <- stri_locate_ith(x.fixed, i = 1, fixed = pattern[[i]])
  expect1[[i]] <- stringi::stri_locate_first_fixed(x.fixed, pattern[[i]])
  out2[[i]] <- stri_locate_ith(x.fixed, i = -1, fixed = pattern[[i]])
  expect2[[i]] <- stringi::stri_locate_last_fixed(x.fixed, pattern[[i]])
  out3[[i]] <- stri_locate_ith(x.fixed, i = c(1, -1, 1), fixed = pattern[[i]])
  expect3[[i]] <- rbind(stringi::stri_locate_first_fixed(x.fixed[1], pattern[[i]]),
                     stringi::stri_locate_last_fixed(x.fixed[2], pattern[[i]]),
                     stringi::stri_locate_first_fixed(x.fixed[3], pattern[[i]]))
}
expect_equal(expect1, out1)
expect_equal(expect2, out2)
expect_equal(expect3, out3)


# coll ====
x <- c('hladn\u00FD', 'hladny')
out <- stri_locate_ith(x, 1, coll='HLADNY', strength=1, locale='sk_SK')
expect_equal(substr(x, out[,1], out[,2]), x)

x.list <- list("a", NA, "ipsum 1234")
p.list <- list("a", NA, "ipsum 1234")
expect1 <- expect2 <- expect3 <- list()
out1 <- out2 <- out3 <- list()
k = 1
for(i in 1:length(x.list)) {
  for(j in 1:length(p.list)) {
    out1[[k]] <- stri_locate_ith(x.list[[i]], i = 1, coll = p.list[[j]])
    expect1[[k]] <- stringi::stri_locate_first_coll(x.list[[i]], p.list[[j]])
    out2[[k]] <- stri_locate_ith(x.list[[i]], i = -1, coll = p.list[[j]])
    expect2[[k]] <- stringi::stri_locate_last_coll(x.list[[i]], p.list[[j]])
    out3[[k]] <- stri_locate_ith(unlist(x.list), i = c(1, -1, 1), coll = p.list[[j]])
    expect3[[k]] <- rbind(
      stringi::stri_locate_first_coll(x.list[[i]][1], p.list[[j]]),
      stringi::stri_locate_last_coll(x.list[[i]][2], p.list[[j]]),
      stringi::stri_locate_first_coll(x.list[[i]][3], p.list[[j]])
    )

    k = k +1
  }
}
expect_equal(expect1, out1)
expect_equal(expect2, out2)



# charclass ====
x.charclass <- c('stRRRingi','R STRINGI', '123')
pattern <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
expect1 <- expect2 <- expect3 <- list()
out1 <- out2 <- out3 <- list()
tempfun <- function(x, i, pattern) {
  out <- stringi::stri_locate_all_charclass(x, pattern)
  if(i == 1) out <- do.call(rbind, lapply(out, \(x)x[1,]))
  if(i == (-1)) out <- do.call(rbind, lapply(out, \(x)x[nrow(x),]))
  return(out)
}
codetools::checkUsage(tempfun)
for(i in 1:length(pattern)) {
  out1[[i]] <- stri_locate_ith(x.charclass, i = 1, charclass = pattern[[i]])
  expect1[[i]] <- tempfun(x.charclass, i = 1, pattern[[i]])
  out2[[i]] <- stri_locate_ith(x.charclass, i = -1, charclass = pattern[[i]])
  expect2[[i]] <- tempfun(x.charclass, i = -1, pattern[[i]])
  out3[[i]] <- stri_locate_ith(x.charclass, i = c(1, -1, 1), charclass = pattern[[i]])
  expect3[[i]] <- rbind(
    tempfun(x.charclass[1], i = 1, pattern[[i]]),
    tempfun(x.charclass[2], i = -1, pattern[[i]]),
    tempfun(x.charclass[3], i = 1, pattern[[i]])
  )
}
expect_equal(expect1, out1)
expect_equal(expect2, out2)
expect_equal(expect3, out3)



# stri_locate_ith (NAs) ====
repNA <- rep(NA, 3)
x <- repNA
expect_equivalent(stri_locate_ith(x, -2, regex="a|e|i|o|u"), cbind(repNA, repNA))


# bad i ====
x <- c("hello", "goodbye")
i <- c(0, NA)
expect_error(
  stri_locate_ith(x, i, regex="a|e|i|o|u"),
  pattern = "`i` is not allowed to be zero or NA"
)
i <- c(-1, -1, 1)
expect_error(
  stri_locate_ith(x, i, regex="a|e|i|o|u"),
  pattern = "`i` must be the same length as `str`, or be a length of 1"
)

# bad pattern ====
x <- c("hello", "goodbye")
i <- 1
expect_error(
  stri_locate_ith(x, i, whoops="a|e|i|o|u"),
  pattern = "you have to specify either `regex`, `fixed`, `coll`, `charclass`"
)
