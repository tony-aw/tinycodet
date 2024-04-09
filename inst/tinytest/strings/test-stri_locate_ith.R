# set-up ===
enumerate <- 0 # to count number of tests performed using iterations in loops
loops <- 0 # to count number of loops

errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}



# regex ====
x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
out1 <- stri_locate_ith(x, c(-1, 1), regex=c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=TRUE)
out2 <- stri_locate_ith(x, c(-1, 1), regex=c("A|E|I|O|U","a|e|i|o|u"), case_insensitive=FALSE)
expect_equal(substr(x, out1[,1], out1[,2]), c("i", "o"))
expect_equal(substr(x, out2[,1], out2[,2]), c(NA, "o"))

x <- lapply(1:10, function(x)paste0(sample(c(letters, LETTERS)), collapse = ""))
p <- "a|e|i|o|u"
loops <- loops + 1
for(bool in c(TRUE, FALSE)) {
  tonyfirst <- stri_locate_ith(x, 1, regex=p, case_insensitive =  bool)
  strifirst <- stringi::stri_locate_first(x, regex=p, case_insensitive =  bool)
  tonylast <- stri_locate_ith(x, -1, regex=p, case_insensitive =  bool)
  strilast <- stringi::stri_locate_last(x, regex=p, case_insensitive =  bool)
  expect_equal(tonyfirst, strifirst) |> errorfun()
  expect_equal(tonylast, strilast) |> errorfun()
  
  tonyfirst <- stri_locate_ith(x, 1, regex=p, opts_regex = stringi::stri_opts_regex(case_insensitive = bool))
  strifirst <- stringi::stri_locate_first(x, regex=p, case_insensitive =  bool)
  tonylast <- stri_locate_ith(x, -1, regex=p, opts_regex = stringi::stri_opts_regex(case_insensitive = bool))
  strilast <- stringi::stri_locate_last(x, regex=p, case_insensitive =  bool)
  expect_equal(tonyfirst, strifirst) |> errorfun()
  expect_equal(tonylast, strilast) |> errorfun()
  
  enumerate <- enumerate + 4
}


x.regex <- c('stringi R', 'R STRINGI', '123')
pattern <- list("R.", '[[:alpha:]]*?', '[a-zC1]', '( R|RE)', 'sTrInG')
expect1 <- expect2 <- expect3 <- list()
out1 <- out2 <- out3 <- list()
loops <- loops + 1
for(i in 1:length(pattern)) {
  out1[[i]] <- stri_locate_ith(x.regex, i = 1, regex = pattern[[i]])
  expect1[[i]] <- stringi::stri_locate_first_regex(x.regex, pattern[[i]])
  out2[[i]] <- stri_locate_ith(x.regex, i = -1, regex = pattern[[i]])
  expect2[[i]] <- stringi::stri_locate_last_regex(x.regex, pattern[[i]])
  out3[[i]] <- stri_locate_ith(x.regex, i = c(1, -1, 1), regex = pattern[[i]])
  expect3[[i]] <- rbind(stringi::stri_locate_first_regex(x.regex[1], pattern[[i]]),
                   stringi::stri_locate_last_regex(x.regex[2], pattern[[i]]),
                   stringi::stri_locate_first_regex(x.regex[3], pattern[[i]]))
  enumerate <- enumerate + 3
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
loops <- loops + 1
for(bool in c(TRUE, FALSE)) {
  tonyfirst <- stri_locate_ith(x, 1, fixed=p, case_insensitive =  bool)
  strifirst <- stringi::stri_locate_first(x, fixed=p, case_insensitive =  bool)
  tonylast <- stri_locate_ith(x, -1, fixed=p, case_insensitive =  bool)
  strilast <- stringi::stri_locate_last(x, fixed=p, case_insensitive =  bool)
  expect_equal(tonyfirst, strifirst) |> errorfun()
  expect_equal(tonylast, strilast) |> errorfun()
  
  tonyfirst <- stri_locate_ith(x, 1, fixed=p, opts_fixed = stringi::stri_opts_fixed(case_insensitive = bool))
  strifirst <- stringi::stri_locate_first(x, fixed=p, case_insensitive =  bool)
  tonylast <- stri_locate_ith(x, -1, fixed=p, opts_fixed = stringi::stri_opts_fixed(case_insensitive = bool))
  strilast <- stringi::stri_locate_last(x, fixed=p, case_insensitive =  bool)
  expect_equal(tonyfirst, strifirst) |> errorfun()
  expect_equal(tonylast, strilast) |> errorfun()
  
  enumerate <- enumerate + 4
}

x.fixed <- c('stringi R', 'R STRINGI', '123')
p.fixed <- c('i', 'R', '0')
expect1 <- expect2 <- expect3 <- list()
out1 <- out2 <- out3 <- list()
loops <- loops + 1
for(i in 1:length(pattern)) {
  out1[[i]] <- stri_locate_ith(x.fixed, i = 1, fixed = pattern[[i]])
  expect1[[i]] <- stringi::stri_locate_first_fixed(x.fixed, pattern[[i]])
  out2[[i]] <- stri_locate_ith(x.fixed, i = -1, fixed = pattern[[i]])
  expect2[[i]] <- stringi::stri_locate_last_fixed(x.fixed, pattern[[i]])
  out3[[i]] <- stri_locate_ith(x.fixed, i = c(1, -1, 1), fixed = pattern[[i]])
  expect3[[i]] <- rbind(stringi::stri_locate_first_fixed(x.fixed[1], pattern[[i]]),
                     stringi::stri_locate_last_fixed(x.fixed[2], pattern[[i]]),
                     stringi::stri_locate_first_fixed(x.fixed[3], pattern[[i]]))
  enumerate <- enumerate + 3
}
expect_equal(expect1, out1)
expect_equal(expect2, out2)
expect_equal(expect3, out3)


# coll ====
x <- c('hladn\u00FD', 'hladny')
out1 <- stri_locate_ith(x, 1, coll='HLADNY', strength=1, locale='sk_SK')
out2 <- stri_locate_ith(x, 1, coll='HLADNY', opts_collator  = stringi::stri_opts_collator(strength=1, locale='sk_SK'))
expect_equal(substr(x, out1[,1], out1[,2]), x)
expect_equal(substr(x, out2[,1], out2[,2]), x)

x.list <- list("a", NA, "ipsum 1234")
p.list <- list("a", NA, "ipsum 1234")
expect1 <- expect2 <- expect3 <- list()
out1 <- out2 <- out3 <- list()
k <- 1
loops <- loops + 1
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

    enumerate <- enumerate + 3
  }
}
expect_equal(expect1, out1)
expect_equal(expect2, out2)



# charclass - merge = TRUE (default) ====
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
loops <- loops + 1
for(i in 1:length(pattern)) {
  out1[[i]] <- stri_locate_ith(x.charclass, i = 1, charclass = pattern[i])
  expect1[[i]] <- tempfun(x.charclass, i = 1, pattern[i])
  out2[[i]] <- stri_locate_ith(x.charclass, i = -1, charclass = pattern[i])
  expect2[[i]] <- tempfun(x.charclass, i = -1, pattern[i])
  out3[[i]] <- stri_locate_ith(x.charclass, i = c(1, -1, 1), charclass = pattern[i])
  expect3[[i]] <- rbind(
    tempfun(x.charclass[1], i = 1, pattern[i]),
    tempfun(x.charclass[2], i = -1, pattern[i]),
    tempfun(x.charclass[3], i = 1, pattern[i])
  )
  enumerate <- enumerate + 3
}
expect_equal(expect1, out1)
expect_equal(expect2, out2)
expect_equal(expect3, out3)

x.charclass <- c('stRRRingi','R STRINGI', '123')
pattern <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
tempfun <- function(x, i, pattern) {
  out <- stringi::stri_locate_all_charclass(x, pattern)
  if(i == 1) out <- do.call(rbind, lapply(out, \(x)x[1,]))
  if(i == (-1)) out <- do.call(rbind, lapply(out, \(x)x[nrow(x),]))
  return(out)
}
expect_equal(
  stri_locate_ith(x.charclass, i = 1, charclass = pattern),
  tempfun(x.charclass, i = 1, pattern)
)
expect_equal(
  stri_locate_ith(x.charclass, i = -1, charclass = pattern),
  tempfun(x.charclass, i = -1, pattern)
)
expect_equal(
  stri_locate_ith(x.charclass, i = c(1, -1, 1), charclass = pattern),
  rbind(
    tempfun(x.charclass[1], i = 1, pattern[1]),
    tempfun(x.charclass[2], i = -1, pattern[2]),
    tempfun(x.charclass[3], i = 1, pattern[3])
  )
)


# charclass - merge = FALSE ====
x.charclass <- c('stRRRingi','R STRINGI', '123')
pattern <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
expect1 <- expect2 <- expect3 <- list()
out1 <- out2 <- out3 <- list()
loops <- loops + 1
for(i in 1:length(pattern)) {
  out1[[i]] <- stri_locate_ith(x.charclass, i = 1, charclass = pattern[[i]], merge = FALSE)
  expect1[[i]] <- stringi::stri_locate_first_charclass(x.charclass, pattern[[i]])
  out2[[i]] <- stri_locate_ith(x.charclass, i = -1, charclass = pattern[[i]], merge = FALSE)
  expect2[[i]] <- stringi::stri_locate_last_charclass(x.charclass, pattern[[i]])
  out3[[i]] <- stri_locate_ith(x.charclass, i = c(1, -1, 1), charclass = pattern[[i]], merge = FALSE)
  expect3[[i]] <- rbind(stringi::stri_locate_first_charclass(x.charclass[1], pattern[[i]]),
                        stringi::stri_locate_last_charclass(x.charclass[2], pattern[[i]]),
                        stringi::stri_locate_first_charclass(x.charclass[3], pattern[[i]]))
  enumerate <- enumerate + 3
}
expect_equal(expect1, out1)
expect_equal(expect2, out2)
expect_equal(expect3, out3)

x.charclass <- c('stRRRingi','R STRINGI', '123')
pattern <- c('\\p{Ll}', '\\p{Lu}', '\\p{Zs}')
expect_equal(
  stri_locate_ith(x.charclass, i = 1, charclass = pattern, merge = FALSE),
  stringi::stri_locate_first_charclass(x.charclass, pattern)
)
expect_equal(
  stri_locate_ith(x.charclass, i = -1, charclass = pattern, merge = FALSE),
  stringi::stri_locate_last_charclass(x.charclass, pattern)
)
expect_equal(
  stri_locate_ith(x.charclass, i = c(1, -1, 1), charclass = pattern, merge = FALSE),
  rbind(
    stringi::stri_locate_first_charclass(x.charclass[1], pattern[1]),
    stringi::stri_locate_last_charclass(x.charclass[2], pattern[2]),
    stringi::stri_locate_first_charclass(x.charclass[3], pattern[3])
  )
)


# boundaries ====
test <- c(
  paste0("The\u00a0above-mentioned    features are very useful. ",
         "Spam, spam, eggs, bacon, and spam. 123 456 789"),
  "good morning, good evening, and good night"
)
pattern <- c("character", "word", "sentence")
out1 <- expect1 <- out2 <- expect2 <- out3 <- expect3 <- list()
loops <- loops + 1
for(i in 1:length(pattern)) {
  out1[[i]] <- stri_locate_ith_boundaries(test, i = 1, type = pattern[i])
  expect1[[i]] <- stringi::stri_locate_first_boundaries(test, type = pattern[i])
  out2[[i]] <- stri_locate_ith_boundaries(test, i = -1, type = pattern[i])
  expect2[[i]] <- stringi::stri_locate_last_boundaries(test, type = pattern[i])
  out3[[i]] <- stri_locate_ith_boundaries(test, i = c(1, -1), type = pattern[i])
  expect3[[i]] <- rbind(
    stringi::stri_locate_first_boundaries(test[1], type = pattern[i]),
    stringi::stri_locate_last_boundaries(test[2], type = pattern[i])
  )

  enumerate <- enumerate + 3
}
expect_equal(expect1, out1)
expect_equal(expect2, out2)
expect_equal(expect3, out3)

