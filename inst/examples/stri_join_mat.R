
#############################################################################

# Basic example

x <- matrix(letters[1:25], ncol = 5, byrow = TRUE)
print(x)
stri_join_mat(x, margin = 1)

x <- matrix(letters[1:25], ncol = 5, byrow = FALSE)
print(x)
stri_join_mat(x, margin = 2)


#############################################################################
# sorting characters in strings ====

x <- c(paste(sample(letters), collapse = ""),
       paste(sample(letters), collapse = ""))
print(x)
mat <- strcut_brk(x)
rank <- stringi::stri_rank(as.vector(mat)) |>  matrix(ncol = ncol(mat))
sorted <- mat %row~% rank
sorted[is.na(sorted)] <- ""
print(sorted)
stri_join_mat(sorted, margin = 1)
stri_join_mat(sorted, margin = 2)


#############################################################################

# sorting words ====

x <- c("2nd 3rd 1st", "Goodbye everyone")
print(x)
mat <- strcut_brk(x, "word")
rank <- stringi::stri_rank(as.vector(mat)) |> matrix(ncol = ncol(mat))
sorted <- mat %row~% rank
sorted[is.na(sorted)] <- ""
stri_c_mat(sorted, margin = 1, sep = " ") # <- alias for stri_join_mat
stri_c_mat(sorted, margin = 2, sep = " ")


#############################################################################

# randomly shuffling sentences ====

x <- c("Hello, who are you? Oh, really?! Cool!",
       "I don't care. But I really don't.")
print(x)
mat <- strcut_brk(x, "sentence")
rank <- sample(seq_along(mat)) |> matrix(ncol = ncol(mat))
sorted <- mat %row~% rank
sorted[is.na(sorted)] <- ""
stri_paste_mat(sorted, margin = 1) # <- another alias for stri_join_mat
stri_paste_mat(sorted, margin = 2)

