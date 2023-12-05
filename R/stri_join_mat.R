#' Concatenate Character Matrix Row-wise or Column-wise
#'
#' @description
#'
#'
#' The \code{stri_join_mat()} function
#' (and their aliases \code{stri_c_mat} and \code{stri_paste_mat})
#' perform row-wise (\code{margin = 1}; the default) or
#' column-wise (\code{margin = 2}) joining of a matrix of strings,
#' thereby transforming a matrix of strings into a vector of strings.
#'
#'
#' @param mat a matrix of strings
#' @param margin the margin over which the strings must be joined.
#'  * If \code{margin = 1}, the elements within each row of matrix \code{mat}
#' are joined into a single string.
#' Thus if the matrix has 10 rows, it returns a vector of 10 strings.
#'  * If \code{margin = 2}, the elements within each column of matrix \code{mat}
#' are joined into a single string.
#' Thus if the matrix has 10 columns, it returns a vector of 10 strings. \cr
#' @param sep,collapse as in \link[stringi]{stri_join}.
#'
#'
#' @returns
#'
#' The \code{stri_join_mat()} function, and its aliases, return a vector of strings.
#'
#' @seealso [tinycodet_strings()]
#'
#' @examples
#'
#' #############################################################################
#'
#' # Basic example
#'
#' x <- matrix(letters[1:25], ncol = 5, byrow = TRUE)
#' print(x)
#' stri_join_mat(x, margin = 1)
#'
#' x <- matrix(letters[1:25], ncol = 5, byrow = FALSE)
#' print(x)
#' stri_join_mat(x, margin = 2)
#'
#'
#' #############################################################################
#' # sorting characters in strings ====
#'
#' x <- c(paste(sample(letters), collapse = ""), paste(sample(letters), collapse = ""))
#' print(x)
#' mat <- strcut_brk(x)
#' rank <- stringi::stri_rank(as.vector(mat)) |>  matrix(ncol=ncol(mat))
#' sorted <- mat %row~% rank
#' sorted[is.na(sorted)] <- ""
#' print(sorted)
#' stri_join_mat(sorted, margin = 1)
#' stri_join_mat(sorted, margin = 2)
#'
#'
#' #############################################################################
#'
#' # sorting words ====
#'
#' x <- c("2nd 3rd 1st", "Goodbye everyone")
#' print(x)
#' mat <- strcut_brk(x, "word")
#' rank <- stringi::stri_rank(as.vector(mat)) |> matrix(ncol=ncol(mat))
#' sorted <- mat %row~% rank
#' sorted[is.na(sorted)] <- ""
#' stri_c_mat(sorted, margin = 1, sep = " ") # <- alias for stri_join_mat
#' stri_c_mat(sorted, margin = 2, sep = " ")
#'
#'
#' #############################################################################
#'
#' # randomly shuffling sentences ====
#'
#' x <- c("Hello, who are you? Oh, really?! Cool!", "I don't care. But I really don't.")
#' print(x)
#' mat <- strcut_brk(x, "sentence")
#' rank <- sample(1:length(mat)) |> matrix(ncol = ncol(mat))
#' sorted <- mat %row~% rank
#' sorted[is.na(sorted)] <- ""
#' stri_paste_mat(sorted, margin = 1) # <- another alias for stri_join_mat
#' stri_paste_mat(sorted, margin = 2)



#' @family join_mat
#' @rdname stri_join_mat
#' @export
stri_join_mat <- function(mat, margin = 1, sep = "", collapse = NULL) {
  if(margin==1) {
    out <- t(mat) |> as.data.frame()
    return(
      stringi::stri_join_list(as.list(out), sep = sep, collapse = collapse)
    )
  }
  if(margin==2) {
    out <- mat |> as.data.frame()
    return(
      stringi::stri_join_list(as.list(out), sep = sep, collapse = collapse)
    )
  } else {
    stop("`margin` must be either 1 or 2")
  }
}

#' @rdname stri_join_mat
#' @export
stri_c_mat <- stri_join_mat

#' @rdname stri_join_mat
#' @export
stri_paste_mat <- stri_join_mat

