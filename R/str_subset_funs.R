#' Substr - functions
#'
#'@description
#' Fully vectorized sub-string functions. \cr
#' These functions extract, replace, add-in, transform, or re-arrange,
#' the \eqn{i^{th}} pattern occurrence or position range. \cr
#' \cr
#' The \code{substr_repl(x, rp, ...)} function
#' replaces a position (range) with string \code{rp}. \cr
#' \cr
#' The \code{substr_chartr(x, old, new, ...)} function
#' transforms the sub-string at a position (range) using \code{chartr(old, new)}. \cr
#' \cr
#' The \code{substr_addin(x, addition, side, ...)} function
#' adds the additional string \code{addition} at the side
#' (specified by argument \code{side}) of a position. \cr
#' \cr
#' The \code{substr_extract(x, type, ...)} function
#' extracts the string at, before, or after some position. \cr
#' \cr
#' The \code{substr_arrange(x, arr, ...)} function
#' sorts (alphabetically or reverse-alphabetically)
#' or reverses the sub-string at a position (range).
#' \cr
#'
#' @param x a string or character vector.
#' @param rp a string, or a character vector of the same length as \code{x},
#' giving the replacing strings.
#' @param loc The result from the \link{stri_locate_ith} function. \cr
#' NOTE: you cannot fill in both \code{loc} and \code{start,end},
#' or both \code{loc} and \code{at}. Choose one or the other.
#' @param at an integer, or integer vector of the same length as \code{x},
#' giving the position after or before which the string is to be added.
#' @param start,end integers, or integer vectors of the same length as \code{x},
#' giving the start and end position of the range to be modified.
#' @param old,new see \link[base]{chartr}.
#' Defaults to \code{old="a-zA-Z", new="A-Za-z"},
#' which means upper case characters will be transformed to lower case characters,
#' and vice-versa.
#' @param side which side of the position to add in the string.
#' Either \code{"before"} or \code{"after"}.
#' @param addition a string, or a character vector of the same length as \code{x},
#' giving the string(s) to add-in.
#' @param type a single string, giving the part of the string to extract.
#' 3 options available: \cr
#'  * \code{type = "at"}: extracts the string part at the position range.
#'  * \code{type = "before"}: extracts the string part before the position range.
#'  * \code{type = "after"}: extracts the string part after the position range.
#' @param arr a single string, giving how the sub-string should be arranged.
#' 3 options available: \cr
#'  * \code{arr = "incr"}: sort the sub-string alphabetically.
#'  * \code{arr = "decr"}: sort the sub-string reverse alphabetically.
#'  * \code{arr = "rev"}: reverse the sub-string.
#'  * \code{arr = "rand"}: randomly shuffles the sub-string.
#' @param opts_collator as in \link[stringi]{stri_rank}.
#' Only used when \code{arr = "incr"} or \code{arr = "decr"}.
#' @param fish although \code{tinyoperators} has no dependencies other than \code{stringi},
#' it does allow the internal functions to use the multi-threadable \code{stringfish}
#' functions. To do so, set \code{fish=TRUE};
#' this requires \code{stringfish} to be installed.
#' @param ... only applicable if \code{fish=TRUE};
#' other arguments to be passed to the \code{stringfish} functions.
#'
#'
#'
#' @returns
#' A modified character vector.
#' If no match is found in a certain string of character vector \code{x},
#' the unmodified string is returned.
#' The exception is for the \code{substr_extract()} function:
#' in this function, non-matches return \code{NA}.
#'
#'
#' @examples
#'
#' # numerical substr ====
#'
#' x <- rep("12345678910", 2)
#' start=c(1, 2); end=c(3,4)
#' substr_extract(x, start=start, end=end)
#' substr_extract(x, type="before", start=start, end=end)
#' substr_extract(x, type="after", start=start, end=end)
#' substr_repl(x, c("??", "!!"), start=start, end=end)
#' substr_chartr(x, start=start, end=end)
#' substr_addin(x, c(" ", "~"), "after", at=end)
#' substr_addin(x, c(" ", "~"), "before", at=start)
#' substr_arrange(x, start=start, end=end)
#' substr_arrange(x, "decr", start=start, end=end)
#' substr_arrange(x, "rev", start=start, end=end)
#' substr_arrange(x, "rand", start=start, end=end)
#'
#' start=10; end=11
#' substr_extract(x, start=start, end=end)
#' substr_extract(x, type="before", start=start, end=end)
#' substr_extract(x, type="after", start=start, end=end)
#' substr_repl(x, "??", start=start, end=end)
#' substr_chartr(x, start=start, end=end)
#' substr_addin(x, " ", "after", at=end)
#' substr_addin(x, " ", "before", at=start)
#'
#' start=5; end=6
#' substr_extract(x, start=start, end=end)
#' substr_extract(x, type="before", start=start, end=end)
#' substr_extract(x, type="after", start=start, end=end)
#' substr_repl(x, "??", start=start, end=end)
#' substr_chartr(x, start=start, end=end)
#' substr_addin(x, " ", "after", at=end)
#' substr_addin(x, " ", "before", at=start)
#'
#'
#' #############################################################################
#'
#' # simple pattern ====
#'
#' x <- c("goodGOODGoodgOOd", "goodGOODGoodgOOd", paste0(letters[1:13], collapse=""))
#' print(x)
#' loc <- stri_locate_ith(
#'   # locate second-last occurrence of "good" of each string in x:
#'   x, -2, regex="good", case_insensitive=TRUE
#' )
#' substr_extract(x, loc=loc) # extract second-last "good"
#' substr_repl(x, c("??", "!!", " "), loc=loc) # replace second-last "good"
#' substr_chartr(x, loc=loc) # switch upper/lower case of second-last "good"
#' substr_addin(x, c(" ", "~", " "), "after", loc=loc) # add white space after second-last "good"
#' substr_addin(x, c(" ", "~", " "), "before", loc=loc) # add white space before second-last "good"
#' substr_arrange(x, loc=loc) # sort second-last "good"
#' substr_arrange(x, "decr", loc=loc) # reverse-sort second-last "good"
#' substr_arrange(x, "rev", loc=loc) # reverse second-last "good"
#' substr_arrange(x, "rand", loc=loc) # randomly shuffles "good"
#'
#'
#'





#' @rdname str_subset_funs
#' @export
substr_repl <- function(
    x, rp, ..., loc=NULL, start=NULL, end=NULL, fish=FALSE
) {
  check.args <- c(!is.null(loc), !is.null(start) & !is.null(end))
  if(sum(check.args)!=1){
    stop("either loc OR start & stop must be filled in")
  }
  if(!isTRUE(is.vector(x) && is.atomic(x) && is.character(x))) {
    stop("`x` must be a character vector.")
  }
  if(is.null(loc)){
    loc <- cbind(start, end)
  }
  if(nrow(loc)!=length(x) & nrow(loc)!=1) {
    stop("x and loc/start/end must be have matching dimensions")
  }
  if(length(rp)==1) {
    rp <-rep(rp, length(x))
  }
  if(length(rp)!=length(x)) {
    stop("`rp` must be of the same length as x, or else of length 1")
  }

  cc <- stats::complete.cases(loc)

  if(!fish){
    n <- nchar(x)
    prepart <- ifelse(
      loc[,1]<=1, "", substr(x, start=1, stop=loc[,1]-1)
    )
    postpart <- ifelse(
      loc[,2]>=n, "", substr(x, start = loc[,2]+1, stop=n)
    )
    mainpart <- rp
    out <- paste(prepart, mainpart, postpart, sep ="")
  }
  if(fish) {
    n <- stringfish::sf_nchar(x, ...)
    prepart <- postpart <- mainpart <- character(nrow(loc))
    prepart <- ifelse(
      loc[cc,1]<=1, "", stringfish::sf_substr(x[cc], 1, loc[cc,1]-1, ...)
    )
    postpart <- ifelse(
      loc[cc,2]>=n, "", stringfish::sf_substr(x[cc], loc[cc,2]+1, n[cc], ...)
    )
    mainpart[cc] <- rp[cc]
    out <- paste(prepart, mainpart, postpart, sep = "")
  }

  if(sum(cc)>0){
    out[!cc] <- x[!cc]
  }

  return(out)
}

#' @rdname str_subset_funs
#' @export
substr_chartr <- function(
    x, old="a-zA-Z", new="A-Za-z", ..., loc=NULL, start=NULL, end=NULL, fish=FALSE
) {
  check.args <- c(!is.null(loc), !is.null(start) & !is.null(end))
  if(sum(check.args)!=1){
    stop("either loc OR start & stop must be filled in")
  }
  if(!isTRUE(is.vector(x) && is.atomic(x) && is.character(x))) {
    stop("`x` must be a character vector.")
  }
  if(is.null(loc)){
    loc <- cbind(start, end)
  }
  if(nrow(loc)!=length(x) & nrow(loc)!=1) {
    stop("x and loc/start/end must be have matching dimensions")
  }

  cc <- stats::complete.cases(loc)

  if(!fish) {
    n <- nchar(x)
    prepart <- ifelse(
      loc[,1]<=1, "", substr(x, start=1, stop=loc[,1]-1)
    )
    postpart <- ifelse(
      loc[,2]>=n, "", substr(x, start = loc[,2]+1, stop=n)
    )
    mainpart <- chartr(old=old, new=new, substr(x, loc[,1], loc[,2]))
    out <- paste(prepart, mainpart, postpart, sep ="")
  }
  if(fish) {
    n <- stringfish::sf_nchar(x, ...)
    prepart <- postpart <- mainpart <- character(nrow(loc))
    prepart[cc] <- ifelse(
      loc[cc,1]<=1, "", stringfish::sf_substr(x[cc], 1, loc[cc,1]-1, ...)
    )
    postpart[cc] <- ifelse(
      loc[cc,2] >= n[cc], "", stringfish::sf_substr(x[cc], loc[cc,2]+1, n[cc], ...)
    )
    mainpart[cc] <- chartr(
      old=old, new=new, stringfish::sf_substr(x[cc], loc[cc,1], loc[cc,2], ...)
    )
    out <- paste(prepart, mainpart, postpart, sep = "")
  }

  if(sum(cc)>0){
    out[!cc] <- x[!cc]
  }

  return(out)
}

#' @rdname str_subset_funs
#' @export
substr_addin <- function(
    x, addition, side="after", ..., loc=NULL, at=NULL, fish=FALSE
) {
  check.args <- c(!is.null(loc), !is.null(at))
  if(sum(check.args)!=1) {
    stop("either `loc` OR `at` must be filled in")
  }
  if(!isTRUE(is.vector(x) && is.atomic(x) && is.character(x))) {
    stop("`x` must be a character vector.")
  }
  if(!side %in% c("before", "after")) {
    stop("`side` must be either 'before' or 'after'")
  }
  if(is.null(loc)){
    loc <- cbind(at, at)
  }
  if(nrow(loc)!=length(x) & nrow(loc)!=1) {
    stop("`x` and `loc`/`at` must be have matching dimensions")
  }
  if(length(addition)==1){
    addition <- rep(addition, length(x))
  }
  if(length(addition)!=length(x)) {
    stop("`addition` must be the same length as `x` or else of length 1")
  }

  cc <- stats::complete.cases(loc)

  if(!fish) {
    n <- nchar(x)
    prepart <- ifelse(
      loc[,1]<=1, "", substr(x, start=1, stop=loc[,1]-1)
    )
    postpart <- ifelse(
      loc[,2] >=n, "", substr(x, start = loc[,2]+1, stop=n)
    )
    mainpart <- substr(x, start=loc[,1], stop=loc[,2])
  }
  if(fish) {
    prepart <- postpart <- mainpart <- character(nrow(loc))
    n <- stringfish::sf_nchar(x, ...)
    prepart[cc] <- ifelse(
      loc[cc,1]<=1, "", stringfish::sf_substr(x[cc], 1, loc[cc, 1]-1, ...)
    )
    postpart[cc] <- ifelse(
      loc[cc,2] >= n[cc], "", stringfish::sf_substr(x[cc], loc[cc,2]+1, n[cc], ...)
    )
    mainpart[cc] <- stringfish::sf_substr(x[cc], start=loc[cc,1], stop=loc[cc,2])
  }
  if(side=="after") {
    out <- paste(prepart, mainpart, addition, postpart, sep ="")
  }
  if(side=="before") {
    out <- paste(prepart, addition, mainpart, postpart, sep ="")
  }

  if(sum(cc)>0){
    out[!cc] <- x[!cc]
  }

  return(out)
}

#' @rdname str_subset_funs
#' @export
substr_extract <- function(
    x, type="at", ..., loc=NULL, start=NULL, end=NULL, fish=FALSE
) {
  check.args <- c(!is.null(loc), !is.null(start) & !is.null(end))
  if(sum(check.args)!=1){
    stop("either loc OR start & stop must be filled in")
  }
  if(!isTRUE(is.vector(x) && is.atomic(x) && is.character(x))) {
    stop("`x` must be a character vector.")
  }
  if(is.null(loc)){
    loc <- cbind(start, end)
  }
  if(nrow(loc)!=length(x) & nrow(loc)!=1) {
    stop("x and loc/start/end must be have matching dimensions")
  }
  cc <- stats::complete.cases(loc)
  if(!fish) {
    n <- nchar(x, ...)
    out <- switch(
      type,
      "at" = substr(x, start=loc[,1], stop=loc[,2]),
      "before" = ifelse(loc[,1]<=1, "", substr(x, start=1, stop=loc[,1]-1, ...)),
      "after" = ifelse(loc[,2]>=n, "", substr(x, start = loc[,2]+1, stop=n, ...))
    )
  }
  if(fish) {
    n <- stringfish::sf_nchar(x, ...)
    out <- character(nrow(loc))
    out[cc] <- switch(
      type,
      "at" =  stringfish::sf_substr(x[cc], start=loc[cc,1], stop=loc[cc,2], ...),
      "before" = ifelse(loc[cc,1] <= 1, "",  stringfish::sf_substr(x[cc], 1, loc[cc,1]-1, ...)),
      "after" = ifelse(loc[cc,2] >= n[cc], "",  stringfish::sf_substr(x[cc], loc[cc,2]+1, n[cc], ...))
    )
  }

  if(sum(cc)>0){
    out[!cc] <- NA
  }

  return(out)
}

#' @rdname str_subset_funs
#' @export
substr_arrange <- function(
    x, arr="incr", ..., loc=NULL, start=NULL, end=NULL, opts_collator = NULL, fish=FALSE
) {
  check.args <- c(!is.null(loc), !is.null(start) & !is.null(end))
  if(sum(check.args)!=1){
    stop("either loc OR start & stop must be filled in")
  }
  if(!isTRUE(is.vector(x) && is.atomic(x) && is.character(x))) {
    stop("`x` must be a character vector.")
  }
  if(!arr %in% c("incr", "decr", "rev", "rand")) {
    stop("`arr` must be 'incr', 'decr', 'rev', or 'rand'")
  }
  if(is.null(loc)){
    loc <- cbind(start, end)
  }
  if(nrow(loc)!=length(x) & nrow(loc)!=1) {
    stop("x and loc/start/end must be have matching dimensions")
  }

  cc <- stats::complete.cases(loc)

  if(!fish){
    n <- nchar(x)
    prepart <- ifelse(
      loc[,1]<=1, "", substr(x, start=1, stop=loc[,1]-1)
    )
    postpart <- ifelse(
      loc[,2]>=n, "", substr(x, start = loc[,2]+1, stop=n)
    )
    mainpart <- substr(x, loc[,1], loc[,2])
  }
  if(fish) {
    n <- stringfish::sf_nchar(x, ...)
    prepart <- postpart <- mainpart <- character(nrow(loc))
    prepart[cc] <- ifelse(
      loc[cc,1]<=1, "", stringfish::sf_substr(x[cc], 1, loc[cc,1]-1, ...)
    )
    postpart[cc] <- ifelse(
      loc[cc,2] >= n[cc], "", stringfish::sf_substr(x[cc], loc[cc,2]+1, n[cc], ...)
    )
    mainpart[cc] <- stringfish::sf_substr(x[cc], loc[cc,1], loc[cc,2])
  }

  if(arr=="incr") {
    mat <- stringi::stri_split_boundaries(
      mainpart, type = "character", simplify = TRUE
    )
    rank <- stringi::stri_rank(as.vector(mat), opts_collator = opts_collator)
    rank <- matrix(rank, ncol=ncol(mat))
    mat <- matrix(
      mat[order(row(mat), rank, decreasing = FALSE)],
      ncol=ncol(mat),
      byrow=TRUE
    )
    mainpart <- mat |> as.data.frame() |> as.list()
    mainpart <- do.call(stringi::stri_join, mainpart)
  }

  if(arr=="decr") {
    mat <- stringi::stri_split_boundaries(
      mainpart, type = "character", simplify = TRUE
    )
    rank <- stringi::stri_rank(as.vector(mat), opts_collator = opts_collator)
    rank <- matrix(rank, ncol=ncol(mat))
    mat <- matrix(
      mat[order(-row(mat), rank, decreasing = TRUE)],
      ncol=ncol(mat),
      byrow=TRUE
    )
    mainpart <- mat |> as.data.frame() |> as.list()
    mainpart <- do.call(stringi::stri_join, mainpart)
  }

  if(arr=="rev") {
    mainpart <- stringi::stri_reverse(mainpart)
  }

  if(arr=="rand") {
    mainpart <- stringi::stri_rand_shuffle(mainpart)
  }

  out <- paste(prepart, mainpart, postpart, sep ="")

  if(sum(cc)>0){
    out[!cc] <- x[!cc]
  }

  return(out)
}

