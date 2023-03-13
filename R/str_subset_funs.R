#' Sub-string functions
#'
#'@description
#' Fully vectorized sub-string functions. \cr
#' \cr
#' The \code{substr_repl(x, rp, ...)} function
#' replaces a position (range) with string \code{rp}. \cr
#' \cr
#' The \code{substr_chartr(x, old, new, ...)} function
#' transforms the sub-string at a position (range) using \code{chartr(old, new)}. \cr
#' \cr
#' The \code{substr_addin(x, addition, side, ...)} function
#' adds the additional string \code{addition} at the side \code{side} of a position. \cr
#' \cr
#' The \code{substr_extract(x, type, ...)} function
#' extracts the string at, before, or after some position. \cr
#' \cr
#' The \code{substr_arrange(x, arr, ...)} function
#' sorts or reverse the sub-string at a position (range). \cr
#' \cr
#'
#' @param x a string or character vector.
#' @param rp a string, or a character vector of the same length as \code{x},
#' giving the replacing strings.
#' @param loc The result from the \link{stri_locate_ith} function.
#' It does not matter if the result is in the list form (\code{simplify = FALSE}),
#' or in the matrix form (\code{simplify = TRUE}).
#' See \link{stri_locate_ith}. \cr
#' NOTE: you cannot fill in both \code{loc} and \code{start,end},
#' or both \code{loc} and \code{at}. Choose one or the other.
#' @param at an integer, or integer vector of the same length as \code{x}.
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
#'  * \code{type = "at"}: extracts the string part at the position range; \cr
#'  * \code{type = "before"}: extracts the string part before the position range; \cr
#'  * \code{type = "after"}: extracts the string part after the position range. \cr
#' @param arr a single string, giving how the sub-string should be arranged.
#' 3 options available: \cr
#'  * \code{arr = "incr"}: sort the sub-string alphabetically.
#'  * \code{arr = "decr"}: sort the sub-string reverse alphabetically.
#'  * \code{arr = "rev"}: reverse the sub-string.
#' @param fish although \code{tidyoperators} has no dependencies other than \code{stringi},
#' it does allow the internal functions to use the very fast \code{stringfish}
#' functions. To do so, set \code{fish=TRUE};
#' this requires \code{stringfish} to be installed.
#' @param ... only applicable if \code{fish=TRUE};
#' other arguments to be passed to the \code{stringfish} functions.
#'
#'
#' @details
#' These functions
#' serve as a way to provide straight-forward sub-string modification and/or extraction. \cr
#' All substr_ functions internally only use fully vectorized R functions
#' (no loops or apply-like functions). \cr
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
#' substr_repl(x, "??", start=start, end=end)
#' substr_chartr(x, start=start, end=end)
#' substr_addin(x, " ", "after", at=end)
#' substr_addin(x, " ", "before", at=start)
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
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' p <- rep("a|e|i|o|u",2)
#' loc <- stri_locate_ith(x, c(-1, 1), regex=p)
#' substr_extract(x, loc=loc)
#' substr_extract(x, type="before", loc=loc)
#' substr_extract(x, type="after", loc=loc)
#' substr_repl(x, "??", loc=loc)
#' substr_chartr(x, loc=loc)
#' substr_addin(x, " ", "after", loc=loc)
#' substr_addin(x, " ", "before", loc=loc)
#'
#' #############################################################################
#'
#' # ignore case pattern ====
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' # pattern with ignore.case=TRUE:
#' loc <- stri_locate_ith(x, c(1, -1), regex = rep("A|E|I|O|U", 2), case_insensitive=TRUE)
#' substr_extract(x, type="at", loc=loc)
#' substr_extract(x, type="before", loc=loc)
#' substr_extract(x, type="after", loc=loc)
#' substr_repl(x, "??", loc=loc)
#' substr_chartr(x, loc=loc)
#' substr_addin(x, " ", "after", loc=loc)
#' substr_addin(x, " ", "before", loc=loc)
#'
#' #############################################################################
#'
#' # multi-character pattern ====
#'
#' x <- c(paste0(letters[1:13], collapse=""), paste0(letters[14:26], collapse=""))
#' print(x)
#' # multi-character pattern:
#' p <- rep("AB", 2)
#' loc <- stri_locate_ith(x, c(1,-1), regex=p, case_insensitive=TRUE)
#' substr_extract(x, loc=loc)
#' substr_extract(x, type="before", loc=loc)
#' substr_extract(x, type="after", loc=loc)
#' substr_repl(x, "??", loc=loc)
#' substr_chartr(x, loc=loc)
#' substr_addin(x, " ", "after", loc=loc)
#' substr_addin(x, " ", "before", loc=loc)
#'
#' #############################################################################
#'
#' # sorting ====
#'
#' x <- c(paste0(sample(letters), collapse=""), paste0(sample(letters), collapse=""))
#' print(x)
#' substr_arrange(x, "rev", start=c(2,2), end=nchar(x)-1)
#' substr_arrange(x, "incr", start=c(2,2), end=nchar(x)-1)
#' substr_arrange(x, "decr", start=c(2,2), end=nchar(x)-1)
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
  if(is.null(loc)){
    loc <- cbind(start, end)
  }
  if(is.list(loc)) {
    loc <- do.call(rbind, loc)
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
    prepart <- ifelse(
      loc[,1]<=1, "", stringfish::sf_substr(x, start=1, stop=loc[,1]-1, ...)
    )
    postpart <- ifelse(
      loc[,2]>=n, "", stringfish::sf_substr(x, start = loc[,2]+1, stop=n, ...)
    )
    mainpart <- rp
    out <- paste(prepart, mainpart, postpart, sep = "")
  }

  cc <- stats::complete.cases(loc)
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
  if(is.null(loc)){
    loc <- cbind(start, end)
  }
  if(is.list(loc)) {
    loc <- do.call(rbind, loc)
  }
  if(nrow(loc)!=length(x) & nrow(loc)!=1) {
    stop("x and loc/start/end must be have matching dimensions")
  }
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
    prepart <- ifelse(
      loc[,1]<=1, "", stringfish::sf_substr(x, start=1, stop=loc[,1]-1, ...)
    )
    postpart <- ifelse(
      loc[,2] >=n, "", stringfish::sf_substr(x, start = loc[,2]+1, stop=n, ...)
    )
    mainpart <- chartr(
      old=old, new=new, stringfish::sf_substr(x, loc[,1], loc[,2], ...)
    )
    out <- paste(prepart, mainpart, postpart, sep = "")
  }

  cc <- stats::complete.cases(loc)
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
    stop("either loc OR at must be filled in")
  }
  if(is.null(loc)){
    loc <- cbind(at, at)
  }
  if(is.list(loc)) {
    loc <- do.call(rbind, loc)
  }
  if(nrow(loc)!=length(x) & nrow(loc)!=1) {
    stop("x and loc/at must be have matching dimensions")
  }
  if(length(addition)==1){
    addition <- rep(addition, length(x))
  }
  if(length(addition)!=length(x)) {
    stop("`addition` must be the same length as x or else of length 1")
  }
  if(side=="after"){
    prepart.end <- loc[,1]
    postpart.start <- loc[,2] + 1
  }
  if(side=="before"){
    prepart.end <- loc[,1] - 1
    postpart.start <- loc[,2]
  }
  if(!fish) {
    n <- nchar(x)
    prepart <- ifelse(
      loc[,1]<=1, "", substr(x, start=1, stop=prepart.end)
    )
    postpart <- ifelse(
      loc[,2] >=n, "", substr(x, start = postpart.start, stop=n)
    )
    out <- paste(prepart, addition, postpart, sep ="")
  }
  if(fish) {
    n <- stringfish::sf_nchar(x, ...)
    prepart <- ifelse(
      loc[,1]<=1, "", stringfish::sf_substr(x, start=1, stop=prepart.end, ...)
    )
    postpart <- ifelse(
      loc[,2] >=n, "", stringfish::sf_substr(x, start = postpart.start, stop=n, ...)
    )
    out <- paste(prepart, addition, postpart, sep = "")
  }

  cc <- stats::complete.cases(loc)
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
  if(is.null(loc)){
    loc <- cbind(start, end)
  }
  if(is.list(loc)) {
    loc <- do.call(rbind, loc)
  }
  if(nrow(loc)!=length(x) & nrow(loc)!=1) {
    stop("x and loc/start/end must be have matching dimensions")
  }
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
    out <- switch(
      type,
      "at" =  stringfish::sf_substr(x, start=loc[,1], stop=loc[,2], ...),
      "before" = ifelse(loc[,1]<=1, "",  stringfish::sf_substr(x, start=1, stop=loc[,1]-1, ...)),
      "after" = ifelse(loc[,2]>=n, "",  stringfish::sf_substr(x, start = loc[,2]+1, stop=n, ...))
    )
  }

  cc <- stats::complete.cases(loc)
  if(sum(cc)>0){
    out[!cc] <- NA
  }

  return(out)
}

#' @rdname str_subset_funs
#' @export
substr_arrange <- function(
    x, arr="incr", ..., loc=NULL, start=NULL, end=NULL, fish=FALSE
) {
  check.args <- c(!is.null(loc), !is.null(start) & !is.null(end))
  if(sum(check.args)!=1){
    stop("either loc OR start & stop must be filled in")
  }
  if(is.null(loc)){
    loc <- cbind(start, end)
  }
  if(is.list(loc)) {
    loc <- do.call(rbind, loc)
  }
  if(nrow(loc)!=length(x) & nrow(loc)!=1) {
    stop("x and loc/start/end must be have matching dimensions")
  }
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
    prepart <- ifelse(
      loc[,1]<=1, "", stringfish::sf_substr(x, start=1, stop=loc[,1]-1, ...)
    )
    postpart <- ifelse(
      loc[,2]>=n, "", stringfish::sf_substr(x, start = loc[,2]+1, stop=n, ...)
    )
    mainpart <- stringfish::sf_substr(x, start=loc[,1], stop=loc[,2])
  }

  if(arr=="incr") {
    mat <- stringi::stri_split_boundaries(
      mainpart, type = "character", simplify = TRUE
    )
    mat <- matrix(
      mat[order(row(mat), mat, decreasing = FALSE)],
      ncol=ncol(mat),
      byrow=TRUE
    )
    mainpart <- split(mat, rep(1:ncol(mat), each = nrow(mat)))
    mainpart <- do.call(stringi::stri_join, mainpart)
  }
  if(arr=="decr") {
    mat <- stringi::stri_split_boundaries(
      mainpart, type = "character", simplify = TRUE
    )
    mat <- matrix(
      mat[order(row(mat), mat, decreasing = TRUE)],
      ncol=ncol(mat),
      byrow=TRUE
    )
    mainpart <- split(mat, rep(1:ncol(mat), each = nrow(mat)))
    mainpart <- do.call(stringi::stri_join, mainpart)
  }
  if(arr=="rev") {
    mainpart <- stringi::stri_reverse(mainpart)
  }

  out <- paste(prepart, mainpart, postpart, sep ="")

  cc <- stats::complete.cases(loc)
  if(sum(cc)>0){
    out[!cc] <- x[!cc]
  }

  return(out)
}
