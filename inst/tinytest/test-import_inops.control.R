

# alias import.inops ====
temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri., overwrite = NA, inherits = FALSE)
}
expect_error(
  temp.fun(),
  pattern = "`overwrite` must be either `TRUE` or `FALSE`"
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri., overwrite=c(TRUE, FALSE), inherits = FALSE)
}
expect_error(
  temp.fun(),
  pattern = "`overwrite` must be either `TRUE` or `FALSE`"
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri., overwrite = TRUE, inherits = NA)
}
expect_error(
  temp.fun(),
  pattern = "`inherits` must be either `TRUE` or `FALSE`"
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri., overwrite = TRUE, inherits = c(TRUE, FALSE))
}
expect_error(
  temp.fun(),
  pattern = "`inherits` must be either `TRUE` or `FALSE`"
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri., include.only="%stri+%", exclude = "%stri*%")
}
expect_error(
  temp.fun(),
  pattern = "canntot specify both `exclude` and `include.only`"
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri., exclude = "")
}
expect_error(
  temp.fun(),
  pattern = "`exclude` must be a character vector of unique function names"
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri., include.only="")
}
expect_error(
  temp.fun(),
  pattern = "`include.only` must be a character vector of unique function names"
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri., exclude = c("%stri+%", "%stri+%"))
}
expect_error(
  temp.fun(),
  pattern = "`exclude` must be a character vector of unique function names"
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri., include.only = c("%stri+%", "%stri+%"))
}
expect_error(
  temp.fun(),
  pattern = "`include.only` must be a character vector of unique function names"
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri., exclude = 1)
}
expect_error(
  temp.fun(),
  pattern = "`exclude` must be a character vector of unique function names"
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri., include.only = 2)
}
expect_error(
  temp.fun(),
  pattern = "`include.only` must be a character vector of unique function names"
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri.)
  import_inops(expose = stri., overwrite = FALSE, inherits=FALSE)
}
expect_error(
  temp.fun(),
  pattern = "ALL prepared infix operators already exist in the current environment"
)

temp.fun <- function() {
  import_as(~stri., "stringi")
  import_inops(expose = stri.)
  import_inops(expose = stri., overwrite = FALSE, inherits=TRUE)
}
expect_error(
  temp.fun(),
  pattern = "ALL prepared infix operators already exist in the current environment"
)

temp.fun <- function() {
  import_as(~stri., "stringi") |> suppressMessages()
  import_inops(expose = stri., exclude = "%foo%")
}
expect_warning(
  temp.fun(),
  pattern = "The following exclusions are not present, and are ignored:"
)

temp.fun <- function() {
  import_as(~stri., "stringi") |> suppressMessages()
  import_inops(expose = stri., include.only = "%foo%")
}
expect_warning(
  temp.fun(),
  pattern = "The following inclusions are not present, and are ignored:"
)

# pkgs import.inops ====

temp.fun <- function(){
  import_inops("stringi", overwrite = NA, inherits = FALSE)
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`overwrite` must be either `TRUE` or `FALSE`"
)

temp.fun <- function(){
  import_inops("stringi", overwrite = c(TRUE, FALSE), inherits = FALSE)
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`overwrite` must be either `TRUE` or `FALSE`"
)

temp.fun <- function(){
  import_inops("stringi", overwrite = TRUE, inherits = NA)
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`inherits` must be either `TRUE` or `FALSE`"
)

temp.fun <- function(){
  import_inops("stringi", overwrite = TRUE, inherits = c(TRUE, FALSE))
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`inherits` must be either `TRUE` or `FALSE`"
)

temp.fun <- function(){
  import_inops("stringi", include.only="%stri+%", exclude = "%stri*%")
  ls()
}
expect_error(
  temp.fun(),
  pattern = "canntot specify both `exclude` and `include.only`"
)

temp.fun <- function(){
  import_inops("stringi", exclude= "")
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`exclude` must be a character vector of unique function names"
)

temp.fun <- function(){
  import_inops("stringi", include.only = "")
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`include.only` must be a character vector of unique function names"
)

temp.fun <- function(){
  import_inops("stringi", exclude = c("%stri+%", "%stri+%"))
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`exclude` must be a character vector of unique function names"
)

temp.fun <- function(){
  import_inops("stringi", include.only = c("%stri+%", "%stri+%"))
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`include.only` must be a character vector of unique function names"
)

temp.fun <- function(){
  import_inops("stringi", exclude = 1)
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`exclude` must be a character vector of unique function names"
)

temp.fun <- function(){
  import_inops("stringi", include.only = 1)
  ls()
}
expect_error(
  temp.fun(),
  pattern = "`include.only` must be a character vector of unique function names"
)


temp.fun <- function(){
  suppressWarnings(import_inops("stringi"))
  import_inops("stringi", overwrite = FALSE, inherits = FALSE)
  ls()
}
expect_error(
  temp.fun(),
  pattern = "ALL prepared infix operators already exist in the current environment"
)

temp.fun <- function(){
  suppressWarnings(import_inops("stringi"))
  import_inops("stringi", overwrite = FALSE, inherits = TRUE)
  ls()
}
expect_error(
  temp.fun(),
  pattern = "ALL prepared infix operators already exist in the current environment"
)

temp.fun <- function() {
  import_inops("stringi", exclude = "%foo%")
}
expect_warning(
  temp.fun(),
  pattern = "The following exclusions are not present, and are ignored:"
)

temp.fun <- function() {
  import_inops("stringi", include.only = "%foo%")
}
expect_warning(
  temp.fun(),
  pattern = "The following inclusions are not present, and are ignored:"
)

