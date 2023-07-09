# test import_as:
stri <- loadNamespace("stringi") |> getNamespaceExports()
import_as(stri., "stringi")
expect_equal(names(stri.)|>sort(), sort(stri))

expect_error(import_as(stri, "stringi", dependencies = "data.table"))
expect_error(import_as(stri, "stringi", enhances = "data.table"))
expect_error(import_as(stri, "stringi", extensions = "data.table"))

# test import_inops:
stri <- grep(":=|%", loadNamespace("stringi") |> getNamespaceExports(), value = TRUE)
temp.fun <- function(){
  import_inops("stringi")
  ls()
}
expect_equal(temp.fun()|>sort(), sort(stri))

stri <- grep(":=|%", loadNamespace("stringi") |> getNamespaceExports(), value = TRUE)
temp.fun <- function(){
  import_inops("stringi", include.only = "%stri+%")
  ls()
}
expect_equal(temp.fun()|>sort(), "%stri+%")

stri <- grep(":=|%", loadNamespace("stringi") |> getNamespaceExports(), value = TRUE)
stri <- setdiff(stri, "%stri+%")
temp.fun <- function(){
  import_inops("stringi", exclude = "%stri+%")
  ls()
}
expect_equal(temp.fun()|>sort(), sort(stri))


# test %@source%:
exprs <- expression({
  helloworld = function()print("helloworld")
  goodbyeworld <- function() print("goodbye world")
  })
myalias. %@source% list(exprs=exprs)
expect_equal(names(myalias.) |> sort(), c("goodbyeworld", "helloworld"))


# test source_inops:
exprs <- expression({
  `%s+test%` <- function(x,y) stringi::`%s+%`(x,y)
  `%s*test%` <- function(x,y) stringi::`%s*%`(x,y)
})
temp.fun <- function(){
  source_inops(exprs=exprs)
  ls()
}
temp.fun()
expect_equal(temp.fun()|>sort(), sort(c("%s+test%", "%s*test%")))

