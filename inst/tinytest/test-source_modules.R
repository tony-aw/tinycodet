
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


# test environments:
exprs <- expression({
  mypaste <- function(x,y) stringi::`%s+%`(x,y)
  `%s+test%` <- function(x,y){
    mypaste(x,y)
  }
})
myalias. %@source% list(exprs=exprs)
source_inops(exprs=exprs)
expect_equal(ls(environment(`%s+test%`)), ls(environment(myalias.$mypaste)))

