helloworld <- function() print("hello world")
goodbyeworld <- function() print("goodbye world")
`%s+test%` <- function(x,y) stringi::`%s+%`(x,y)
`%s*test%` <- function(x,y) stringi::`%s*%`(x,y)

mymeth <- function(x){
  UseMethod("mymeth", x)
}

mymeth.numeric <- function(x)x * 2

mymeth.character <- function(x){chartr(x, old = "a-zA-Z", new = "A-Za-z")}
