
# expression ====
exprs <- expression({
  helloworld = function()print("hello world")
  goodbyeworld <- function() print("goodbye world")
  `%s+test%` <- function(x,y) stringi::`%s+%`(x,y)
  `%s*test%` <- function(x,y) stringi::`%s*%`(x,y)
   mymethod.foo <- function(x) UseMethod("mymethod.foo", x)
   mymethod.foo.numeric <- function(x)x * 2
   mymethod.foo.character <- function(x)chartr(x, old = "a-zA-Z", new = "A-Za-z")
})
tempenv <- new.env()
source(exprs = exprs, local = tempenv)


# test general ====
temp.fun <- function(){
  source_selection(list(exprs=exprs), regex= "^mymethod", fixed = c("%", ":="))
  ls() # list all objects residing within the function definition
}
expected <- c("mymethod.foo", "mymethod.foo.numeric", "mymethod.foo.character",
              "%s+test%", "%s*test%") |> sort()
expect_equal(temp.fun()|>sort(), expected)

temp.fun <- function(){
  source_selection(list(exprs=exprs), regex= "%|:=", fixed = c("mymethod.foo"))
  ls() # list all objects residing within the function definition
}
expected <- c("mymethod.foo", "mymethod.foo.numeric", "mymethod.foo.character",
              "%s+test%", "%s*test%") |> sort()
expect_equal(temp.fun()|>sort(), expected)

temp.fun <- function(){
  source_selection(list(exprs=exprs), select = c("helloworld", "goodbyeworld"))
  ls() # list all objects residing within the function definition
}
expect_equal(temp.fun()|>sort(), c("goodbyeworld", "helloworld"))


# test methods via regex ====
temp.fun <- function() {
  source_selection(list(exprs=exprs), regex = "^mymethod")
  mymethod.foo(1)
}
expect_equal(temp.fun(), 2)
temp.fun <- function() {
  source_selection(list(exprs=exprs), regex = "^mymethod")
  mymethod.foo("a")
}
expect_equal(temp.fun(), "A")


# test methods via fixed ====
temp.fun <- function() {
  source_selection(list(exprs=exprs), fixed = "mymethod.foo")
  mymethod.foo(1)
}
expect_equal(temp.fun(), 2)
temp.fun <- function() {
  source_selection(list(exprs=exprs), regex = "mymethod.foo")
  mymethod.foo("a")
}
expect_equal(temp.fun(), "A")


# test methods via select ====
temp.fun <- function() {
  source_selection(list(exprs=exprs), select= c("mymethod.foo", "mymethod.foo.numeric"))
  mymethod.foo(1)
}
expect_equal(temp.fun(), 2)
temp.fun <- function() {
  source_selection(list(exprs=exprs), select = c("mymethod.foo", "mymethod.foo.character"))
  mymethod.foo("a")
}
expect_equal(temp.fun(), "A")

# test function environments ====
exprs2 <- expression({
  mypaste <- function(x,y) stringi::`%s+%`(x,y)
  `%s+test%` <- function(x,y){
    mypaste(x,y)
  }
})
source_selection(list(exprs=exprs2), select = c("%s+test%", "mypaste"))
expect_equal(ls(environment(`%s+test%`)), ls(environment(mypaste)))


# check error handling regarding `lst` argument ====
expect_error(source_selection("lst", select = "helloworld"),
             pattern = "`lst` must be a list")

expect_error(source_selection(list(), select = "helloworld"),
             pattern = "`lst` is of length 0")

expect_error(source_selection(list(local=new.env()), select = "helloworld"),
             pattern = paste0(
               "Do not supply the `local` argument:",
               "\n",
               "Environment is already specified to be the current environment"
             ))

expect_error(source_selection(list(exprs=exprs, env = new.env()), select = "helloworld"),
             pattern = "unused argument")


# check error handling regarding selections ====
expect_error(source_selection(list(exprs=exprs)),
             pattern = "You must specified at least one of `select`, `regex`, or `fixed`")
expect_error(source_selection(list(exprs=exprs), regex = ""),
             pattern = "invalid `regex` argument given")

expect_error(source_selection(list(exprs=exprs), fixed = ""),
             pattern = "invalid `fixed` argument given")

expect_error(source_selection(list(exprs=exprs), regex = character()),
             pattern = "invalid `regex` argument given")

expect_error(source_selection(list(exprs=exprs), fixed = character()),
             pattern = "invalid `fixed` argument given")

expect_error(source_selection(list(exprs=exprs), select = c("foo", "bar")),
                           pattern = paste0(
                             "The following selections not in the source:",
                             "\n",
                             "foo, bar"
                           ))


# check warnings regarding pattern matches ====
expect_warning(
  source_selection(list(exprs = exprs), regex = "bla"),
  pattern = "no appropriate matches found in sourced script"
)
expect_warning(
  source_selection(list(exprs = exprs), fixed = "bla"),
  pattern = "no appropriate matches found in sourced script"
)

