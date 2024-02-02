
library(tinytest)
library(tinycodet)

# set working directory to source file location ====
SourceFileLocation <- function() {
  # BATCH way:
  path <- funr::get_script_path()
  if(!is.null(path)) return(path)
  
  # R-Studio way:
  if(Sys.getenv("RSTUDIO") == "1") {
    if(rstudioapi::isAvailable(version_needed = NULL,child_ok = FALSE)) {
      return(dirname(rstudioapi::getSourceEditorContext()$path))
    }
    if(is.null(knitr::current_input(dir = TRUE)) == FALSE) {
      return(knitr::current_input(dir = TRUE))
    }
    return(getwd())
  }
}

wd <- SourceFileLocation()
setwd(wd)
setwd("..")



# count number of "expect_" occurrences ====
testfiles <- list.files(file.path(getwd(), "/"), pattern = "*.R", recursive = TRUE)
n.testfiles <- length(testfiles)
temp.fun <- function(x) {
  foo <- readLines(file.path(x))
  sum(stringi::stri_count(foo, regex="expect_"))
}
testcount_regular <- sapply(
  testfiles,
  FUN = temp.fun
) |> sum()


# count number of loop iterated tests ====
n.iterations <- 0
n.loops <- 0
testfiles <- list.files(file.path(getwd(), "regular"), pattern = "*.R", recursive = FALSE)
for(i in testfiles) {
  if(!grepl("-special", i)) {
    my_env <- new.env()
    source(file.path(getwd(),"regular", i), local = my_env) |> suppressMessages()
    if("enumerate" %in% names(my_env) && "loops" %in% names(my_env)){
      print(my_env$enumerate)
      n.iterations <- n.iterations + my_env$enumerate
      n.loops <- n.loops + my_env$loops
    }
  }
}
testcount_loops <- n.iterations - n.loops


# determine total number of tests ====
testcount_total <- testcount_regular + testcount_loops
print(testcount_total)


# test / function ratio ====
nfuns <- length(getNamespaceExports(loadNamespace("tinycodet")))
testcount_total / nfuns # about 48 tests per function on average


