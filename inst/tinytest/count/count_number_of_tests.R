
# set working directory to source file location ====
stub <- function() {}
thisPath <- function() {
  # Based on:
  # https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/15373917#15373917
  # https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/7585599#7585599
  # https://stackoverflow.com/questions/47044068/get-the-path-of-current-script/47045368#47045368
  # https://stackoverflow.com/questions/53512868/how-to-automatically-include-filepath-in-r-markdown-document/53516876#53516876
  # https://gist.github.com/jasonsychau/ff6bc78a33bf3fd1c6bd4fa78bbf42e7
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  if (length(grep("^-f$", cmdArgs)) > 0) {
    # R console option
    return(normalizePath(dirname(cmdArgs[grep("^-f", cmdArgs) + 1]))[1])
  } else if (length(grep("^--file=", cmdArgs)) > 0) {
    # Rscript/R console option
    scriptPath <- normalizePath(dirname(sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)])))[1]
    return(scriptPath)
  } else if (Sys.getenv("RSTUDIO") == "1") {
    if (rstudioapi::isAvailable(version_needed=NULL,child_ok=FALSE)) {
      # RStudio interactive
      return(dirname(rstudioapi::getSourceEditorContext()$path))
    } else if (is.null(knitr::current_input(dir = TRUE)) == FALSE) {
      # Knit
      return(knitr::current_input(dir = TRUE))
    } else {
      # R markdown on RStudio
      return(getwd())
    }
  } else if (is.null(attr(stub, "srcref")) == FALSE) {
    # 'source'd via R console
    return(dirname(normalizePath(attr(attr(stub, "srcref"), "srcfile")$filename)))
  } else {
    stop("Cannot find file path")
  }
}

wd <- thisPath()
setwd(wd)


# count number of "expect_" occurrences ====
testfiles <- list.files(file.path(wd, "/tinytest/"), pattern = "*.R")
n.testfiles <- length(testfiles)
temp.fun <- function(x) {
  foo <- readLines(file.path("./tinytest/", x))
  sum(stringi::stri_count(foo, regex="expect_"))
}
testcount_regular <- sapply(
  testfiles,
  FUN = temp.fun
) |> sum()


# count number of loop iterated tests ====
n.iterations <- 0
n.loops <- 0
for(i in testfiles) {
  if(!grepl("-special", i)) {
    my_env <- new.env()
    source(file.path("./tinytest/", i), local = my_env) |> suppressMessages()
    if("enumerate" %in% names(my_env) & "loops" %in% names(my_env)){
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
testcount_total / nfuns # about a dozen tests per function on average


