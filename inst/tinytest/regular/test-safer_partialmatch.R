
opts <- options()
# check if the options that are set by tinycodet, still exist in the current verion of R:
expect_true(
  !is.null(opts$warnPartialMatchArgs) &&
    !is.null(opts$warnPartialMatchAttr) &&
    !is.null(opts$warnPartialMatchDollar)
)
