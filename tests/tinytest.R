
if ( requireNamespace("tinytest", quietly=TRUE) ){
  # perform regular tests:
  tinytest::test_package("tinycodet", testdir = file.path("tinytest", "regular"))

  # perform special tests on fake packages:
  tinytest::test_package("tinycodet", testdir = file.path("tinytest", "special"))
}

