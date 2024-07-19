
if ( requireNamespace("tinytest", quietly=TRUE) ){
  # perform regular tests:
  tinytest::test_package("tinycodet", set_env=list(LC_COLLATE="C"))
  tinytest::test_package("tinycodet", testdir = file.path("tinytest", "safer"), set_env=list(LC_COLLATE="C"))
  tinytest::test_package("tinycodet", testdir = file.path("tinytest", "import"), set_env=list(LC_COLLATE="C"))
  tinytest::test_package("tinycodet", testdir = file.path("tinytest", "strings"), set_env=list(LC_COLLATE="C"))
  tinytest::test_package("tinycodet", testdir = file.path("tinytest", "dry"), set_env=list(LC_COLLATE="C"))

  # perform special tests on fake packages:
  tinytest::test_package("tinycodet", testdir = file.path("tinytest", "special"), set_env=list(LC_COLLATE="C"))
}

