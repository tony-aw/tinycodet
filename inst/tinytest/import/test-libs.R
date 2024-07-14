

obj1 <- tinycodet:::.internal_prep_Namespace(
  "stringi", c("foo", .libPaths()), sys.call()
)
obj2 <- tinycodet:::.internal_prep_Namespace(
  "stringi", c(.libPaths(), "foo"), sys.call()
)
expect_equal(
  obj1, obj2,
)

