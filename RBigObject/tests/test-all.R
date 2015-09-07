library("testthat")
library("RBigObject")

{
# bootstrap
  invisible(bigobject_sql("DROP TABLE hashtest"))
}

test_check("RBigObject")

{
  # clean up
  invisible(bigobject_sql("DROP TABLE hashtest"))
}
