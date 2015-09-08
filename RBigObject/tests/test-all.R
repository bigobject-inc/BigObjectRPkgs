library("testthat")
library("RBigObject")
library("digest")
library("magrittr")
{
# bootstrap
  invisible(bigobject_sql("DROP TABLE hashtest"))
}

verify_sql <- function(stmt, result) {
  response <- bigobject_sql(stmt)
  expect_equal(digest(response), result)
}

test_check("RBigObject")

{
  # clean up
  invisible(bigobject_sql("DROP TABLE hashtest"))
}


