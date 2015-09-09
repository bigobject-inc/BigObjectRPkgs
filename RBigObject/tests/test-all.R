library("testthat")
library("RBigObject")
library("digest")
library("magrittr")
{
# bootstrap
  invisible(bigobject_sql("DROP TABLE hashtest"))
}

verify <- function(obj, signature) {
  expect_equal(digest(obj), signature)
}

verify_sql <- function(stmt, signature) {
  response <- bigobject_sql(stmt)
  verify(response, signature)
}

test_check("RBigObject")

{
  # clean up
  invisible(bigobject_sql("DROP TABLE hashtest"))

  bigobject_sql("GC ALL")
}


