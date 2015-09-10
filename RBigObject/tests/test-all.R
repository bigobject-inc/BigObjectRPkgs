options(
  "BIGOBJECT_IP" = Sys.getenv("BIGOBJECT_IP"),
  "BIGOBJECT_PORT" = Sys.getenv("BIGOBJECT_PORT")
)

library("testthat")
library("RBigObject")
library("digest")
library("magrittr")
{
# bootstrap
  invisible(bigobject_sql("DROP TABLE hashtest"))
  invisible(bigobject_sql("DROP TABLE iristest"))
}

test_check("RBigObject")

{
  # clean up
  invisible(bigobject_sql("DROP TABLE hashtest"))
  invisible(bigobject_sql("DROP TABLE iristest"))
  bigobject_sql("GC ALL")
}


