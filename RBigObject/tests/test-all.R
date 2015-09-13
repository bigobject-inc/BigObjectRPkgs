library("testthat")
library("RBigObject")
library("magrittr")

if (Sys.getenv("BIGOBJECT_IP") != "") {
  bigobject_verbose(FALSE)
  bigobject_connection(Sys.getenv("BIGOBJECT_IP"), Sys.getenv("BIGOBJECT_PORT"))
  
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
    invisible(bigobject_sql("GC ALL"))
  }
}


