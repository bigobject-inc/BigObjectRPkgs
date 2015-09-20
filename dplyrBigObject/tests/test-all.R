library("testthat")
library("RBigObject")
library("dplyrBigObject")

if (Sys.getenv("BIGOBJECT_IP") != "") {

  bigobject_connection(Sys.getenv("BIGOBJECT_IP"), Sys.getenv("BIGOBJECT_PORT"))
  bigobject_verbose(FALSE)
  
  test_check("RBigObject")
  
}


