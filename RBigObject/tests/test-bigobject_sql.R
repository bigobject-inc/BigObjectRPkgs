library(RBigObject)
stmt <- c(
  "CREATE TABLE hash (id STRING, val STRING, key(id))",
  "INSERT INTO hash VALUES (key:1, hello_internet)"
)
response <- bigobject_sql(stmt)
stopifnot(class(response) == "list")

stmt <- c(
  "CREATE TABLE hash (id STRING, val STRING, key(id))",
  "INSERT INTO hash VALUES (key:1, hello_internet"
)
response <- bigobject_sql(stmt)
stopifnot(class(response) == "list")
stopifnot(response$Status == -6)

stmt <- c(
  "INSERT INTO hash VALUES (key:1, hello_internet"
)
response <- bigobject_sql(stmt)
stopifnot(class(response) == "list")
stopifnot(response$Status == -305)
