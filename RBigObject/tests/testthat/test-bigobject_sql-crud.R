context("Basic CRUD")

test_that("CREATE TABLE (syntax error)", {
  stmt <- "CREATE TABLE hashtest (id STRING, val STRING, key(id)"
  expect_error(response <- bigobject_sql(stmt), regexp = "Syntax error")
})

test_that("CREATE TABLE (first)", {
  stmt <- "CREATE TABLE hashtest (id STRING, val STRING, key(id))"
  response <- bigobject_sql(stmt)
  expect_is(response, "NULL")
})

test_that("CREATE TABLE (second)", {
  stmt <- "CREATE TABLE hashtest (id STRING, val STRING, key(id))"
  expect_error(response <- bigobject_sql(stmt), regexp = "Object already exists")
})

test_that("INSERT with single statement", {
  stmt <- "INSERT INTO hashtest VALUES (1, hello_internet)"
  expect_is(response <- bigobject_sql(stmt), "NULL")
})

test_that("SELECT to verify that INSERT is succesful", {
  df <- bigobject_sql("SELECT count(*) FROM hashtest")
  df2 <- structure(list(`COUNT(*)` = 1), .Names = "COUNT(*)", class = "data.frame", row.names = "1")
  expect_equal(df, df2)
})

test_that("INSERT with multiple statement", {
  stmt <- c(
    "INSERT INTO hashtest VALUES (2, hello_world)",
    "INSERT INTO hashtest VALUES (3, hello_jeff)",
    "INSERT INTO hashtest VALUES (4, hello_eugene)",
    "INSERT INTO hashtest VALUES (5, hello_ethan)"
  )
  expect_is(response <- bigobject_sql(stmt), "list")
  expect_equal(response, vector("list", 4))
})

test_that("SELECT to verify that INSERT is succesful", {
  df <- bigobject_sql("SELECT count(*) FROM hashtest")
  expect_equal(df, structure(list(`COUNT(*)` = 5), .Names = "COUNT(*)", class = "data.frame", row.names = "1"))
  df <- bigobject_sql("SELECT * FROM hashtest")
  expect_equal(df, structure(list(id = c("1", "2", "3", "4", "5"), val = c("hello_internet", 
    "hello_world", "hello_jeff", "hello_eugene", "hello_ethan")), .Names = c("id", 
    "val"), class = "data.frame", row.names = c("1", "2", "3", "4", 
    "5"))
  )
})

test_that("MUTATE", {
  response <- bigobject_sql("UPDATE hashtest SET val='test' WHERE id='4'")
  expect_is(response, "NULL")
  df <- bigobject_sql("SELECT * FROM hashtest")
  expect_equal(df$val[4], "test")
})

test_that("TRIM", {
  df0 <- bigobject_sql("SELECT * FROM hashtest")
  response <- bigobject_sql("TRIM hashtest TO 2")
  expect_is(response, "NULL")
  df <- bigobject_sql("SELECT * FROM hashtest")
  expect_equal(df, head(df0, 2))
})

test_that("ALTER TABLE", {
  df0 <- bigobject_sql("SELECT * FROM hashtest")
  response <- bigobject_sql("ALTER TABLE hashtest RENAME id TO key2")
  df <- bigobject_sql("SELECT * FROM hashtest")
  expect_equal(colnames(df)[1], "key2")
  expect_equal(df0[[1]], df[[1]])
  expect_equal(df0[[2]], df[[2]])
})

test_that("SELECT and sales", {
  df <- bigobject_sql("SELECT * FROM sales LIMIT 2")
  expect_equal(df, 
    structure(list(order_id = c("1", "2"), Customer.id = c("3226", 
    "6691"), Product.id = c("2557", "2631"), channel_name = c("am/pm", 
    "am/pm"), Date = structure(c(1356969844, 1356970286), class = c("POSIXct", 
    "POSIXt")), qty = c(8, 4), total_price = c(52.24, 39.72)), .Names = c("order_id", 
    "Customer.id", "Product.id", "channel_name", "Date", "qty", "total_price"
    ), class = "data.frame", row.names = c("1", "2"))
  )
  df <- bigobject_sql("SELECT count(*) FROM sales")
  expect_equal(df, structure(list(`COUNT(*)` = 1e+05), .Names = "COUNT(*)", class = "data.frame", row.names = "1"))
  df <- bigobject_sql("SELECT * FROM sales")
  expect_equal(nrow(df), 100000)
  expect_equal(sapply(df, class), structure(list(order_id = "character", Customer.id = "character", 
    Product.id = "character", channel_name = "character", Date = c("POSIXct", 
    "POSIXt"), qty = "numeric", total_price = "numeric"), 
    .Names = c("order_id", "Customer.id", "Product.id","channel_name", "Date", "qty", "total_price"
  )))
  expect_equal(sum(df[[6]]), 549522)
})

