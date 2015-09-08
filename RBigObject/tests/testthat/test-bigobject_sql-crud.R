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
  verify_sql("SELECT count(*) FROM hashtest", "8f1484ede34071d79c0009a1450744c3")
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
  verify_sql("SELECT count(*) FROM hashtest", "dc28600c0419f1e3f69f40c1e2aa028b")
  verify_sql("SELECT * FROM hashtest", "d81a3484e9ea85cbec1273940ee4a365")
})

test_that("MUTATE", {
  response <- bigobject_sql("UPDATE hashtest SET val='test' WHERE id='4'")
  expect_is(response, "NULL")
  verify_sql("SELECT * FROM hashtest", "46883d627fbbf184439f7a54d087ba37")
})

test_that("TRIM", {
  response <- bigobject_sql("TRIM hashtest TO 2")
  expect_is(response, "NULL")
  verify_sql("SELECT * FROM hashtest", "ee8ca8cd44ab5659b3d13e15ea2ffdc3")
  response <- bigobject_sql("TRIM hashtest TO 2")
  expect_is(response, "NULL")
  verify_sql("SELECT * FROM hashtest", "ee8ca8cd44ab5659b3d13e15ea2ffdc3")
})

test_that("ALTER TABLE", {
  response <- bigobject_sql("ALTER TABLE hashtest RENAME id TO key2")
  verify_sql("SELECT * FROM hashtest", "e390cdc89da3450cf6256cd0b6c3c329")
})

test_that("SELECT and sales", {
  verify_sql("SELECT * FROM sales LIMIT 2", "c3db36c1e334e2a2c884c9db0b056eb0")
  verify_sql("SELECT count(*) FROM sales", "5710c62ed03ac07a54ce9e4eb4bc3919")
  verify_sql("SELECT * FROM sales", "8d44d2d9bb36e9d227cfca72d504107d")
})


