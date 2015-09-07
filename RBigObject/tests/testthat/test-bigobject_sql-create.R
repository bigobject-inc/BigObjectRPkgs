context("CREATE TABLE")

test_that("CREATE TABLE (second)", {
  stmt <- "CREATE TABLE hashtest (id STRING, val STRING, key(id)"
  expect_error(response <- bigobject_sql(stmt), regexp = "Syntax error")
})

test_that("CREATE TABLE (first)", {
  stmt <- "CREATE TABLE hashtest (id STRING, val STRING, key(id))"
  response <- bigobject_sql(stmt)
  expect_is(response, "list")
  expect_equal(response$Status, 0)
  expect_equal(response$Err, "")
})

test_that("CREATE TABLE (second)", {
  stmt <- "CREATE TABLE hashtest (id STRING, val STRING, key(id))"
  expect_error(response <- bigobject_sql(stmt), regexp = "Object already exists")
})
