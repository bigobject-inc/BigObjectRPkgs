context("Import Data")

test_that("Import iris", {
  bigobject_import(iris, "iristest")
  df <- bigobject_sql("SELECT * FROM iristest")
  df$Species <- factor(df$Species, levels = iris$Species %>% levels)
  attr(df, "row.names") <- attr(iris, "row.names")
  expect_equal(df, iris)
})

test_that("Append iristest", {
  bigobject_import(iris, "iristest", action = "append")
  df <- bigobject_sql("SELECT * FROM iristest")
  expect_true(all(head(df, 150) == tail(df, 150)))
})

test_that("Create iristest should fail", {
  expect_error(bigobject_import(iris, "iristest"), "already exists") 
})

test_that("Overwrite iristest", {
  bigobject_import(iris, "iristest", action = "overwrite")
  df <- bigobject_sql("SELECT * FROM iristest")
  df$Species <- factor(df$Species, levels = iris$Species %>% levels)
  attr(df, "row.names") <- attr(iris, "row.names")
  expect_equal(df, iris)
})