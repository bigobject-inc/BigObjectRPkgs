context("DBI Interface")

ip <- getOption("BIGOBJECT_IP", "127.0.0.1")
port <- getOption("BIGOBJECT_PORT", "9090")

test_that("DBI Driver for BigObject", {
  drv <- dbDriver("BigObject")
  dbGetInfo(drv)
  summary(drv)
  dbUnloadDriver(drv)
})

test_that("DBI Connection", {
  drv <- dbDriver("BigObject")
  con <- dbConnect(drv, ip, port)
  con1 <- dbConnect(con)
  dbDisconnect(con)
  stopifnot(ls(con@results) %>% length == 0)
})

test_that("BigObjectResult", {
  drv <- dbDriver("BigObject")
  con <- dbConnect(drv, ip, port)
  rs <- dbSendQuery(con, "SELECT * FROM sales")
  expect_equal(rs$index, 0L)
  df <- fetch(rs, 10)

  expect_equal(nrow(df), 10L)
  expect_equal(rs$index, 10L)
  print(df)
  dbClearResult(rs)
  expect_equal(ls(con@results) %>% length, 0L)
  expect_false(rs@handle %in% dbListResults(con))
  dbDisconnect(con)
  stopifnot(ls(con@results) %>% length == 0)
})

test_that("dbSendQuery with error", {
  drv <- dbDriver("BigObject")
  con <- dbConnect(drv, ip, port)
  rs <- dbSendQuery(con, "SELECT * FROM saless")
  expect_is(rs, "BigObjectErrorResult")
  expect_match(rs@err, "doesn't exist: table")
  expect_true(dbClearResult(rs))
  dbDisconnect(con)
})

test_that("dbGetQuery", {
  drv <- dbDriver("BigObject")
  con <- dbConnect(drv, ip, port)
  df <- dbGetQuery(con, "SELECT * FROM sales")
  df2 <- dbReadTable(con, "sales")
  expect_equal(df, df2)
  expect_equal(nrow(df), 100000)
  dbDisconnect(con)
})

test_that("dbGetInfo", {
  drv <- dbDriver("BigObject")
  con <- dbConnect(drv, ip, port)
  expect_match(capture.output(dbGetInfo(con)), ip)
  expect_match(capture.output(dbGetInfo(con)), port)
  summary(con)
})

