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
  verify(fetch(rs, 10), "143ba4e036ba2892ded1cf931c037390")
  expect_equal(rs$index, 10L)
  df2 <- fetch(rs, -1)
  verify(df2, "8407dabc18a838407e34f9c26861da78")
  expect_equal(ls(con@results) %>% length, 1L)
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
  verify(df, "8d44d2d9bb36e9d227cfca72d504107d")
  df2 <- dbReadTable(con, "sales")
  verify(df2, "8d44d2d9bb36e9d227cfca72d504107d")
  dbDisconnect(con)
})

test_that("dbGetInfo", {
  drv <- dbDriver("BigObject")
  con <- dbConnect(drv, ip, port)
  verify(capture.output(dbGetInfo(con)), "4aa54be68bb27826ebfceba44dd3428a")
  summary(con)
})

