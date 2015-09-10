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
  df0 <-
    structure(list(order_id = c("1", "2", "2", "3", "3", "3", "3", 
    "4", "5", "5"), Customer.id = c("3226", "6691", "6691", "4138", 
    "4138", "4138", "4138", "1292", "5596", "5596"), Product.id = c("2557", 
    "2631", "1833", "1626", "375", "3336", "736", "4434", "4135", 
    "3528"), channel_name = c("am/pm", "am/pm", "am/pm", "am/pm", 
    "am/pm", "am/pm", "CVS", "7-11", "7-11", "am/pm"), Date = structure(c(1356969844, 
    1356970286, 1356970862, 1356971422, 1356971744, 1356972312, 1356972934, 
    1356973560, 1356973722, 1356974290), class = c("POSIXct", "POSIXt"
    )), qty = c(8, 4, 1, 5, 6, 8, 6, 6, 10, 9), total_price = c(52.24, 
    39.72, 6.9, 42.1, 67.26, 41.68, 56.4, 86.64, 50.1, 94.68)), .Names = c("order_id", 
    "Customer.id", "Product.id", "channel_name", "Date", "qty", "total_price"
    ), class = "data.frame", row.names = c("1", "2", "3", "4", "5", 
    "6", "7", "8", "9", "10"))
  expect_equal(nrow(df), 10L)
  expect_equal(rs$index, 10L)
  expect_equal(df, df0)
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

