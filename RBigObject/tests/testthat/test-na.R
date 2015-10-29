context("NA Handling")

ip <- get_ip()
port <- get_port()

test_that("issue #16", {
  drv <- dbDriver("BigObject")
  con <- dbConnect(drv, ip, port)

  tryCatch({
    dbGetQuery(con, sql)
    df <- data.frame(
      stringsAsFactors = FALSE,
      a = rnorm(10), 
      b = sample(letters, 10), 
      c = c(1, 2, 3, NA, 5, 6, 7, 8, 9, 10),
      d = c('a', 'b', 'c', 'd', NA, 'f', 'g', 'h', 'i', 'j'))
    dbWriteTable(con, "testdf", df, row.names = FALSE)
    df2 <- dbReadTable(con, "testdf")
    expect_equal(df, df2)
  }, finally = {
    dbRemoveTable(con, "testdf")
    dbDisconnect(con)
  })
})

