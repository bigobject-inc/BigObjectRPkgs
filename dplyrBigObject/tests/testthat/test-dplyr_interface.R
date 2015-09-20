context("test dplyr interface")

get_src <- function()   src <- src_bigobject(get_ip(), get_port())

test_that("test src_bigobject and basic printing of sales", {
  src <- get_src()
  print(src)
  expect_true(length(tb_name <- db_list_tables(src$con)) > 0)
  expect_true(db_has_table(src$con, tb_name[1]))
})

get_sales <- function() tbl(get_src(), sql("SELECT * FROM sales"))

sales <- get_sales()
test_that("dplyr tbl", {
  print(sales)
})

test_that("dplyr filter", {
  sales1 <- dplyr::filter(sales, order_id == "3") %>% dplyr::collect()
  expect_equal(sales1 %>% nrow, 4)
  expect_equal(sales1 %>% sapply(class), structure(list(order_id = "character", Customer.id = "character", 
    Product.id = "character", channel_name = "character", Date = c("POSIXct", 
    "POSIXt"), qty = "numeric", total_price = "numeric"), 
    .Names = c("order_id", "Customer.id", "Product.id", "channel_name", "Date", "qty", "total_price")))
  sales2 <- dplyr::filter(sales, order_id == 3) %>% dplyr::collect()
  expect_equal(sales2 %>% nrow, 0)
  sales3 <- dplyr::filter(sales, qty == 8) %>% dplyr::collect()
  expect_true((sales3$qty == 8) %>% all)
  sales4 <- dplyr::filter(sales, total_price > 50) %>% dplyr::collect()
  expect_true((sales4$total_price > 50) %>% all)
  sales5 <- dplyr::filter(sales, Date < "2013-01-01 00:35:00") %>% dplyr::collect()
  expect_true(max(sales5$Date) < as.POSIXct("2015-01-01 00:35:00"))
})

test_that("dplyr select", {
  for(name in colnames(sales)) {
    tmp <- parse(text = sprintf("dplyr::select(sales, %s) %%>%% head %%>%% dplyr::collect()", name)) %>% eval
    expect_equal(colnames(tmp), name)
  }
})

test_that("dplyr arrange", {
  sales1 <- dplyr::arrange(sales, Date) %>%
    dplyr::select(Date) %>% 
    dplyr::collect()
  expect_true(all(sales1$Date %>% diff >= 0))
  sales2 <- dplyr::arrange(sales, desc(Date)) %>%
    dplyr::select(Date) %>%
    dplyr::collect()
  expect_true(all(sales2$Date %>% diff <= 0))
})

test_that("dplyr rename", {
  sales1 <- dplyr::rename(sales, date = Date) %>% 
    head %>%
    dplyr::collect()
  expect_true("date" %in% colnames(sales1))
  expect_false("date" %in% colnames(get_sales()))
})

test_that("dplyr mutate", {
})

test_that("dplyr sample_n", {
  expect_error(
    sales1 <- dplyr::sample_n(sales, 100) %>%
      dplyr::collect())
})

test_that("dplyr group_by summarise", {
  sales1 <- dplyr::group_by(sales, qty) %>%
    dplyr::summarise() %>% dplyr::collect()
  sales2 <- dplyr::select(sales, qty) %>% dplyr::collect()
  expect_equal(sales1$qty, sales2$qty %>% unique %>% sort)
})

test_that("dplyr join", {
#  Join is not supported currently!!
#   sales1 <- dplyr::group_by(sales, channel_name) %>%
#     dplyr::summarise(qty = mean(qty))
#   sales2 <- dplyr::group_by(sales, channel_name) %>%
#     dplyr::summarise(qty = max(qty))
#   dplyr::left_join(sales1, sales2, by = "channel_name")
})
