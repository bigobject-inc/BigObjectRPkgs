context("test dplyr interface")

get_src <- function()   src <- src_bigobject(get_ip(), get_port())

test_that("test src_bigobject and basic printing of sales", {
  src <- get_src()
  print(src)
  expect_true(length(tb_name <- db_list_tables(src$con)) > 0)
  expect_true(db_has_table(src$con, tb_name[1]))
})

get_sales <- function() tbl(get_src(), sql("SELECT * FROM sales"))

test_that("dplyr tbl", {
  sales <- get_sales()
  print(sales)
})

test_that("dplyr filter", {
  sales <- get_sales()
  dplyr::filter(sales, order_id == 3) %>% dplyr::compute()
  dplyr::filter(sales, order_id == "3") %>% dplyr::compute()
})
