#'@title Connect to BigObject
#'@param ip string. The ip address of BigObject
#'@param port string. The port number of BigObject
#'@return A BigObject src
#'@export
src_bigobject <- function(ip, port) {
  drv <- DBI::dbDriver("BigObject")
  con <- DBI::dbConnect(drv, ip, port)
  dplyr::src_sql("bigobject", con, disco = DBI::dbDisconnect(con))
}

#'@export
src_desc.src_bigobject <- function(src) {
  DBI::dbGetInfo(src$con)
}

#'@export
tbl.src_bigobject <- function(src, from, ...) {
  dplyr::tbl_sql("bigobject", src = src, from = from, ...)
}

#'@export
sql_escape_string.BigObjectConnection <- function(con, x) x

#'@export
sql_escape_ident.BigObjectConnection <- function(con, x) x

#'@export
sql_subquery.BigObjectConnection <- function(con, sql, name) {
  if (is.ident(sql)) return(sql)
  build_sql("(", sql, ")", con = con)
}

#'@export
src_translate_env.src_bigobject <- function(x) {
  sql_variant(
    base_scalar, sql_translator(.parent = base_agg, 
    n = function() sql("count(*)"), 
    cor = sql_prefix("corr"), 
    cov = sql_prefix("covar_samp"), 
    sd = sql_prefix("stddev_samp"), 
    var = sql_prefix("var_samp"), 
    all = sql_prefix("bool_and"), 
    any = sql_prefix("bool_or"), 
    paste = function(x, collapse) build_sql("string_agg(", x, collapse, ")")
    ), 
    base_win
  )
}

#'@export
#'@importFrom dplyr build_sql
#'@importFrom dplyr db_save_query
db_save_query.BigObjectConnection <- function(con, sql, name, temporary = TRUE, ...) {
  tt_sql <- build_sql("CREATE ", if (temporary) 
      sql("TEMPORARY "), "TABLE ", ident(name), " AS ", sql, 
      con = con)
  dbGetQuery(con, tt_sql)
  name
}
