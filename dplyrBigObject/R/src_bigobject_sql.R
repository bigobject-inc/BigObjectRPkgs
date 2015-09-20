#'@title Connect to BigObject
#'@param ip string. The ip address of BigObject
#'@param port string. The port number of BigObject
#'@return A BigObject src
#'@importFrom RBigObject get_ip
#'@importFrom RBigObject get_port
#'@export
src_bigobject <- function(ip, port) {
  drv <- DBI::dbDriver("BigObject")
  con <- DBI::dbConnect(drv, ip, port)
  dplyr::src_sql("bigobject", con, disco = DBI::dbDisconnect(con))
}

#'@importFrom dplyr src_desc
#'@export
src_desc.src_bigobject <- function(x) {
  DBI::dbGetInfo(x$con)
}

#'@importFrom dplyr tbl
#'@export
tbl.src_bigobject <- function(src, from, ...) {
  dplyr::tbl_sql("bigobject", src = src, from = from, ...)
}

#'@importFrom dplyr sql_escape_string
#'@export
sql_escape_string.BigObjectConnection <- function(con, x) x

#'@importFrom dplyr sql_escape_ident
#'@export
sql_escape_ident.BigObjectConnection <- function(con, x) x

#'@importFrom dplyr sql_subquery
#'@export
sql_subquery.BigObjectConnection <- function(con, sql, name, ...) {
  if (dplyr::is.ident(sql)) return(sql)
  build_sql("(", sql, ")", con = con)
}

#'@importFrom dplyr src_translate_env sql sql_prefix build_sql base_win base_scalar sql_translator base_agg
#'@export
src_translate_env.src_bigobject <- function(x) {
  dplyr::sql_variant(
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
#'@importFrom dplyr db_save_query ident
#'@importFrom DBI dbGetQuery
db_save_query.BigObjectConnection <- function(con, sql, name, temporary = TRUE, ...) {
  tt_sql <- build_sql("CREATE ", if (temporary) 
      sql("TEMPORARY "), "TABLE ", ident(name), " AS ", sql, 
      con = con)
  dbGetQuery(con, tt_sql)
  name
}
