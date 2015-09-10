#'@title Configure the default connection to BigObject
#'@param ip string. The ip address or domain name to the BigObject instance.
#'@param port string. The port number.
#'@details
#'The default ip is stored in the options with key: \code{"BIGOBJECT_IP"}.
#'The default port is stored in the options with key: \code{"BIGOBJECT_PORT"}.
#'@export
bigobject_connection <- function(ip, port) {
  stopifnot(length(ip) == 1)
  stopifnot(length(port) == 1)
  options("BIGOBJECT_IP" = as.character(ip)[1], "BIGOBJECT_PORT" = as.character(port)[1])
}

check_response <- function(obj) {
  if (obj$Status != 0) stop(obj$Err)
}

.type_mapping <- c(
  "STRING" = "character",
  "BYTE" = "character",
  "INT8" = "integer",
  "INT16" = "integer",
  "INT32" = "integer",
  "INT64" = "numeric",
  "FLOAT" = "numeric",
  "DOUBLE" = "numeric",
  "DATE32" = "POSIXct",
  "DATETIME32" = "POSIXct",
  "DATETIME64" = "POSIXct"
)

.type_class <- list(
  "STRING" = "character",
  "BYTE" = "character",
  "INT8" = "integer",
  "INT16" = "integer",
  "INT32" = "integer",
  "INT64" = "numeric",
  "FLOAT" = "numeric",
  "DOUBLE" = "numeric",
  "DATE32" = NA,
  "DATETIME32" = c("POSIXct", "POSIXt"),
  "DATETIME64" = NA
)

type_mapping <- function(x) {
  retval <- .type_mapping[x]
  stopifnot(!is.na(retval))
  lapply(retval, function(x) get(sprintf("as.%s", x)))
}

#'@title Submit SQL Query to BigObject
#'@param stmt character. A series of SQL statement. 
#'Each element is a complete SQL statement.
#'These query will be executed from the first element to the last element.
#'@param ip string. The ip address or domain name to the BigObject instance.
#'@param port string. The port number.
#'@param verbose logical value. Whether to print verbose message.
#'@param page integer. The chunk size of retriving data. The max size is 1000.
#'@return 
#'\itemize{
#'  \item{\code{data.frame}} Single statement. Some data are returned.
#'  \item{\code{NULL}} Single statement. No data is returned.
#'  \item{\code{list}} Multiple statements. The element is the returned \code{data.frame} or \code{NULL}
#'}
#'@references \url{http://docs.bigobject.io/API/index.html}
#'@details
#'This is a low level function which will be called by other sql interface.
#'Please do not use this function unless you want to modify the returned function by your self.
#'@export
#'@importFrom httr POST
#'@importFrom httr content
#'@importFrom jsonlite toJSON
#'@importFrom jsonlite fromJSON
#'@importFrom magrittr %>%
bigobject_sql <- function(stmt, ip = getOption("BIGOBJECT_IP", "127.0.0.1"), port = getOption("BIGOBJECT_PORT", "9090"), verbose = getOption("BIGOJBECT_VERBOSE", TRUE), page = 1000L) {
  if (length(stmt) == 1) .bigobject_sql(stmt, ip, port, verbose) else lapply(stmt, .bigobject_sql, ip = ip, port = port, verbose = verbose, page)
}

.bigobject_stmt <- function(stmt, opts = list()) {
  stopifnot(length(stmt) == 1)
  if (length(opts) == 0) list(Stmt = stmt) %>% toJSON(auto_unbox = TRUE) else {
    list(list(Stmt = stmt, Opts = opts)) %>% 
      lapply(toJSON, pretty = FALSE, auto_unbox = TRUE) %>%
      paste(collapse = "")
  }
}

.get_bigobject_url <- function(ip, port, path = "/cmd") {
  sprintf("%s%s", paste(ip, port, sep = ":"), path)
}

.get_bigobject_operator <- function(ip, port, path = "/cmd", operator = POST) {
  url <- .get_bigobject_url(ip, port, path)
  function(body, ..., as = NULL) {
    if (is.null(body)) {
      operator(url = url, ...) %>% content(as) 
    } else {
      operator(url = url, body = body, ...) %>% content(as)
    }
  }
}

.get_bigobject_poster <- function(ip, port, path = "/cmd") {
  .get_bigobject_operator(ip, port, path = path, operator = POST)
}

.get_bigobject_getter <- function(ip, port, path = "/cmd") {
  .get_bigobject_operator(ip, port, path = path, operator = GET)
}

.get_bigobject_putter <- function(ip, port, path = "/cmd") {
  .get_bigobject_operator(ip, port, path = path, operator = PUT)
}

.bigobject_sql_handle <- function(stmt, ip, port, verbose) {
  body <- .bigobject_stmt(stmt, list(Handle = TRUE))
  poster <- .get_bigobject_poster(ip, port)
  handle <- poster(body)
  check_response(handle)
  handle$Content$res
}


.bigobject_sql_hdesc <- function(handle, ip, port, verbose) {
  body <- .bigobject_stmt(sprintf("hdesc %s", handle))
  poster <- .get_bigobject_poster(ip, port)
  desc <- poster(body, as = "text") %>% fromJSON
  desc$Content$schema$attr
}

.bigobject_sql_scan <- function(handle, ip, port, verbose, start = 1L, end = -1L, page = 1000L, as = c("json", "raw", "table"), desc = NULL) {
  stopifnot(page <= 1000 & page >= 1)
  body <- .bigobject_stmt(sprintf("scan %s %d %d %d", handle, as.integer(start), as.integer(end), as.integer(page)))
  poster <- .get_bigobject_poster(ip, port)
  switch(as[1], 
         "json" = poster(body),
         "raw" = poster(body, as = "raw"),
         "table" = {
           if (is.null(desc)) desc <- .bigobject_sql_hdesc(handle, ip, port, verbose)
           .bigobject_scan_table(poster(body, as = "raw"), desc)
         },
         stop("Not supported argument!")
         )
}

.bigobject_scan_table <- function(retval.bin, desc) {
  con <- rawConnection(retval.bin)
  on.exit(close(con), add = TRUE)
  tmp <- readLines(con, n = 1L)
  obj <- fromJSON(tmp)
  if (obj$Status != 0) stop(obj$Err) else {
    if (is.null(obj$Content)) return(invisible(NULL))
    if (obj$Content$index == -1) {
      retval.content <- list(obj$Content$content)
    } else {
      retval.content <- vector(mode = "list", 100)
      retval.content[[retval.i <- 1]] <- obj$Content$content
      while(obj$Content$index != -1) {
        tmp <- readLines(con, n = 1L)
        obj <- fromJSON(tmp)
        if (obj$Status != 0) stop(obj$Err) else {
          retval.i <- retval.i + 1
          if (retval.i > length(retval.content)) {
            length(retval.content) <- 2 * length(retval.content)
          }
          retval.content[[retval.i]] <- obj$Content$content
        }
      }
    }
  }
  retval.content <- retval.content[sapply(retval.content, length) > 0]
  stopifnot(sapply(retval.content, function(x) dim(x)[2]) %>% unique %>% length == 1)
  stopifnot(nrow(desc) == dim(retval.content[[1]])[2])
  mapper <- type_mapping(desc$type)
  mapper_class <- .type_class[desc$type]
  retval <- list()
  # browser()
  for(i in seq_along(mapper)) {
    retval[[desc$name[i]]] <- lapply(retval.content, function(mat) {
      mapper[[i]](mat[,i])
    }) %>% unlist
    class(retval[[desc$name[i]]]) <- mapper_class[[i]]
  }
  class(retval) <- "data.frame"
  rownames(retval) <- seq_len(length(retval[[1]])) %>% paste
  retval
}

.bigobject_sql <- function(stmt, ip, port, verbose, page = 1000L) {
  stopifnot(class(stmt) == "character")
  handle <- .bigobject_sql_handle(stmt, ip, port, verbose)
  if (is.null(handle)) return(invisible(NULL))
  desc <- .bigobject_sql_hdesc(handle, ip, port, verbose)
  retval <- .bigobject_sql_scan(handle, ip, port, verbose, page = 1000L, as = "table", desc = desc)
  .bigobject_gc(handle, ip, port)
  retval
}


.bigobject_datatype <- function(obj) {
  switch(class(obj)[1],
         "character" = "STRING",
         "integer" = "INT32",
         "numeric" = "DOUBLE",
         "POSIXct" = "DATETIME64",
         "factor" = "STRING"
  )
}

.bigobject_gc_list <- function(ip, port) {
  poster <- .get_bigobject_poster(ip, port)
  body <- .bigobject_stmt("GC LIST")
  obj <- poster(body)
  check_response(obj)
  obj$Content %>% unlist
}

.bigobject_gc <- function(handle, ip, port) {
  poster <- .get_bigobject_poster(ip, port)
  body <- .bigobject_stmt(sprintf("GC DEL %s", handle))
  obj <- poster(body)
  check_response(obj)
  invisible(NULL)
}

.bigobject_gc_all <- function(ip, port) {
  poster <- .get_bigobject_poster(ip, port)
  body <- .bigobject_stmt(sprintf("GC ALL"))
  obj <- poster(body)
  check_response(obj)
  invisible(NULL)
}

#'@title Import data.frame to BigObject
#'@references url{http://docs.bigobject.io/API/Data_Import_Service.html}
#'@param df data.frame. The data which will be imported to BigObject.
#'@param name string. The table name of the imported 
#'@param action string. Specify how to update object in BigObject. Only the first element is used. Please see details.
#'@param ip string. The ip address or domain name to the BigObject instance.
#'@param port string. The port number.
#'@param verbose logical value. Whether to print verbose message.
#'@details
#'The \code{action} parameter indicates the behavior of BigObject:
#'\itemize{
#'  \item {"create":} Creating a new table.
#'  \item {"append":} Appending data to an existed table.
#'  \item {"overwrite":} Overwrite an existed table with the same schema.
#'}
#'@importFrom httr upload_file
#'@importFrom httr GET
#'@importFrom httr PUT
#'@importFrom httr content_type
#'@export
bigobject_import <- function(df, name, action = c("create", "append", "overwrite"), ip = getOption("BIGOBJECT_IP", "127.0.0.1"), port = getOption("BIGOBJECT_PORT", "9090"), verbose = getOption("BIGOBJECT_VERBOSE", TRUE)) {
  schema <- sapply(df, .bigobject_datatype)
  stopifnot(sapply(schema, is.null) %>% sum == 0)
  importer <- .get_bigobject_poster(ip, port, path = "/import")
  importer_url <- importer("") %>% fromJSON
  m <- regmatches(importer_url$callback_url, regexec("^/import/(.*)\\?token=(.*)$", importer_url$callback_url))
  session <- m[[1]][2]
  token <- m[[1]][3]
  uploader <- .get_bigobject_poster(ip, port, path = sprintf("/import/%s?token=%s", session, token))
  write.table(df, file = tmp.file <- tempfile(), sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
  body <- list(file = upload_file(tmp.file, type = "text/csv"), misc = list(
    fields = nrow(df),
    quotes = TRUE,
    skip = 1
  ))
  uploader(body)
  planner <- .get_bigobject_getter(ip, port, path = sprintf("/import/%s?token=%s", session, token))
  plan <- planner(body = NULL)
  for(i in seq_along(plan$columns)) {
    plan$columns[[i]]$attr <- names(schema)[i]
    plan$columns[[i]]$type <- as.vector(schema[i])
  }
  plan$name <- name
  body2 <- plan %>% toJSON(auto_unbox = TRUE)
  putter <- .get_bigobject_putter(ip, port, path = sprintf("/import/%s?token=%s&action=%s", session, token, action[1]))
  put_res <- putter(body = as.character(body2), content_type("json"))
  if (put_res$status != 0) stop(put_res$err)
  retval_url <- sprintf("/import/status/%s?token=%s", session, token)
  retval_callback <- .get_bigobject_getter(ip, port, retval_url)
  duration <- 0
  while(retval_callback(NULL) != "done") {
    Sys.sleep(duration <- duration + 10)
  }
  invisible(NULL)
}
