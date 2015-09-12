#'@importFrom methods setClass
#'@importFrom methods new
setClass("BigObjectDriver", representation("DBIDriver"))

# For dbDriver("BigObject")
BigObject <- function() {
  new("BigObjectDriver")
}

setMethod("dbUnloadDriver", "BigObjectDriver",
          def = function(drv, ...) invisible(TRUE),
          valueClass = "logical"
)

setMethod("dbGetInfo", "BigObjectDriver",
          def = function(dbObj, ...) {
            cat(sprintf("DBI Interface for BigObject\n"))
          }
)

setMethod("summary", "BigObjectDriver",
          def = function(object, ...) DBI::dbGetInfo(object, ...)
)

setClass("BigObjectConnection", representation("DBIConnection", "ip" = "character", "port" = "character", "results" = "environment"))

setMethod("initialize", "BigObjectConnection", function(.Object, ip, port) {
  .Object@ip <- ip
  .Object@port <- port
  .Object@results <- new.env()
  .Object
})

setMethod("dbConnect", "BigObjectDriver",
          def = function(drv, ip, port, ...) new("BigObjectConnection", as.character(ip), as.character(port)),
          valueClass = "BigObjectConnection"
)

setMethod("dbConnect", "BigObjectConnection",
          def = function(drv, ...) drv,
          valueClass = "BigObjectConnection"
)

setMethod("dbDisconnect", "BigObjectConnection",
          def = function(conn, ...) {
            handles <- DBI::dbListResults(conn)
            if (length(handles) > 0) {
              for(handle in handles) {
                .bigobject_gc(handle, conn@ip, conn@port, verbose = get_verbose())
              }
              rm(list = handles, envir = conn@results)
            }
            invisible(TRUE)
          },
          valueClass = "logical"
)

setClass("BigObjectResult", contains = c("DBIResult", "VIRTUAL"))

setRefClass("BigObjectHandleResult", contains = "BigObjectResult", slots = c(handle = "character", conn = "BigObjectConnection"), fields = c(index = "integer"))

.check_handle <- function(handle) handle@handle != ".NULL"

setMethod("initialize", "BigObjectHandleResult", function(.Object, handle, conn) {
  .Object@handle <- handle
  .Object@conn <- conn
  .Object$index <- 0L
  .Object
})

setClass("BigObjectErrorResult", contains = "BigObjectResult", slots = c(err = "character"))

setMethod("initialize", "BigObjectErrorResult", function(.Object, err) {
  .Object@err <- err
  .Object
})

setMethod("dbClearResult", "BigObjectResult",
          def = function(res, ...) {
            if (inherits(res, "BigObjectErrorResult")) return(TRUE)
            if (.check_handle(res)) .bigobject_gc(res@handle, res@conn@ip, res@conn@port, verbose = get_verbose())
            rm(list = res@handle, envir = res@conn@results)
            invisible(TRUE)
          },
          valueClass = "logical"
)

setMethod("fetch", signature(res="BigObjectResult", n="integer"),
          def = function(res, n, ...){
            if (!.check_handle(res)) return(NULL)
            start <- res$index + 1
            if (n == -1) {
              end <- -1
            } else {
              end <- res$index + n
            }
            retval <- .bigobject_sql_scan(res@handle, res@conn@ip, res@conn@port, verbose = get_verbose(), 
                                          start = start, end = end, as = "table")
            res$index <- res$index + nrow(retval)
            retval
          },
          valueClass = "data.frame"
)

setMethod("fetch", signature(res="BigObjectResult", n="numeric"),
          def = function(res, n, ...){
            DBI::fetch(res, as.integer(n))
          },
          valueClass = "data.frame"
)

setMethod("dbSendQuery",
          signature(conn = "BigObjectConnection", statement = "character"),
          def = function(conn, statement,...) {
            handle <- try(.bigobject_sql_handle(statement, conn@ip, conn@port, verbose = get_verbose()), silent = TRUE)
            if (class(handle) == "try-error") {
              new("BigObjectErrorResult", conditionMessage(attr(handle, "condition")))
            } else {
              if (is.null(handle)) handle <- ".NULL"
              (conn@results[[handle]] <- new("BigObjectHandleResult", handle, conn))
            }
          },
          valueClass = "BigObjectResult"
)

setMethod("dbGetQuery",
          signature(conn = "BigObjectConnection", statement = "character"),
          def = function(conn, statement, ...) {
            .bigobject_sql(statement, conn@ip, conn@port, verbose = get_verbose())
          }
)

setMethod("dbGetInfo", "BigObjectConnection",
          def = function(dbObj, ...) {
            sprintf("BigObject Connection. (ip: %s port: %s)\n", dbObj@ip, dbObj@port) %>% cat
          }
)

setMethod("dbListResults", "BigObjectConnection",
          def = function(conn, ...) {
            ls(conn@results)
          }
)

setMethod("summary", "BigObjectConnection",
          def = function(object, ...) DBI::dbGetInfo(object)
)

setMethod("dbListTables", "BigObjectConnection",
          def = function(conn, ...){
            poster <- .get_bigobject_poster(conn@ip, conn@port, get_verbose())
            body <- .bigobject_stmt("SHOW TABLES")
            retval <- poster(body)
            check_response(retval)
            unlist(retval$Content)
          },
          valueClass = "character"
)

setMethod("dbReadTable", signature(conn="BigObjectConnection", name="character"),
          def = function(conn, name, ...) {
            stmt <- sprintf("SELECT * FROM %s", name)
            .bigobject_sql(stmt, conn@ip, conn@port, verbose = get_verbose())
          },
          valueClass = "data.frame"
)

setMethod("dbDataType",
          signature(dbObj = "BigObjectConnection", obj = "ANY"),
          def = function(dbObj, obj, ...) {
            .bigobject_datatype(obj)
          },
          valueClass = "character"
)

#'@importFrom DBI dbExistsTable
setMethod("dbWriteTable",
          signature(conn="BigObjectConnection", name="character", value="data.frame"),
          def = function(conn, name, value, field.type, row.names = TRUE, overwrite = FALSE, append = FALSE, ..., allow.keywords = FALSE) {
            if (overwrite && append) stop("Invalid argument: overwrite and append should not be both TRUE")
            stopifnot(is.data.frame(value))
            if (row.names) {
              value <- cbind(row.names(value), value)
              colnames(value)[1] <- "row.names"
            }
            # check parameters
            if (.exist <- DBI::dbExistsTable(conn, name)) {
              if (overwrite) {
                if (!DBI::dbRemoveTable(conn, name)) {
                  warning(sprintf("Table %s could not be overwritten!", name))
                  return(FALSE)
                }
                .exist <- FALSE
              } else if (!append) {
                warning(sprintf("Table %s is existed! (use parameter `overwrite` or `append` to change the existed table)", name))
                return(FALSE)
              }
            }
            # create table if needed
            if (!.exist) {
              if (missing(field.type) || is.null(field.type)) {
                field.type <- sapply(value, .bigobject_datatype)
              }
              sql1 <- sprintf("create table %s (", name)
              sql2 <- sprintf("%s %s", names(field.type), field.type) %>% paste(collapse = " ,\n")
              sql3 <- sprintf(")")
              sql <- paste(sql1, sql2, sql3, sep = "\n")
              rs <- DBI::dbSendQuery(conn, sql)
              if (inherits(rs, "BigObjectErrorResult")) {
                warning(sprintf("Failed to create table"))
                retval <- FALSE
                attr(retval, "err") <- rs@err
                return(retval)
              }
              DBI::dbClearResult(rs)
            }
            .retval <- try(bigobject_import(value, name, action = "append", ip = conn@ip, port = conn@port, verbose = get_verbose()), silent = TRUE)
            if (class(.retval)[1] != "try-error") TRUE else {
              browser()
              FALSE
            }
          },
          valueClass = "logical"
)

setMethod("dbExistsTable",
          signature(conn="BigObjectConnection", name="character"),
          def = function(conn, name, ...){
            handle <- DBI::dbSendQuery(conn, sprintf("SELECT * FROM %s LIMIT 0", name))
            if (inherits(handle, "BigObjectErrorResult")) FALSE else {
              .bigobject_gc(handle@handle, conn@ip, conn@port, get_verbose())
              TRUE
            }
          },
          valueClass = "logical"
)

setMethod("dbRemoveTable", 
          signature(conn="BigObjectConnection", name="character"),
          def = function(conn, name, ...) {
            handle <- DBI::dbSendQuery(conn, sprintf("DROP TABLE %s", name))
            if (inherits(handle, "BigObjectErrorResult")) FALSE else {
              if (.check_handle(handle)) .bigobject_gc(handle@handle, conn@ip, conn@port, get_verbose())
              TRUE
            }
          },
          valueClass = "logical"
)

setMethod("dbListFields",
          signature(conn="BigObjectConnection", name="character"),
          def = function(conn, name, ...) {
            stmt <- sprintf("SELECT * FROM %s LIMIT 0", name)
            handle <- .bigobject_sql_handle(stmt, conn@ip, conn@port, get_verbose())
            desc <- .bigobject_sql_hdesc(handle, conn@ip, conn@port, get_verbose())
            retval <- desc$type
            names(retval) <- desc$name
            retval
          },
          valueClass = "character"
)