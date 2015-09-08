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
  "DATE32" = NA,
  "DATETIME32" = "POSIXct",
  "DATETIME64" = NA
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
bigobject_sql <- function(stmt, ip = getOption("BIGOBJECT_IP", "127.0.0.1"), port = getOption("BIGOBJECT_PORT", "9090"), verbose = getOption("BIGOJBECT_VERBOSE", TRUE)) {
  if (length(stmt) == 1) .bigobject_sql(stmt, ip, port, verbose) else lapply(stmt, .bigobject_sql, ip = ip, port = port, verbose = verbose)
}

.bigobject_sql <- function(stmt, ip = getOption("BIGOBJECT_IP", "127.0.0.1"), port = getOption("BIGOBJECT_PORT", "9090"), verbose = getOption("BIGOJBECT_VERBOSE", TRUE)) {
  stopifnot(class(stmt) == "character")
  body <- lapply(stmt, function(x) list(Stmt = x, Opts = list(Handle = TRUE))) %>%
    lapply(toJSON, pretty = FALSE, auto_unbox = TRUE) %>%
    paste(collapse = "")
  url <- sprintf("%s/cmd", paste(ip, port, sep = ":"))
  handle <- POST(url, body = body) %>% 
    content()
  check_response(handle)
  handle <- handle$Content$res
  if (is.null(handle)) return(invisible(NULL))
  body2 <- list(Stmt = sprintf("hdesc %s", handle)) %>%
    toJSON(auto_unbox = TRUE)
  desc <- POST(url, body = body2) %>%
    content("text") %>%
    fromJSON
  check_response(desc)
  desc <- desc$Content$schema$attr
  body3 <- list(Stmt = sprintf("scan %s", handle)) %>%
    toJSON(auto_unbox = TRUE)
  response <- POST(url, body = body3)
  retval.bin <- content(response, "raw")
  if (verbose) cat(sprintf("Read %d bytes data from BigObject\n", length(retval.bin)))
  con <- rawConnection(retval.bin)
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
  retval.content <- retval.content[!sapply(retval.content, is.null)]
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