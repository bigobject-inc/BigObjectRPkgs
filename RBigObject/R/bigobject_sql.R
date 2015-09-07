#'@export
bigobject_connection <- function(ip, port) {
  stopifnot(length(ip) == 1)
  stopifnot(length(port) == 1)
  options("BIGOBJECT_IP" = as.character(ip)[1], "BIGOBJECT_PORT" = as.character(port)[1])
}

#'@export
#'@importFrom httr POST
#'@importFrom httr content
#'@importFrom RJSONIO toJSON
#'@importFrom magrittr %>%
bigobject_sql <- function(stmt, ip = getOption("BIGOBJECT_IP", "127.0.0.1"), port = getOption("BIGOBJECT_PORT", "9090")) {
  stopifnot(class(stmt) == "character")
  content <- lapply(stmt, function(x) list(Stmt = x)) %>%
    lapply(toJSON, pretty = FALSE, collapse = "") %>%
    paste(collapse = "")
  url <- sprintf("%s/cmd", paste(ip, port, sep = ":"))
  response <- POST(url, body = content)
  content(response)
}