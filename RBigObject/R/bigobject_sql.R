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

#'@title Submit SQL Query to BigObject
#'@param stmt character. A series of SQL statement. 
#'Each element is a complete SQL statement.
#'These query will be executed from the first element to the last element.
#'@param ip string. The ip address or domain name to the BigObject instance.
#'@param port string. The port number.
#'@return A list with the following slots:
#'\itemize{
#'  \item{"Content"} The returned content.
#'  \item{"Status"}
#'  \item{"Err"}
#'}
#'@references \url{http://docs.bigobject.io/API/index.html}
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