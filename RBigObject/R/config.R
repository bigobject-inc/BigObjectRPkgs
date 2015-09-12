.ip <- "BIGOBJECT_IP"
.port <- "BIGOBJECT_PORT"
.verbose <- "BIGOBJECT_VERBOSE"

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
  options(BIGOBJECT_IP = as.character(ip)[1], BIGOBJECT_PORT = as.character(port)[1])
}

#'@title Configure verbose of RBigObject
#'@param verbose logical. Whether to print verbose message.
#'@export
bigobject_verbose <- function(verbose) {
  stopifnot(is.logical(verbose))
  options(BIGOBJECT_VERBOSE = verbose[1])
}

#'@title Get the default IP of BigObject
#'@return string. The default ip address of BigObject.
#'@export
get_ip <- function() getOption(.ip, "127.0.0.1")

#'@title Get the default port of BigObject
#'@return string. The default port of BigObject.
#'@export
get_port <- function() getOption(.port, "9090")

#'@title Get the verbose of RBigObject.
#'@return logical. Whether to print verbose message.
#'@export
get_verbose <- function() getOption(.verbose, TRUE)

