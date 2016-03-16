#' Create a data manager for epiviz app
#' 
#' @param server An object of class \code{\link{EpivizServer}}
#'
createMgr <- function(server = server) {
  if (!is(server, "EpivizServer")) {
    stop("'server' must be of class 'EpivizServer', see epivizrServer package")
  }
  EpivizDataMgr$new(server=server)
}