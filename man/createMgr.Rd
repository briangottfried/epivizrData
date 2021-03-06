% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createMgr.R
\name{createMgr}
\alias{createMgr}
\title{Create a data manager for epiviz app}
\usage{
createMgr(server = server)
}
\arguments{
\item{server}{An object of class \code{\link{EpivizServer}}}
}
\value{
An object of class \code{\link{EpivizDataMgr}}
}
\description{
Create a data manager for epiviz app
}
\examples{
server <- epivizrServer::createServer(port=7123L)
data_mgr <- epivizrData::createMgr(server)

}
