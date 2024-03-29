% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readNWM.R
\name{readNWMdata}
\alias{readNWMdata}
\title{NWM Reanalysis Extraction}
\usage{
readNWMdata(
  AOI = NULL,
  comid = NULL,
  siteID = NULL,
  startDate = NULL,
  endDate = NULL,
  tz = "UTC",
  version = 2.1,
  addObs = FALSE,
  add_nhd = FALSE
)
}
\arguments{
\item{AOI}{spatial polygon or point to extract data for}

\item{comid}{a NHD common identifier}

\item{siteID}{a USGS NWIS site identifier (eight digits)}

\item{startDate}{a start date (YYYY-MM-DD) or (YYYY-MM-DD HH)}

\item{endDate}{an end date (YYYY-MM-DD) or (YYYY-MM-DD HH)}

\item{tz}{the desired timezone of the data. Can be found with \code{OlsonNames}}

\item{version}{the NWM version to extract (current = 1.2 or 2 (default))}

\item{addObs}{should observation data be added? Only available when !is.null(siteID)}

\item{add_nhd}{should the NHD spatial features be added to the output}
}
\value{
data.frame or sf object
}
\description{
Download hourly flow values for an NHD COMID from the National 
Water Model version 1.2 or 2.0. Returned data is available between 
"1993-01-01 00" and "2017-12-31 23" but can be 
subset using a startDate and endDate.
}
\examples{
\dontrun{
readNWMdata(comid = 101)
readNWMdata(comid = 101, version = 1.2)
readNWMdata(comid = 101, tz = "US/Pacific")
}
}
