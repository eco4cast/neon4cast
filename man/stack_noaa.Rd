% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/noaa.R
\name{stack_noaa}
\alias{stack_noaa}
\title{Stack downloaded NOAA files}
\usage{
stack_noaa(dir = tempdir(), forecast_date = NULL)
}
\arguments{
\item{dir}{storage location.  Use tempdir unless you want to keep this
data around on your computer, in which case, \code{neonstore::neon_dir()} might
be a convenient choice.}

\item{forecast_date}{Include only forecasts issued on this date}
}
\description{
Stack downloaded NOAA files
}
\examples{
stack_noaa()
}
