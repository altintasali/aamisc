% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fittedHR.R
\name{fittedHR}
\alias{fittedHR}
\title{Extracts fitted values from \code{\link{harmonic.regression}} object}
\usage{
fittedHR(hr.out.object, inputTime, tau = 24, trend.degree = 0)
}
\arguments{
\item{hr.out.object}{Output object from \code{\link{harmonic.regression}}}

\item{inputTime}{Numeric vestor for time series to be fitted by harmonic regression.}

\item{tau}{Period of harmonic regression, defaults to 24. \code{tau} should be the same value used in \code{link{harmonic.regresion}}.}

\item{trend.degree}{Degree of polynomial trend function used in \code{\link{harmonic.regression}}. \code{trend.deggree} should be the same value used in \code{link{harmonic.regresion}}.}
}
\value{
fitted values for provided time series
}
\description{
Create fitted values (y) for a sepcific time series (x). The fit obejct should be generated by \code{\link{harmonic.regression}}
}
\examples{
#A detailed example will come here
}
