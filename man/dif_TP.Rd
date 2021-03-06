% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dif_MinMax.R
\name{dif_TP}
\alias{dif_TP}
\title{Closest differences}
\usage{
dif_TP(dato1, dato2, minmax = 1)
}
\arguments{
\item{dato1}{A vector of data}

\item{dato2}{A vector of data2}

\item{minmax}{Indicator variable 1 minimum, 2 maximum}
}
\value{
Differences between turning points
}
\description{
Finds closest differences between two sets of turning points
}
\details{
From Martinez et al. (2016): "When the number of turning points in the two
 time series that are being compared is not the same,
 we recommend choosing the pairs of turning points, i.e.
 peak–peak and trough–trough, in the following way:
 take the first peak of the first time series, then
 compute the chronological difference with each one
 of the peaks of the second time series and choose
 the peak in this time series that leaves the smallest
 algebraic difference. Now, consider the second peak
 of the first time series and compute the differences
 with all the peaks of the second one but the peaks
 that precedes the one chosen in the previous step.
 Take from the second time series that for which the
 difference is the smallest. This procedure for pairing
 peaks is repeated until either all the peaks in the first
 time series have been considered or all the peaks in the
 second series have been exhausted."
}
\examples{
x <- rnorm(100)
y <- rnorm(100)
dif_TP(TP_BryBoschan(x),TP_BryBoschan(y))

}
