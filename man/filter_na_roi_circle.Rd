% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_na_roi.R
\name{filter_na_roi_circle}
\alias{filter_na_roi_circle}
\title{Filter coordinates outside a circular ROI}
\usage{
filter_na_roi_circle(data, x_center, y_center, radius)
}
\arguments{
\item{data}{A data frame containing 'x' and 'y' coordinates}

\item{x_center, y_center}{Center coordinates of the circular ROI}

\item{radius}{Radius of the circular ROI}
}
\value{
A data frame with coordinates outside circular ROI set to NA
}
\description{
Helper function for filter_na_roi() that handles circular ROIs.
Sets coordinates to NA if they fall outside the specified circle.
}
\keyword{internal}
