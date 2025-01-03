% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_coordinates_translate.R
\name{translate_coords_vector}
\alias{translate_coords_vector}
\title{Translate coordinates relative to coordinates}
\usage{
translate_coords_vector(data, to_x, to_y, to_z = NULL)
}
\arguments{
\item{data}{movement data frame with columns: time, individual, keypoint, x, y}

\item{to_x}{x coordinates; either a single value or a time-length vector}

\item{to_y}{y coordinates; either a single value or a time-length vector}

\item{to_z}{z coordinates (only if 3D); either a single value or a time-length vector}
}
\description{
Translate coordinates relative to coordinates
}
\keyword{internal}
