% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_trex.R
\name{read_trex}
\alias{read_trex}
\title{Read TRex Movement Tracking Data}
\usage{
read_trex(path)
}
\arguments{
\item{path}{Character string specifying the path to a TRex CSV file.
The file should contain columns for:
\itemize{
\item time
\item x and y coordinates for tracked points (e.g., x_head, y_head)
\item x and y coordinates for centroid (x_number_wcentroid_cm, y_number_wcentroid_cm)
}}
}
\value{
A data frame containing movement data with the following columns:
\itemize{
\item \code{time}: Time values from the tracking
\item \code{individual}: Factor (set to NA, as TRex tracks one individual)
\item \code{keypoint}: Factor identifying tracked points (e.g., "head", "centroid")
\item \code{x}: x-coordinates in centimeters
\item \code{y}: y-coordinates in centimeters
\item \code{confidence}: Numeric confidence values (set to NA as TRex doesn't provide these)
}
}
\description{
Reads and formats movement tracking data exported from TRex (Walter & Couzin, 2021).
TRex is a software for tracking animal movement in videos, which exports
coordinate data in CSV format. This function processes these files into
a standardized movement data format.
}
\details{
The function performs several processing steps:
\enumerate{
\item Validates the input file format (must be CSV)
\item Reads the data using vroom for efficient processing
\item Cleans column names to a consistent format
\item Restructures the data from wide to long format
\item Initializes metadata fields required for movement data
}
}
\examples{
\dontrun{
# Read a TRex CSV file
data <- read_trex("path/to/trex_export.csv")
}

}
\references{
Walter, T., & Couzin, I. D. (2021). TRex, a fast multi-animal tracking
system with markerless identification, and 2D estimation of posture and
visual fields. eLife, 10, e64000.
}
\seealso{
\itemize{
\item \code{init_metadata()} for details on metadata initialization
\item TRex software: https://trex.run
}
}
