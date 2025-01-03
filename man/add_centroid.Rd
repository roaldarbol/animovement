% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_centroid.R
\name{add_centroid}
\alias{add_centroid}
\title{Add Centroid to Movement Data}
\usage{
add_centroid(
  data,
  include_keypoints = NULL,
  exclude_keypoints = NULL,
  centroid_name = "centroid"
)
}
\arguments{
\item{data}{A data frame containing movement tracking data with the following
required columns:
\itemize{
\item \code{individual}: Identifier for each tracked subject
\item \code{keypoint}: Factor specifying tracked points
\item \code{time}: Time values
\item \code{x}: x-coordinates
\item \code{y}: y-coordinates
\item \code{confidence}: Confidence values for tracked points
}}

\item{include_keypoints}{Optional character vector specifying which keypoints
to use for centroid calculation. If NULL (default), all keypoints are used
unless \code{exclude_keypoints} is specified.}

\item{exclude_keypoints}{Optional character vector specifying which keypoints
to exclude from centroid calculation. If NULL (default), no keypoints are
excluded unless \code{include_keypoints} is specified.}

\item{centroid_name}{Character string specifying the name for the centroid
keypoint (default: "centroid")}
}
\value{
A data frame with the same structure as the input, but with an
additional keypoint representing the centroid. The centroid's confidence
values are set to NA.
}
\description{
Calculates and adds a centroid point to movement tracking data. The centroid
represents the mean position of selected keypoints at each time point.
}
\details{
The function calculates the centroid as the mean x and y position of the
selected keypoints at each time point for each individual. Keypoints can be
selected either by specifying which ones to include (\code{include_keypoints}) or
which ones to exclude (\code{exclude_keypoints}). The resulting centroid is added
as a new keypoint to the data frame.
}
\examples{
\dontrun{
# Add centroid using all keypoints
add_centroid(movement_data)

# Calculate centroid using only specific keypoints
add_centroid(movement_data,
            include_keypoints = c("head", "thorax", "abdomen"))

# Calculate centroid excluding certain keypoints
add_centroid(movement_data,
            exclude_keypoints = c("antenna_left", "antenna_right"),
            centroid_name = "body_centroid")
}

}
\seealso{
\code{convert_nan_to_na()} for NaN handling in the centroid calculation
}
