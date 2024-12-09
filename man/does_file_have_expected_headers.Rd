% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validator_files.R
\name{does_file_have_expected_headers}
\alias{does_file_have_expected_headers}
\title{Check whether file has expected headers}
\usage{
does_file_have_expected_headers(path, expected_headers = c("x", "y", "time"))
}
\arguments{
\item{path}{Path(s) to the file.}

\item{expected_headers}{Expected column name(s) to be present among the header names. Default is c("x", "y", "time").}
}
\description{
Check whether file has expected headers
}
\keyword{internal}
