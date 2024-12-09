% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_idtracker.R
\name{read_idtracker}
\alias{read_idtracker}
\title{Read idtracker.ai data}
\usage{
read_idtracker(path, path_probabilities = NULL, version = 6)
}
\arguments{
\item{path}{Path to an idtracker.ai data frame}

\item{path_probabilities}{Path to a csv file with probabilities. Only needed if you are reading csv files as they are included in h5 files.}

\item{version}{idtracker.ai version. Currently only v6 output is implemented}
}
\value{
a movement dataframe
}
\description{
Read idtracker.ai data
}
