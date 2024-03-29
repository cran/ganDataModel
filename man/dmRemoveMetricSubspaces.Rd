% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dmBuildMetricSubspaces.R
\name{dmRemoveMetricSubspaces}
\alias{dmRemoveMetricSubspaces}
\title{Remove metric subspaces for a level}
\usage{
dmRemoveMetricSubspaces(dataModelFileName, level)
}
\arguments{
\item{dataModelFileName}{Name of data model file}

\item{level}{Level}
}
\value{
None
}
\description{
Read a data model from file,
remove metric subspaces in the data model for a level
and write it to original file.
}
\examples{
\dontrun{
dmRemoveMetricSubspaces("dm.bin", 0.7)}
}
