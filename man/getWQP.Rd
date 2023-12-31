% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getWQP.R
\name{getWQP}
\alias{getWQP}
\title{Pull and pre-process Water Quality Portal data}
\usage{
getWQP(
  organization,
  characteristicName = params$params[1:11],
  startDate = "01-01-2015",
  endDate = "12-31-2022",
  multiplier = 0.5
)
}
\arguments{
\item{organization}{organization name(s), sent to dataRetrieval::readWQPdata}

\item{characteristicName}{parameter name(s), sent to dataRetrieval::readWQPdata}

\item{startDate}{start date for data in format 'mm-dd-yyyy', sent to dataRetrieval::readWQPdata}

\item{endDate}{end date for data in format 'mm-dd-yyyy', sent to dataRetrieval::readWQPdata}

\item{multiplier}{multiplier used for replacing values below detection limits (see R8WD::preProcessResults)}
}
\value{
list of dataframes
}
\description{
Pull and pre-process Water Quality Portal data
}
