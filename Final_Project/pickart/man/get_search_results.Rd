% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_search_results.R
\name{get_search_results}
\alias{get_search_results}
\title{Find artworks realted to one theme or topic}
\usage{
get_search_results(search = "Realism", famous = TRUE, onview = TRUE)
}
\arguments{
\item{search}{The theme or topic you want to search for. Default to Realism}

\item{famous}{If the artworks are famous. Default to TRUE}

\item{onview}{If the artworks are on view in the museum. Default to TRUE}
}
\value{
Dataframe
}
\description{
This function allows users give a certain theme or topic for searching, and will return the details of all artworks related to the search.
}
\examples{
get_search_results(search = "Realism", famous = TRUE, onview = TRUE)
get_search_results(search = "Impressionism")
}
\keyword{search}
