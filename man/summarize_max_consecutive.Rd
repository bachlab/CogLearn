% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sequence_test_functions.R
\name{summarize_max_consecutive}
\alias{summarize_max_consecutive}
\title{Summarize the maximum successive number per CS condition}
\usage{
summarize_max_consecutive(x)
}
\arguments{
\item{x}{A string vector (e.g., c('CS1', 'CS1', 'CS2'))}
}
\value{
A tibble with two columns
}
\description{
Summarize the maximum successive number per CS condition
}
\examples{
# k <- summarize_max_consecutive(x = c('CS1', 'CS3', 'CS1', 'CS1', 'CS2', 'CS2', 'CS2'))
}
