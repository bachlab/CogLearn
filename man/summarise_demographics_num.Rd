% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_analysis_functions.R
\name{summarise_demographics_num}
\alias{summarise_demographics_num}
\title{Summarise numerical demographic information}
\usage{
summarise_demographics_num(df, DVs = "age")
}
\arguments{
\item{df}{A tibble with participant information}

\item{DVs}{A string vector of demographic variables (default "age")}
}
\value{
A summarised tibble
}
\description{
Summarise numerical demographic information
}
\examples{
d <- tibble::tibble(age = c(21:40))
summarise_demographics_num(d)

}
