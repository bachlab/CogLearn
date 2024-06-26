% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{plot_by_phase}
\alias{plot_by_phase}
\title{Draw phase-level plots}
\usage{
plot_by_phase(
  df,
  phases,
  DV,
  cs_types = c("CSp", "CSm"),
  fill_colors = c("red", "lightgreen", "blue", "yellow")
)
}
\arguments{
\item{df}{A trial results tibble}

\item{phases}{Experimental phases}

\item{DV}{Outcome variable (e.g., 'mean_dist_CS')}

\item{cs_types}{A string vector of needed CS types (e.g., 'CS1p', 'CS1m')}

\item{fill_colors}{A string vector of fill colors for different CS conditions (e.g., 'red', 'lightgreen')}
}
\description{
For each outcome variable per condition (CS+/CS-), this function will plot
the average value and its error bars phase-by-phase
}
\examples{
# plot_by_phase(inner_d, c(1,2), "mean_dist_CS", cs_types = c('CS1p', 'CS1m'), fill_colors = c('red', 'lightgreen', 'blue', 'yellow'))


}
